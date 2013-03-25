;;; -*- coding: utf-8 mode: emacs-lisp -*-
;;; hangman.el --- Hangman game

;;; Copyright (C) 1997  Eric M. Ludlam
;;                2012  Yuta Yamada <cokesboy"at"gmail.com>
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: games
;; X-RCS: $Id: hangman.el,v 1.3 1997/09/12 22:07:31 zappo Exp $
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;
;; Future versions of checkdoc will appear at:
;;   ftp://ftp.ultranet.com/pub/zappo/hangman-*.el
;;

;;; Commentary:
;;
;; Allows user to play hangman iff /usr/dict/words or compatible file
;; is installed.

;;; Usage
;; (add-to-list 'load-path "path/to/hangman")
;; (require 'hangman)
;; specify path for dictionary-file, by default "/usr/dict/words"
;; for example..
;; (setq hm-dictionary-file "~/.logaling/glossary/my-dictionary.en.ja.yml")

;;; History:
;;; 0.1  Initial revision
;;; 2012/8~ Improve (Yuta Yamada)

;;; Code:

(eval-when-compile (require 'cl))

(require 'json)

(defvar hm-user-proper-nouns-flag t
  "*Non-nil means to allow proper nouns as potential words.")

(defvar hm-shortest-word 4
  "*The sortest word allowed for a potential word.")

(defvar hm-dictionary-file "/usr/dict/words"
  "The file where a list of words is stored.")

(defvar hm-hooks nil
  "Hooks run when entering hangman mode.")

(defvar hm-map
  (let* ((map (make-sparse-keymap)))
    (loop for i from ?a to ?z do
          (define-key map (char-to-string i) 'hm-self-guess-char))
    (define-key map " "    'hm-give-up)
    (define-key map "\C-j" 'hm-give-up)
    (define-key map "\C-k" 'hm-skip) ; jump
    (define-key map "\C-q" 'hm-quit)
    (define-key map "T"    'hm-toggle-spelling-practice-mode)
    map)
  "Keymap used in hangman mode.")

(defvar hm-current-word-alist nil)

(defvar hm-mistaken-words '())

(defvar hm-current-fetch-process :random)

(defvar hm-ignoring-character "[_ ']")

(defvar hm-review nil)

(defvar hm-ignore-single-word t)

(defvar hm-next-word-index 0)

(defvar hm-ignoring-regexp "[' --!?,.~0-9$%&;:/]")

(defvar hm-history '())

;;; Game Mode
(defun hm-mode ()
  "Major mode for playing the hangman game against emacs."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Hangman*"))
  (kill-all-local-variables)
  (setq major-mode 'hm-mode
        mode-name "HangMan")
  (use-local-map hm-map)
  (hm-initialize)
  (run-hooks 'hm-hooks))

(defun hangman ()
  (interactive)
  (hm-mode))

;;; Game playing functions and variables
(defvar hm-original-current-word nil
  "This is not the word the user must guess represented as a vector.")

(defvar hm-displaying-guess-string nil
  "The string representing what the user has guessed correctly so far.")

(defvar hm-wrong-guess-string nil
  "The letters guessed so far.")

(defvar hm-num-failed-guesses nil
  "The number of errors so far.")

(defvar hm-win-statistics [ 0 0 ]
  "The number of won and lost games since emacs bootup.")

(defvar hm-vector
  [ "+---------+\n|         |\n|           \n|          \n|           \n\
|          \n|           \n|            \n|"
    "+---------+\n|         |\n|        ( )\n|         +\n|           \n\
|          \n|           \n|            \n|"
    "+---------+\n|         |\n|        ( )\n|         +\n|         | \n\
|         |\n|           \n|            \n|"
    "+---------+\n|         |\n|        ( )\n|         +\n|         |\\\n\
|         |\n|           \n|            \n|"
    "+---------+\n|         |\n|        ( )\n|         +\n|        /|\\\n\
|         |\n|           \n|            \n|"
    "+---------+\n|         |\n|        ( )\n|         +\n|        /|\\\n\
|         |\n|        /  \n|       /    \n|"
    "+---------+\n|         |\n|        ( )\n|         +\n|        /|\\\n\
|         |\n|        / \\\n|       /   \\\n|"]
  "Vector of hangman states.")

(defvar hm-correct-answer-list '())

(defvar hm-use-other-format nil)

(defvar hm-mistaken-word-memory-limit 3)

(defvar hm-use-spelling-practice nil)

(defvar hm-use-sparta-mode t)

(defmacro hm-with-writable (&rest forms)
  "Allow the buffer to be writable and evaluate FORMS.
Turn read only back on when done."
  (list 'let '((hm-with-writable-buff (current-buffer)))
        '(setq buffer-read-only nil)
        (cons 'progn forms)
        '(with-current-buffer hm-with-writable-buff
           (setq buffer-read-only t))))

(put 'hm-with-writable 'lisp-indent-function 0)

(defun hm-initialize ()
  "Initialize *Hangman* buffer and setup new word."
  (interactive)
  (unless hm-mistaken-words
    (hm-review-mode :off))
  (hm-fetch)
  (hm-set-spelling-check-flag)
  (setq hm-displaying-guess-string (hm-make-guess-string)
        hm-num-failed-guesses 0
        hm-wrong-guess-string ""
        buffer-read-only t
        hm-next-word-index 0)
  (hm-refresh)
  t)

(defun hm-fetch ()
  (let* ((mistaken-length (1- (length hm-mistaken-words)))
         (mistaken-word (when (hm-review-mode-p)
                          (hm-review-mode :on)
                          (nth (random mistaken-length) hm-mistaken-words))))
    (case hm-current-fetch-process
      (:random
       (if (string-match "en\.ja\.yml$" hm-dictionary-file)
           (hm-initialize-for-logaling mistaken-word)
         (setq hm-original-current-word
               (or mistaken-word (hm-fetch-random-word)))))
      (:practice
       (hm-fetch/practice-word mistaken-word)))
    (hm-save-history)))

(defun hm-fetch/practice-word (&optional mistaken-word)
  (lexical-let
      ((search-word (or mistaken-word (hm-fetch-english-word))))
    (setq hm-current-word-alist (hm-fetch-from-yaml search-word)
          hm-original-current-word (hm-extract :source))))

(defun hm-nthcar (n list)
  (reverse (nthcdr (- (length list) n) (reverse list))))

(defun hm-save-history ()
  (if (< 10 (length hm-history))
      (setq hm-history
            (hm-nthcar 9 hm-history)))
  (push hm-current-word-alist hm-history))

(defun hm-review-mode (&optional force)
  (let* ((choice (or force (if hm-review :off :on))))
    (case choice
      (:on  (setq hm-review t))
      (:off (setq hm-review nil)))))

(defun hm-set-spelling-check-flag ()
  (when hm-use-sparta-mode
    (if (loop for word in hm-mistaken-words
              if (equal word hm-original-current-word)
              do (return t)
              finally return nil)
        (setq hm-use-spelling-practice t)
      (setq hm-use-spelling-practice nil))))

(defun hm-review-mode-p ()
  (or (< hm-mistaken-word-memory-limit (length hm-mistaken-words))
      hm-review))

(defun hm-initialize-for-logaling (&optional mistaken-word)
  (hm-setup-word-for-logaling mistaken-word)
  (setq hm-original-current-word (hm-extract :source)))

(defun hm-count-ignoring-character ()
  (loop with tokens = (string-to-list (split-string (hm-extract :source) ""))
        for token in tokens
        for count = 0 then count
        if (string-match hm-ignoring-character token)
        do (setq count (+ count 1))
        finally return count))

(defun hm-count-next-index ()
  (loop with length = (- (length hm-displaying-guess-string) 2)
        for i from hm-next-word-index upto length
        for char = (hm-nth-string i hm-displaying-guess-string)
        if (string-match hm-ignoring-regexp char)
        do (setq hm-next-word-index (1+ i))
        else do (return)))

(defun hm-self-guess-char ()
  "Guess the character that was pressed."
  (interactive)
  (hm-check-each-character (char-to-string last-input-event))
  (hm-count-next-index)
  (hm-refresh)
  (hm-judgment))

(defun hm-set-stat (state)
  (interactive)
  (let ((source (hm-extract :source)))
    (case state
      (:win
       (add-to-list 'hm-correct-answer-list source)
       (hm-delete-mistaken-word source)
       (aset hm-win-statistics 0 (1+ (aref hm-win-statistics 0))))
      (:lose
       (add-to-list 'hm-mistaken-words source)
       (aset hm-win-statistics 1 (1+ (aref hm-win-statistics 1)))
       (setq hm-displaying-guess-string (hm-make-guess-string t))))
    (hm-initialize)))

(defun hm-corrected-answer-p ()
  (loop with current-answer = (hm-extract :source)
        for answer in hm-correct-answer-list
        if (equal answer current-answer)
        do (return t)
        finally return nil))

(defun hm-check-each-character (input)
  (unless (and (not hm-use-spelling-practice)
               (hm-already-guessed input))
    (loop with case-fold-search = nil
          with length = (1- (length hm-original-current-word))
          with initial = (if hm-use-spelling-practice
                             (min hm-next-word-index (1- length))
                           0)
          for i from initial upto (if hm-use-spelling-practice
                                      hm-next-word-index
                                    length)
          for found = 0 then found
          for token = (hm-nth-string i hm-original-current-word)
          if (equal (downcase token) input) do
          (setq found (1+ found))
          (if (not hm-use-spelling-practice)
              (hm-replace-guess-string i)
            (when (not
                   (equal (hm-nth-string i hm-displaying-guess-string) token))
              (hm-replace-guess-string i)
              (hm-response found input)
              (setq hm-next-word-index (1+ hm-next-word-index))
              (hm-refresh)
              (return)))
          finally (hm-response found input))))

(defun hm-toggle-spelling-practice-mode (&optional force)
  (interactive)
  (setq hm-use-spelling-practice
        (if force
            force
          (if hm-use-spelling-practice nil t)))
  (let* ((on-or-off (if hm-use-spelling-practice "on" "off")))
    (minibuffer-message
     (format "Toggle spelling practice mode: %s" on-or-off))))

(defun hm-response (found string)
  (if (or (/= found 0)
          (hm-already-guessed string t))
      (message "Found %d occurances of %s" found string)
    (message "No uccurances of %s" string)
    (setq hm-num-failed-guesses (1+ hm-num-failed-guesses)
          hm-wrong-guess-string (concat hm-wrong-guess-string " "
                                        (upcase string)))))

(defun hm-replace-guess-string (i)
  (aset hm-displaying-guess-string i
        (string-to-char (hm-nth-string i hm-original-current-word)))
  (hm-fontify-char hm-displaying-guess-string i
                   'font-lock-function-name-face))

(defun hm-already-guessed (str &optional no-message)
  "Signal an error if character C has already been played."
  (let ((case-fold-search t))
    (when (or (string-match str hm-wrong-guess-string)
              (string-match str hm-displaying-guess-string))
      (unless no-message
        (minibuffer-message "You have already guessed %s" str))
      t)))

(defun hm-judgment ()
  (let ((case-fold-search nil))
    (if (string-match "[a-z_] " hm-displaying-guess-string)
        (if (= (1- (length hm-vector)) hm-num-failed-guesses)
            (hm-set-stat :lose))
      (hm-refresh))
    (when (hm-win-p)
      (hm-set-stat :win))))

(defun hm-give-up ()
  (interactive)
  (hm-set-stat :lose))

(defun hm-skip ()
  (interactive)
  (hm-set-stat :win)
  (hm-initialize))

(defun hm-delete-mistaken-word (corrected-word)
  (loop with updated-mistaken-words = '()
        for mistaken-word in hm-mistaken-words
        unless (equal corrected-word mistaken-word)
        collect mistaken-word into updated-mistaken-words
        finally (setq hm-mistaken-words updated-mistaken-words)))

(defun hm-win-p ()
  (not (string-match "_" hm-displaying-guess-string)))

(defun hm-query-playng-again (win-or-lost)
  (if (y-or-n-p (concat "You " (symbol-name win-or-lost) "! Play again?"))
      (hm-initialize)
    (hm-refresh t)))

;;; Rendering
(defun hm-refresh (&optional game-over)
  "Refresh the hangman buffer w/ new images."
  (hm-with-writable
    (erase-buffer)
    (insert (aref hm-vector hm-num-failed-guesses))
    (goto-char (point-min))
    (loop for line from 0 upto (window-height) do
          (case line
            (1 (insert "         Failed Letters: " hm-wrong-guess-string))
            (3 (insert (format "         Won: %d    Lost: %d"
                               (aref hm-win-statistics 0)
                               (aref hm-win-statistics 1))))
            (9 (insert "\n              " hm-displaying-guess-string "\n")))
          (when (string-match "en\.ja\.yml$" hm-dictionary-file)
            (case line
              (5 (insert (format "         Spelling-practice-mode: %s"
                                 (if hm-use-spelling-practice "on" "off"))))
              (6 (insert (format "        Review-mode: %s"
                                 (if hm-review "on" "off"))))
              (7 (insert (format "       Mistaken: %i"
                                 (length hm-mistaken-words))))
              (10 (insert (format "\n     Meaning: %s" (hm-extract :target))))
              (11 (hm-insert-prewords))))
          (forward-line 1)
          (end-of-line))
    (when game-over
      (hm-game-over))))

(defun hm-insert-prewords ()
  (let* ((pre-word-pair (cadr hm-history))
         (source (cdar  pre-word-pair))
         (target (cdadr pre-word-pair)))
    (insert (format "\n=======================================\n"))
    (insert (format "          %s \n          %s" source target))))


(defun hm-game-over ()
  (loop with game-over = "GAME OVER"
        for i from 0 upto (length game-over)
        for char = (hm-nth-string i game-over) do
        (animate-string char 12 (+ i (- (/ (window-width) 2) 15))))
  (animate-string "Until next time!" 13 (- (/ (window-width) 2) 15)))

(defun hm-nth-string (n string)
  (nth n (hm-split-string string)))

(defun hm-split-string (string)
  (hm-delete-empty-string
   (split-string string "")))

(defun hm-delete-empty-string (string)
  (loop with result = '()
        for token in string
        if (string< "" token)
        collect token into result
        finally return result))

;;; Text Properties
(defun hm-fontify-char (string idx face)
  "Fontify one character in STRING at position IDX with FACE."
  (if (fboundp 'put-text-property)
      (put-text-property  idx (1+ idx) 'face (hm-justify-face face) string)))

(defun hm-justify-face (face)
  (if (facep face)
      face
    (case face
      ('font-lock-comment-face 'underline)
      ('font-lock-function-name-face 'bold))))

;;; Word Retrieval
(defun hm-make-guess-string (&optional finish?)
  "Return a string representing a new guess string based on STRING.
Optional argument FINISH non-nil means to not replace characters with _."
  (loop with new-string = ""
        with finished-word   = hm-displaying-guess-string
        with unfinished-word = hm-original-current-word
        with allowing-regexp = "[a-zA-Z]"
        for i from 0 upto (1- (length unfinished-word))
        for character = (hm-nth-string i unfinished-word)
        if (and (string-match allowing-regexp character)
                finish?)
        do (hm-coloring-to-unifinished-word i)
        else if (string-match hm-ignoring-regexp character)
        do      (setq new-string (concat new-string character))
        else do (setq new-string (concat new-string "_"))
        finally return (if finish? finished-word new-string)))

(defun hm-coloring-to-unifinished-word (i)
  (let* ((finished-word   hm-displaying-guess-string)
         (unfinished-word hm-original-current-word))
    (when (char-equal (aref finished-word i) ?_)
      (aset finished-word i (aref unfinished-word i))
      (hm-fontify-char finished-word i 'font-lock-comment-face))))

(defun hm-fetch-random-word ()
  "Return a random word that will match the options applied by the user."
  (let ((word (hm-fetch-one-random-word))
        (case-fold-search nil))
    (while (or (string-match "^[A-Z]" word)
               (< (length word) hm-shortest-word))
      (setq word (hm-fetch-one-random-word)))
    word))

(defun hm-fetch-one-random-word ()
  "Return a word from the system dictionary."
  (with-current-buffer (find-file-noselect hm-dictionary-file)
    (goto-char (random (point-max)))
    (beginning-of-line)
    (prog1
        (buffer-substring-no-properties (point) (point-at-eol))
      (goto-char (point-min)))))

(defun hm-extract (&optional choice)
  (case choice
    (:source (assoc-default 'source hm-current-word-alist))
    (:target (assoc-default 'target hm-current-word-alist))
    (t hm-current-word-alist)))

(defun hm-setup-word-for-logaling (&optional mistaken-word)
  (let* ((source-regexp "- source_term: \\(.+\\)?\n")
         (target-regexp "  target_term: \\(.+\\)?\n")
         (make-alist (lambda ()
                       (setq hm-current-word-alist
                             (list (cons 'source (match-string 1))
                                   (cons 'target (match-string 2)))))))
    (with-current-buffer (find-file-noselect hm-dictionary-file)
      (if mistaken-word
          (setq hm-current-word-alist (hm-fetch-from-yaml mistaken-word))
        ;; general
        (goto-char (random (point-max)))
        (if (re-search-forward (concat source-regexp target-regexp) nil t)
            (funcall make-alist)
          (re-search-backward (concat source-regexp target-regexp) nil t)
          (funcall make-alist))))
    (if (or (hm-corrected-answer-p)
            (hm-single-word-p))
        (hm-setup-word-for-logaling))))

(defun hm-single-word-p ()
  (if hm-ignore-single-word
      (not (string-match " " (hm-extract :source)))))

(defun hm-quit ()
  (interactive)
  (kill-buffer "*Hangman*"))

(defvar hm-practice-word "")
(defvar hm/practice-english-current-line 0)
(defvar hm/english-question-collections '()
  "Set up list of string type file-name for challenge")

(defun hm-fetch-english-word ()
  (interactive)
  (let* ((next-line
          (lambda ()
            (point-min)
            (beginning-of-line)
            (line-move (setq hm/practice-english-current-line
                             (1+ hm/practice-english-current-line)))))
         line)
    (with-temp-buffer
      (insert-file-contents
       (file-truename
        (car hm/english-question-collections)))
      (funcall next-line)
      (beginning-of-line)
      (while (looking-at "^* .+")
        (funcall next-line)
        (beginning-of-line))
      (setq line (substring-no-properties (thing-at-point 'line)))
      (string-match " +\\(.+\\)\n" line)
      (setq hm-practice-word (match-string 1 line)))))

(defun hm-fetch-from-yaml (&optional word-or-index)
  (let* ((fetch-from-yaml
          (lambda ()
            (unless word-or-index (hm-fetch-english-word))
            (unless (null hm-practice-word)
              (hm-extract-word-from (file-truename hm-dictionary-file)
                                    (or word-or-index hm-practice-word)))))
         (format
          (lambda (json)
            (loop for list across (json-read-from-string json)
                  for note   = (cdr (nth 0 list))
                  for target = (cdr (nth 1 list))
                  for source = (cdr (nth 2 list))
                  finally return (list (cons 'source source)
                                       (cons 'target target))))))
    (funcall format
             (condition-case error
                 (funcall fetch-from-yaml)
               (error "perhaps You need 'gem install json'")))))

(defun hm-do-ruby (body match)
  (shell-command-to-string (concat "ruby -e " "'" body "' \"" match "\"")))

(defun hm-extract-word-from (file arg)
  (hm-do-ruby
   (concat
      "
require \"rubygems\"
require \"yaml\"
require \"json/pure\"

arg = ARGV[0]
index = false
if arg.to_s =~ /[0-9]+/ && !(arg.to_s =~ /a-zA-Z/)
  index = arg.to_i
else
  match = arg.to_s
end

file = \"" file "\"
if File.exist?(file)
  open(file, \"r\") do |contents|
    yaml = YAML.load(contents.read)
    unless index
      yaml.each do |json|
        if json[\"source_term\"] =~ /#{match}/
          output = JSON.pretty_generate(json)
          puts \"[\" + output + \"]\"
        end
      end
    else
      puts \"[\" + JSON.pretty_generate(yaml[index.to_i]) + \"]\"
    end
  end
else
  puts \"file not found\"
end") arg))

(provide 'hangman)
;;; hangman.el ends here
