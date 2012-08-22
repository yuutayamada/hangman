;;; hangman.el --- Hangman game

;;; Copyright (C) 1997  Eric M. Ludlam
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

(eval-when-compile
  (require 'cl))

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
    (define-key map " " 'hm-lose)
    (define-key map "\C-j" 'hm-lose)
    map)
  "Keymap used in hangman mode.")

(defvar hm-current-word-alist nil)

(defvar hm-mistaken-words '())

(defvar hm-current-fetch-process :random)

(defvar hm-ignoring-character "[_ ']")

(defvar hm-review nil)

;;; Game Mode
(defalias 'hangman 'hm-mode)

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

(defvar hm-mistaken-word-memory-limit 5)

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
  (hm-fetch (hm-review-mode-p))
  (setq hm-displaying-guess-string (hm-make-guess-string)
        hm-num-failed-guesses 0
        hm-wrong-guess-string ""
        buffer-read-only t)
  (hm-refresh)
  t)

(defun hm-fetch (review)
  (let* ((mistaken-length (1- (length hm-mistaken-words)))
         (mistaken-word (when review
                          (setq hm-review t)
                          (nth (random mistaken-length) hm-mistaken-words))))
    (case hm-current-fetch-process
      (:random
       (if (string-match "en\.ja\.yml$" hm-dictionary-file)
           (hm-initialize-for-logaling mistaken-word)
         (setq hm-original-current-word (or mistaken-word (hm-fetch-random-word))))))))

(defun hm-review-mode-p ()
  (or (< hm-mistaken-word-memory-limit (1- (length hm-mistaken-words)))
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

(defun hm-self-guess-char ()
  "Guess the character that was pressed."
  (interactive)
  (hm-check-each-character (char-to-string last-input-event))
  (hm-refresh)
  (hm-judgment)
  (when (hm-win-p)
    (add-to-list 'hm-correct-answer-list (hm-extract :source))
    (hm-delete-mistaken-word (hm-extract :source))
    (aset hm-win-statistics 0 (1+ (aref hm-win-statistics 0)))
    (hm-query-playng-again 'win)))

(defun hm-corrected-answer-p ()
  (unless hm-review
    (loop with current-answer = (hm-extract :source)
        for answer in hm-correct-answer-list
        if (equal answer current-answer)
        do (return t)
        finally return nil)))

(defun hm-check-each-character (input)
  (unless (hm-already-guessed input)
    (loop with case-fold-search = nil
          for i from 0 upto (1- (length hm-original-current-word))
          for found = 0 then found
          if (equal (hm-nth-string i hm-original-current-word) input) do
          (setq found (1+ found))
          (hm-found-guess-string i (string-to-char input))
          finally (hm-response found input))))

(defun hm-response (found string)
  (if (/= found 0)
      (message "Found %d occurances of %s" found string)
    (message "No uccurances of %s" string)
    (setq hm-num-failed-guesses (1+ hm-num-failed-guesses)
          hm-wrong-guess-string (concat hm-wrong-guess-string " "
                                        (upcase string)))))

(defun hm-found-guess-string (i character)
  (aset hm-displaying-guess-string (* i 2) character) ;upcase
  (hm-fontify-char hm-displaying-guess-string (* 2 i)
                   'font-lock-function-name-face))

(defun hm-already-guessed (c)
  "Signal an error if character C has already been played."
  (let ((case-fold-search t) (re c))
    (when (or (string-match re hm-wrong-guess-string)
              (string-match re hm-displaying-guess-string))
      (minibuffer-message "You have already guessed %s" c)
      t)))

(defun hm-judgment ()
  (let ((case-fold-search nil))
    (if (string-match "[a-z_] " hm-displaying-guess-string)
        (when (= hm-num-failed-guesses (1- (length hm-vector)))
          (hm-lose))
      (hm-refresh)
      t)))

(defun hm-lose ()
  (interactive)
  (add-to-list 'hm-mistaken-words (hm-extract :source))
  ;; Lose count
  (aset hm-win-statistics 1 (1+ (aref hm-win-statistics 1)))
  (setq hm-displaying-guess-string (hm-make-guess-string t))
  (hm-refresh)
  (hm-query-playng-again 'lost))

(defun hm-delete-mistaken-word (corrected-word)
  (loop with updated-mistaken-words = '()
        for mistaken-word in hm-mistaken-words
        unless (equal corrected-word mistaken-word)
        collect mistaken-word into updated-mistaken-words
        finally (setq hm-mistaken-words updated-mistaken-words)))

(defun hm-win-p ()
  (equal (- (length hm-displaying-guess-string)
            (hm-count-ignoring-character))
         (length (replace-regexp-in-string "_" "" hm-displaying-guess-string))))

(defun hm-query-playng-again (win-or-lost)
  (if (y-or-n-p (concat "You " (symbol-name win-or-lost) "! Play again?"))
      (hm-initialize)))

;;; Rendering
(defun hm-refresh ()
  "Refresh the hangman buffer w/ new images."
  (hm-with-writable
    (erase-buffer)
    (insert (aref hm-vector hm-num-failed-guesses))
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (insert "         Failed Letters: " hm-wrong-guess-string)
    (forward-line 2)
    (end-of-line)
    (insert (format "         Games won: %d    Games Lost: %d"
                    (aref hm-win-statistics 0) (aref hm-win-statistics 1)))
    (hm-insert-target-word-for-logaling)
    (hm-insert-currnet-guess-string)))

(defun hm-insert-currnet-guess-string ()
  (forward-line 20)
  (end-of-line)
  (insert "\n              " (hm-convert hm-displaying-guess-string) "\n"))

(defun hm-convert (guess-string)
  (if hm-use-other-format
      (loop with source-tokens = (hm-split-string (hm-extract :source))
            with result = '()
            for num from 0 upto (1- (length (hm-extract :source)))
            for current-character = (nth num source-tokens)
            for guess = (nth num
                             (hm-split-string
                              (replace-regexp-in-string " " "" guess-string)))
            if (string= "_" current-character)
            collect " " into result
            else collect guess into result
            finally return (mapconcat 'identity result ""))
    guess-string))

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

(defun hm-insert-target-word-for-logaling ()
  (when (string-match "en\.ja\.yml$" hm-dictionary-file)
    (forward-line 2)
    (end-of-line)
    (insert (format "         Meaning: %s" (hm-extract :target)))
    (forward-line 1)
    (end-of-line)
    (insert (format "        Review-mode: %s" (if hm-review "on" "off")))
    (forward-line 1)
    (end-of-line)
    (insert (format "                    Mistaken: %i \n"
                    (length hm-mistaken-words)))))

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
        for i from 0 upto (1- (length unfinished-word))
        if (string-match "[a-zA-Z_]" (hm-nth-string i unfinished-word))
        do (if finish?
               (hm-coloring-to-unifinished-word i)
             (setq new-string (concat new-string "_ ")))
        finally return (if finish? finished-word new-string)))

(defun hm-coloring-to-unifinished-word (i)
  (let* ((finished-word   hm-displaying-guess-string)
         (unfinished-word hm-original-current-word))
    (when (char-equal (aref finished-word (* 2 i)) ?_)
      (aset finished-word (* 2 i) (aref unfinished-word i))
      (hm-fontify-char finished-word (* 2 i) 'font-lock-comment-face))))

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
        (buffer-substring-no-properties (point) (progn (end-of-line) (point)))
      (goto-char (point-min)))))

(defun hm-extract (&optional choice)
  (case choice
    (:source
     (replace-regexp-in-string " " "_"
                               (downcase
                                (assoc-default 'source hm-current-word-alist))))
    (:target (assoc-default 'target hm-current-word-alist))
    (t hm-current-word-alist)))

(defun hm-setup-word-for-logaling (&optional mistaken-word)
  (let* ((source-regexp "- source_term: \\([a-zA-Z ]+\\)?\n")
         (target-regexp "  target_term: \\(\\w+\\)?\n")
         (make-alist (lambda ()
                       (setq hm-current-word-alist
                             (list (cons 'source (match-string 1))
                                   (cons 'target (match-string 2)))))))
    (with-current-buffer (find-file-noselect hm-dictionary-file)
      (if (not mistaken-word)
          (goto-char (random (point-max)))
        (goto-char (point-min))
        (re-search-forward (replace-regexp-in-string "_" " " mistaken-word))
        (goto-char (point-at-bol))
        (goto-char (1- (point))))
      (if (re-search-forward (concat source-regexp target-regexp) nil t)
          (funcall make-alist)
        (re-search-backward (concat source-regexp target-regexp) nil t)
        (funcall make-alist)))
    (if (hm-corrected-answer-p)
        (hm-setup-word-for-logaling))))

(provide 'hangman)
;;; hangman.el ends here
