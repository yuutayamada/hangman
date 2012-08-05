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
    map)
  "Keymap used in hangman mode.")

(defvar hm-current-word-alist nil)

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
(defvar hm-current-word nil
  "This is not the word the user must guess represented as a vector.")

(defvar hm-current-guess-string nil
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
  "Initialize this buffer w/ a new word."
  (interactive)
  (if (string-match "\.yml$" hm-dictionary-file)
      (hm-initialize-for-logaling)
    (set (make-local-variable 'hm-current-word) (hm-fetch-random-word)))
  (set (make-local-variable 'hm-current-guess-string)
       (hm-make-guess-string hm-current-word))
  (set (make-local-variable 'hm-num-failed-guesses) 0)
  (set (make-local-variable 'hm-wrong-guess-string) "")
  (setq buffer-read-only t)
  (hm-refresh)
  t)

(defun hm-initialize-for-logaling ()
  (hm-setup-random-word-for-logaling)
  (set (make-local-variable 'hm-current-word) (hm-extract :source)))

(defun hm-count-under-score ()
  (loop with tokens = (string-to-list (split-string (hm-extract :source) ""))
        for token in tokens
        for count = 0 then count
        if (string-match "_" token)
        do (setq count (+ count 1))
        finally return count))

(defun hm-self-guess-char ()
  "Guess the character that was pressed."
  (interactive)
  (hm-check-each-character last-input-event)
  (hm-refresh)
  (hm-win t)
  (when (hm-win-p)
    (aset hm-win-statistics 0 (1+ (aref hm-win-statistics 0)))
    (hm-query-playng-again 'win)))

(defun hm-check-each-character (input)
  (hm-already-guessed input)
  (loop with case-fold-search = nil
        for i from 0 upto (1- (length hm-current-word))
        for character = input
        for found = 0 then found
        if (char-equal (aref hm-current-word i) character) do
        (setq found (1+ found))
        (hm-found-guess-string i character)
        finally (hm-response found character)))

(defun hm-response (found c)
  (if (/= found 0)
      (message "Found %d occurances of %c" found c)
    (message "No uccurances of %c" c)
    (setq hm-num-failed-guesses (1+ hm-num-failed-guesses)
          hm-wrong-guess-string (concat hm-wrong-guess-string " "
                                        (char-to-string (upcase c))))))

(defun hm-found-guess-string (i character)
  (aset hm-current-guess-string (* i 2) character) ;upcase
  (hm-fontify-char hm-current-guess-string (* 2 i)
                   (if (facep 'font-lock-function-name-face)
                       'font-lock-function-name-face
                     'bold)))

(defun hm-already-guessed (c)
  "Signal an error if character C has already been played."
  (let ((case-fold-search t) (re (char-to-string c)))
    (if (or (string-match re hm-wrong-guess-string)
            (string-match re hm-current-guess-string))
        (error "You have already guessed %c" c))))

(defun hm-win (&optional dostats)
  "Do the right thing if the game has been won.
Optional argument DOSTATS will update the statistics if set."
  (let ((case-fold-search nil))
    (if (string-match "[a-z_] " hm-current-guess-string)
        (when (= hm-num-failed-guesses (1- (length hm-vector)))
          (if dostats
              (aset hm-win-statistics 1 (1+ (aref hm-win-statistics 1))))
          (setq hm-current-guess-string
                (hm-make-guess-string hm-current-word
                                      hm-current-guess-string))
          (hm-refresh)
          (hm-query-playng-again 'lost))
      (hm-refresh)
      t)))

(defun hm-win-p ()
  (equal (- (length hm-current-guess-string) (hm-count-under-score))
         (length (replace-regexp-in-string "_" "" hm-current-guess-string))))

(defun hm-query-playng-again (win-or-lost)
  (if (y-or-n-p (concat "You " (symbol-name win-or-lost) "! Play again?"))
      (hm-initialize)))

;;; Rendering
;;
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
    (hm-insert-currnet-guess-string)
    (hm-insert-target-word-for-logaling)))

(defun hm-insert-currnet-guess-string ()
  (forward-line 20)
  (end-of-line)
  (insert "\n   " hm-current-guess-string))

(defun hm-insert-target-word-for-logaling ()
  (when (string-match "\.yml$" hm-dictionary-file)
    (forward-line 2)
    (end-of-line)
    (insert (format "
 ============= H i n t ================
 == meaning %s ==
 == string number %s ==
 ============= H i n t ================"
                    (hm-extract :target)
                    (- (length hm-current-word) (hm-count-under-score))))))

;;; Text Properties
(defun hm-fontify-char (string idx face)
  "Fontify one character in STRING at position IDX with FACE."
  (if (fboundp 'put-text-property)
      (put-text-property  idx (1+ idx) 'face face string)))

;;; Word Retrieval
(defun hm-make-guess-string (string &optional finish)
  "Return a string representing a new guess string based on STRING.
Optional argument FINISH non-nil means to not replace characters with _."
  (let* ((ns "") (i 0))
    (while (< i (length string))
      (cond ((and (>= (aref string i) ?A) (<= (aref string i) ?z))
             (if finish
                 (if (char-equal (aref finish (* 2 i)) ?_)
                     (progn
                       (aset finish (* 2 i) (aref string i))
                       (hm-fontify-char finish (* 2 i)
                                        (if (facep 'font-lock-comment-face)
                                            'font-lock-comment-face
                                          'underline))))
               (setq ns (concat ns "_ "))))
            (t
             (setq ns (concat ns (aref string i) " "))))
      (setq i (1+ i)))
    (or finish ns)))

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

(defun hm-setup-random-word-for-logaling ()
  (let* ((source-regexp "- source_term: \\([a-zA-Z ]+\\)?\n")
         (target-regexp "  target_term: \\(\\w+\\)?\n")
         (make-alist (lambda ()
                       (setq hm-current-word-alist
                             (list (cons 'source (match-string 1))
                                   (cons 'target (match-string 2)))))))
    (with-current-buffer (find-file-noselect hm-dictionary-file)
      (goto-char (random (point-max)))
      (if (re-search-forward (concat source-regexp target-regexp) nil t)
          (funcall make-alist)
        (re-search-backward (concat source-regexp target-regexp) nil t)
        (funcall make-alist)))))

(provide 'hangman)
;;; hangman.el ends here
