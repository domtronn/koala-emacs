;;; koala.el --- An Emacs plugin for `Koala.js' for live scratchpad
;;; javascript evaluation

;; Copyright (C) 2018  Dom Charlesworth <dgc336@gmail.com>

;; Author: Dom Charlesworth <dgc336@gmail.com>
;; Keywords: convenience, tools, docs, help
;; Created: 24 Sep 2018

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'async)
(require 'dash)
(require 's)

;;; Custom Variables
(defgroup koala nil
  "Manage different aspects of `koala.el'."
  :prefix "koala-"
  :group 'appearance
  :group 'convenience)

(defcustom koala-category 'koala-result
  "The category used when create overlays with `make-overlay'."
  :group 'koala
  :type 'symbol)

(defcustom koala-format "   🐨  %s"
  "Format string for the koala annotations.
N.B. Only the replaced `%s' will get propertised with a face."
  :group 'koala
  :type 'string)

(defcustom koalarc-file ".koalarc.json"
  "The file name of the koala config used for execution."
  :group 'koala
  :type 'string)

(defcustom koala-modes '(js2-mode js-mode javascript-mode web-mode rjsx-mode)
  "List of major modes that `koala' supports running in."
  :group 'koala
  :type 'list)

(defconst koala--symbol " 🐨 "
  "The string to split output on to distinguish results from compilation meta data.")

(defface koala-results-overlay-face
  '((((class color) (background light))
     :foreground "grey10" :background "grey90" :height 0.9)
    (((class color) (background dark))
     :foreground "grey90" :background "grey10" :height 0.9))
  "Face used to display evaluation results at the end of line."
  :group 'koala)


(defface koala-results-prefix-face
  '((((class color) (background light)) :height 0.7)
    (((class color) (background dark)) :height 0.7))
  "Face used to display evaluation results at the end of line."
  :group 'koala)


(defun koala--parse-line (line)
  "Parse a koala compilation LINE into it's semantic parts."
  (pcase-let* ((`(,result ,file) (split-string line koala--symbol))
               (`(,file ,line ,col ,pos) (split-string file ":")))
    `((file . ,file)
      (line . ,(string-to-number line))
      (col . ,(string-to-number col))
      (pos . ,(string-to-number pos))
      (result . ,result))))

(defun koala--make-overlay (line)
  "Convert a koala compilation LINE into an overlay."
  (let-alist (koala--parse-line line)
    (let* ((o-pos (1+ .pos))
           (o-result (format koala-format .result))
           (o (make-overlay o-pos o-pos (current-buffer))))
      (put-text-property 0 (length (format koala-format ""))
                         'face 'koala-results-prefix-face
                         o-result)
      (put-text-property (length (format koala-format ""))
                         (length o-result)
                         'face 'koala-results-overlay-face
                         o-result)
      (overlay-put o 'category koala-category)
      (overlay-put o 'after-string o-result))))

(defun koala--make-overlays (text &optional buffer)
  "Split input TEXT into lines and create overlays.
Create overlays in BUFFER or `current-buffer'"
  (with-current-buffer
      (or buffer (current-buffer))
    (koala-clear)
    (-map 'koala--make-overlay (-reject 's-blank? (split-string text "\n"))))
  t)

;; Interactive mode methods
(defun koala-clear ()
  "Clears all the currently koala results."
  (interactive)
  (remove-overlays nil nil 'category koala-category))

(defun koala--start (b)
  (goto-char b)
  (beginning-of-line)
  (point))

(defun koala--end (e)
  (goto-char e)
  (end-of-line)
  (1+ (point)))

(defun koala--before-change-f (b e)
  "Function to clear current evaluations before save.
B & E are the start and end positions of the change
about to happen."
  (save-excursion
    (let ((beg (koala--start b))
          (end (koala--end e)))
      (remove-overlays beg end 'category koala-category))))

(defun koala--on-save-f ()
  (when (and (buffer-file-name)
             (member major-mode koala-modes))
    (let ((buf (current-buffer)))
      (async-start
       `(lambda ()
          (let* ((conf (format "%s%s"
                               ,(locate-dominating-file (buffer-file-name) koalarc-file )
                               ,koalarc-file))
                 (cmd (format "koala %s --config %s" ,(buffer-file-name) conf)))
            (shell-command-to-string cmd)))
       `(lambda (result)
          (koala--make-overlays result ,buf))))))

(defun koala-mode ()
  (interactive)
  (add-hook 'after-save-hook 'koala--on-save-f)
  (add-to-list 'before-change-functions 'koala--before-change-f))

(provide 'koala)
;;; koala.el ends here
