;;; csound-score.el --- A major mode for interacting and coding Csound -*- lexical-binding: t; -*-

;; Copyright (C) 2017 - 2023  Hlöðver Sigurðsson

;; Author: Hlöðver Sigurðsson <hlolli@gmail.com>
;; Version: 0.2.9
;; Package-Requires: ((emacs "25") (shut-up "0.3.2") (multi "2.0.1") (dash "2.16.0") (highlight "0"))
;; URL: https://github.com/hlolli/csound-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This fine includes all helpers and handling of csound-score events
;; for interactive composition

;;; Code:

(require 'csound-font-lock)
(require 'csound-util)
(require 'font-lock)
(require 'dash)
(require 'highlight)

(defun csound-score--align-cols (start end)
  (save-excursion
    (let ((line-end (line-number-at-pos end))
          (max-matrix '()))
      ;; Create matrix of max lengths
      (let ((statements (-> (buffer-substring start end)
                            (substring-no-properties)
                            (split-string "\n"))))
        (dolist (stm statements)
          ;; Remove comments and extra whitespaces
          (let* ((stm* (->> (replace-regexp-in-string "\\(;\\|//\\).*" "" stm)
                            csound-util-chomp
                            ;; replace space by _ in [expression]
                            (replace-regexp-in-string "\\[[^]]*]" #'(lambda (x) (replace-regexp-in-string "\\s-" "_" x)))
                            (replace-regexp-in-string "\\s-+" " ")))
                 (param-list (split-string stm* " "))
                 ;; (param-num (length param-list))
                 (max-matrix-len (length max-matrix))
                 (index 0))
            ;; (print stm*)
            (dolist (param param-list)
              (if (<= max-matrix-len index)
                  (setq max-matrix (append max-matrix (list (length param)))
                        index (1+ index))
                (progn
                  (setf (nth index max-matrix)
                        (max (length param)
                             (nth index max-matrix)))
                  (setq index (1+ index))))))))
      ;; Align the block
      (goto-char start)
      (while (<= (line-number-at-pos) line-end)
        (beginning-of-line)
        ;; Add a space before comment if non
        (save-excursion
          (if (and (re-search-forward "\\(^\\|.\\)\\(;\\|//\\)" (line-end-position) t)
                   (save-match-data (string-match "\\S-" (match-string 1))))
            (replace-match "\\1 \\2")))
        ;; Align the line
        (let ((line-num (line-number-at-pos))
              (param-length 0)
              (index -1))
          (while (= (line-number-at-pos) line-num)
            ;; Align the parameter
            (let* ((margin-length (skip-chars-forward "[:space:]"))
                   (before-comment (looking-at ";\\|//"))
                   (spaces-to-add
                    (if (or (zerop param-length) (eolp))
                        ;; after line beginning or before line end
                        (- margin-length)
                      (- (if before-comment
                             ;; needed length before comment
                             (let ((subvec (nthcdr index max-matrix)))
                               (+ (apply #'+ subvec) (length subvec) 1))
                           ;; needed length before next parameter
                           (1+ (nth index max-matrix)))
                         ;; current length
                         (+ param-length margin-length)))))
              ;; Adjust the margin
              (if (< 0 spaces-to-add)
                  (insert (make-string spaces-to-add ?\040))
                (delete-char spaces-to-add))
              (if before-comment
                  (forward-line)
                (let ((ex-length
                       ;; length of [expression] before ]
                       (if (looking-at "\\[") (skip-chars-forward "^]") 0)))
                  ;; Move to the end of next parameter and get the length
                  (setq param-length
                        (+ ex-length (skip-chars-forward "^[:space:]")))
                  (setq index (1+ index))))
              ;; Exit loop at buffer end
              (if (eobp)
                  (setq line-num 0
                        line-end 0)))))))))

(defun csound-score-align-block ()
  "Align score block so that all parameter are of same space width."
  (interactive)
  (let ((re "^\\s-*[[:alpha:]$]"))
    (if (save-excursion
          ;; See if point is on an score event line
          (beginning-of-line)
          (re-search-forward re (line-end-position) t))
        (let ((beginning-of-block
               (save-excursion
                 ;; Search for beginning of block
                 (beginning-of-line)
                 (while (re-search-backward re (line-beginning-position 0) t)
                   (beginning-of-line))
                 (point)))
              (end-of-block
               (save-excursion
                 ;; Search for end of block
                 (end-of-line)
                 (while (re-search-forward re (line-end-position 2) t)
                   (end-of-line))
                 (point))))
          (csound-score--align-cols beginning-of-block end-of-block)))))

(defun csound-score-trim-time (score-string)
  (let ((trimmed-string (split-string
                         (substring-no-properties
                          score-string)
                         "\n"))
        (min-p2 0)
        (closure-list '())
        (final-str "")
        ;; (lex-p2-list '())
        (p2-list '())
        (last-p3 0))
    (dolist (event trimmed-string)
      (let* ((lexical-p-list (split-string
                              (replace-regexp-in-string
                               "\\s-+" " " (csound-util-chomp event))
                              " "))
             (lex-last-p3 last-p3)
             (lex-p2-list (cons (if (< 2 (length lexical-p-list))
                                    (if (string-equal "+" (nth 2 lexical-p-list))
                                        (if (car p2-list)
                                            (+ (car p2-list) lex-last-p3)
                                          last-p3)
                                      (if (string-equal "." (nth 2 lexical-p-list))
                                          (if (car p2-list)
                                              (car p2-list)
                                            0)
                                        (string-to-number
                                         (nth 2 lexical-p-list))))
                                  0)
                                p2-list)))
        (setq p2-list lex-p2-list
              closure-list (cons
                            (lambda (min-time)
                              (setf (nth 2 lexical-p-list)
                                    (number-to-string
                                     (- (car lex-p2-list)
                                        ;;(nth 2 lexical-p-list)
                                        min-time)))
                              ;; (message "%s lastp3: %s" lex-p2-list lex-last-p3)
                              (string-join lexical-p-list " "))
                            closure-list)
              last-p3 (if (string-equal "." (nth 3 lexical-p-list))
                          last-p3
                        (string-to-number
                         (nth 3 lexical-p-list))))))
    ;; (message "p2-list: %s" p2-list)
    (setq min-p2 (apply #'min p2-list)
          closure-list (reverse closure-list))
    (dolist (event-fn closure-list)
      (setq final-str (concat final-str (funcall event-fn min-p2) "\n")))
    ;; (message "%s" final-str)
    final-str))

(defvar csound-score--last-start)

(defvar csound-score--last-end)

(defun csound-score--flash ()
  (hlt-highlight-region
   csound-score--last-start
   csound-score--last-end
   'font-lock-string-face)
  (run-with-idle-timer
   0.15
   nil
   (lambda ()
     (hlt-unhighlight-region csound-score--last-start csound-score--last-end))))

(defun csound-score-find-instr-def ()
  "For a score statement,
   jump the cursor to where
   its defined in the orchestra.
   Sets a mark."
  (interactive)
  (let* ((instr-on-line (save-excursion
                          (beginning-of-line)
                          (search-forward-regexp "\\<i\\s-?\\(\\\".*\\\"\\|[0-9]+\\)"
                                                 (line-end-position) t 1)
                          (match-string-no-properties 1)))
         (instr-on-line (replace-regexp-in-string "\\\"" "" instr-on-line))
         (search-attempt (save-excursion
                           (goto-char 0)
                           (search-forward-regexp (format "\\<instr\\s-+%s" instr-on-line) nil t 1))))
    (if search-attempt
        (progn (goto-char search-attempt)
               (setq csound-score--last-start (- (point) (length (thing-at-point 'symbol)))
                     csound-score--last-end (1- (point)))
               (csound-score--flash)
               (recenter-top-bottom))
      (message "instrument: %s not found in buffer" instr-on-line))))


(provide 'csound-score)

;;; csound-score.el ends here
