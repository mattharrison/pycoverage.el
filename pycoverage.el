;;; pycoverage.el -- Python Coverage Analysis Tool

;;; Copyright (c) 2009 matt harrison
;;;
;;; Use and distribution subject to the terms of the rcov license.
;;; modeled after rcov.el


(defvar pycoverage-before-visit-source-hook nil
  "Hook executed before jump.")
(defvar pycoverage-after-visit-source-hook nil
  "Hook executed after jump.")

(define-derived-mode pycoverage-mode python-mode "Python-Cov"
  "Major mode for annotating Python scripts with coverage information"
  (setq truncate-lines t)
  ;;  (and ruby-electric-mode (ruby-electric-mode -1))
  (and (boundp 'pabbrev-mode) pabbrev-mode (pabbrev-mode -1))
  (suppress-keymap pycoverage-mode-map)
  (define-key pycoverage-mode-map "\C-i" 'pycoverage-next-tag)
  (define-key pycoverage-mode-map "\M-\C-i" 'pycoverage-previous-tag)
  (define-key pycoverage-mode-map "\C-m" 'pycoverage-visit-source)
  (set (make-local-variable 'automatic-hscrolling) nil)
  )

(defvar pycoverage-tag-regexp "\\[\\[\\(.*?\\)\\]\\]")

(defun pycoverage-next-tag (n)
  "Go to next LINK"
  (interactive "p")
  (when (looking-at pycoverage-tag-regexp)
    (goto-char (match-end 0)))
  (when (re-search-forward  pycoverage-tag-regexp nil t n)
    (goto-char (match-beginning 0)))
  (pycoverage-show-link)
  )

(defun pycoverage-previous-tag (n)
  "Go to previous LINK."
  (interactive "p")
  (re-search-backward pycoverage-tag-regexp nil t n)
  (pycoverage-show-link)
  )
(defvar pycoverage-link-tempbuffer " *pycoverage-link*")

(defun pycoverage-show-link ()
  "Follow current LINK."
  (let ((link (match-string 1))
        (eol (point-at-eol)))
    (save-excursion
      (when (and link
                 (re-search-backward "# \\(>>\\|<<\\) " (point-at-bol) t))
        (while (re-search-forward pycoverage-tag-regexp eol t)
          (let ((matched (match-string 1)))
            (when (string= link matched)
              (add-text-properties 0 (length matched) '(face highlight) matched))
            (with-current-buffer (get-buffer-create pycoverage-link-tempbuffer)
              (insert matched "\n"))))
        (let (message-log-max)          ; inhibit *Messages*
          (message "%s" (with-current-buffer pycoverage-link-tempbuffer
                          (substring (buffer-string) 0 -1)))) ; chomp
        (kill-buffer pycoverage-link-tempbuffer)))))


;; copied from jw-visit-source
(defun pycoverage-extract-file-lines (line)
  "Extract a list of file/line pairs from the given line of text."
  (let*
      ((unix_fn "[^ \t\n\r\"'([<{]+")
       (dos_fn  "[a-zA-Z]:[^ \t\n\r\"'([<{]+")
       (flre (concat "\\(" unix_fn "\\|" dos_fn "\\):\\([0-9]+\\)"))
       (start nil)
       (result nil))
    (while (string-match flre line start)
      (setq start (match-end 0))
      (setq result
            (cons (list
                   (substring line (match-beginning 1) (match-end 1))
                   (string-to-int (substring line (match-beginning 2) (match-end 2))))
                  result)))
    result))


(defun pycoverage-select-file-line (candidates)
  "Select a file/line candidate that references an existing file."
  (cond ((null candidates) nil)
        ((file-readable-p (caar candidates)) (car candidates))
        (t (pycoverage-select-file-line (cdr candidates))) ))


(defun pycoverage-visit-source ()
  "If the current line contains text like '../src/program.rb:34', visit
  that file in the other window and position point on that line."
  (interactive)
  (let* ((line (progn (looking-at pycoverage-tag-regexp) (match-string 1)))

         (candidates (pycoverage-extract-file-lines line))
         (file-line (pycoverage-select-file-line candidates)))
    (cond (file-line
           (run-hooks 'pycoverage-before-visit-source-hook)
           (find-file (car file-line))
           (goto-line (cadr file-line))
           (run-hooks 'pycoverage-after-visit-source-hook))
          (t
           (error "No source location on line.")) )))


;; matt stuff
(defun pycoverage-load-coverage ()
  "Look for .coverage file"
  (interactive)
    (compile-internal (format "cov2emacs --python-file %s" (buffer-file-name)) ""
                      nil nil nil (lambda (x)
                                    "*pycoverage results*"))
  )

(provide 'pycoverage)