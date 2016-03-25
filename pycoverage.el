;;; pycoverage.el --- Support for coverage stats on Python 2.X and 3  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 harrison, bertand

;; Author: matt harrison
;; URL: https://github.com/mattharrison/pycoverage.el
;; Package-Version: 20160323
;; Keywords: project, convenience
;; Version: 0.0.2
;; Package-Requires: ((emacs "24.3"))

;; Thanks to bertrand.lallau@gmail.com for refactor

;;; Code:

(require 'cl-lib)

(defconst pycoverage-mode-text " pycoverage(I)")
;; Need to figure out how to use these without errors
(defconst pycoverage-cov2emacs-cmd "cov2emacs")
(defvar-local pycoverage-binary-installed nil) ; cov2emacs
(defvar-local pycoverage-debug-message t)
(defvar-local pycoverage-data nil)

;;;###autoload
(define-minor-mode pycoverage-mode
  "Allow annotating the file with coverage information"
  :lighter pycoverage-mode-text
  (if pycoverage-mode
      (progn
        (unless
            (setq pycoverage-binary-installed
                  (pycoverage-exe-found pycoverage-cov2emacs-cmd))
          (error "Missing cov2emacs in PATH"))
        (add-hook 'after-save-hook 'pycoverage-on-change nil t)
        (linum-mode t)
        (setf linum-format 'pycoverage-line-format)
        (pycoverage-on-change))
    (setf linum-format 'dynamic)
    (remove-hook 'after-save-hook 'pycoverage-on-change)
    (linum-delete-overlays)))

(defun pycoverage-exe-found (path)
  ;; spliting and taking last item in order to support something like this:
  ;; "PYTHONPATH=cov2emacs
  (pycoverage-message (format "Looking for %s" path))
  (executable-find path))

(defun pycoverage-message (txt)
  (if pycoverage-debug-message
      (message txt)))

(defun pycoverage-on-change ()
  (progn
    (pycoverage-message "Running pycoverage")
    (pycoverage-get-data (buffer-file-name))))

(defun pycoverage-get-data (filename)
  (let* ((result (pycoverage-run-better filename))
         (lines (split-string result "[\n]+")))
    (setq pycoverage-data nil)
    (if result
        (progn
          ;; take status from first line
          (pycoverage-process-status (car lines))
          (mapcar (lambda (line)
                    (if (not (equal line ""))
                        (pycoverage-process-script-line line)))
                  (cdr lines))))))

(defun pycoverage-process-status (line)
  ;; status like looks like this: SUCCESS:23
  ;; where 23 is percent of coverage
  (let* ((data (split-string line ":"))
         (stat (cl-first data)))
    (progn
      ;; set mode-line to error, others will overwrite
      (setq pycoverage-mode-text " pycov(...)")
      (force-mode-line-update))
    (when (equal stat "SUCCESS")
      (progn
        ;; update mode-line
        (setq pycoverage-mode-text (format " pycov:%s%%" (cl-second data)))
        (force-mode-line-update)))
    (when (equal stat "OLD")
      (progn
        ;; update mode-line
        (setq pycoverage-mode-text " pycov(OLD)")
        (force-mode-line-update)))
    (when (equal stat "NO COVERAGE FILE")
      (progn
        ;; update mode-line
        (setq pycoverage-mode-text " pycov(Err:no .coverage file)")
        (force-mode-line-update)))))

(defun pycoverage-process-script-line (line)
  ;; line looks like this filepath:103:MISSING
  (let* ((data (split-string line ":"))
         (path (cl-first data))
         (number (string-to-number (cl-second data)))
         (status (cl-third data)))
    (when (equal status "MISSING")
      ;; add linenum to pycoverage-data
      (add-to-list 'pycoverage-data number))))

(defun pycoverage-line-format (linenum)
  (cond
   ((member linenum pycoverage-data)
    (propertize " " 'face '(:background "red" :foreground "red")))
   (pycoverage-data
    ;; covered data
    (propertize " " 'face '(:background " " :foreground " ")))))

(defun pycoverage-run-better (filename)
  (let* ((command (format "%s --compile-mode --python-file %s" pycoverage-cov2emacs-cmd filename)))
    (message command)
    (shell-command-to-string command)))

(defun pycoverage-refresh ()
  "reload data for buffer"
  (interactive )
  (pycoverage-get-data (buffer-file-name)))


(provide 'pycoverage)

;;; pycoverage.el ends here
