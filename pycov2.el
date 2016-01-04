;;; pycov2.el --- Support for coverage stats on Python 2.X  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  bertrand

;; Author: bertrand(defvar pycov2-data nil "Coverage data for the buffer") <bertrand.lallau@gmail.com>
;; URL: https://github.com/blallau/pycoverage.el
;; Package-Version: 20160104
;; Keywords: project, convenience
;; Version: 0.0.1

(defvar pycov2-mode-text " pycov2(I)")
;; Need to figure out how to use these without errors
(defvar pycov2-cov2emacs-cmd "cov2emacs")
(defvar pycov2-binary-installed nil) ; cov2emacs
(defvar pycov2-debug-message t)

(make-variable-buffer-local 'pycov2-mode-text)
(make-variable-buffer-local 'pycov2-data)
(make-variable-buffer-local 'pycov2-cov-file)
(make-variable-buffer-local 'pycov2-binary-installed)
(make-variable-buffer-local 'pycov2-debug-message)

(define-minor-mode pycov2-mode
  "Allow annotating the file with coverage information"
  :lighter pycov2-mode-text
  (if pycov2-mode
      (progn
         (add-hook 'after-save-hook 'pycov2-on-change)
         (setq pycov2-binary-installed (pycov2-exe-found pycov2-cov2emacs-cmd))
         (if (not pycov2-binary-installed)
             (error "Missing cov2emacs in PATH")
           )
	 (linum-mode t)
         (setf linum-format 'pycov2-line-format)
	 (pycov2-on-change)
         )
    (setf linum-format 'dynamic)
    (remove-hook 'after-save-hook 'pycov2-on-change)
    (linum-delete-overlays)))

(defun pycov2-exe-found (path)
  ;; spliting and taking last item in order to support something like this:
  ;; "PYTHONPATH=cov2emacs
  (pycov2-message (format "Looking for %s" path))
  (executable-find path))

(defun pycov2-message (txt)
  (if pycov2-debug-message
      (message txt)))

(defun pycov2-on-change ()
  (progn
    (pycov2-message "Running pycov2")
    (pycov2-get-data (buffer-file-name))))

(defun pycov2-get-data (filename)
  (let* ((result (pycov2-run-better filename))
         (lines (split-string result "[\n]+")))
    (setq pycov2-data nil)
    (if result
        (progn
          ;; take status from first line
          (pycov2-process-status (car lines))
          (mapcar (lambda (line)
                    (if (not (equal line ""))
                        (pycov2-process-script-line line)))
                  (cdr lines))))))

(defun pycov2-process-status (line)
  ;; status like looks like this: SUCCESS:23
  ;; where 23 is percent of coverage
  (let* ((data (split-string line ":"))
         (stat (first data)))
    (progn
      ;; set mode-line to error, others will overwrite
      (setq pycov2-mode-text " pycov(...)")
      (force-mode-line-update)
      )
    (when (equal stat "SUCCESS")
      (progn
        ;; update mode-line
        (setq pycov2-mode-text (format " pycov:%s%%" (second data)))
        (force-mode-line-update)))
    (when (equal stat "OLD")
      (progn
        ;; update mode-line
        (setq pycov2-mode-text " pycov(OLD)")
        (force-mode-line-update)))
    (when (equal stat "NO COVERAGE FILE")
      (progn
        ;; update mode-line
        (setq pycov2-mode-text " pycov(Err:no .coverage file)")
        (force-mode-line-update)))))

(defun pycov2-process-script-line (line)
  ;; line looks like this filepath:103:MISSING
  (let* ((data (split-string line ":"))
         (path (first data))
         (number (string-to-number (second data)))
         (status (third data)))
    (when (equal status "MISSING")
      ;; add linenum to pycov2-data
      (add-to-list 'pycov2-data number))))

(defun pycov2-line-format (linenum)
  (cond
   ((member linenum pycov2-data)
    (propertize " " 'face '(:background "red" :foreground "red")))
   (pycov2-data
    ;; covered data
    (propertize " " 'face '(:background " " :foreground " ")))))

(defun pycov2-run-better (filename)
  (let* ((command (format "%s --compile-mode --python-file %s" pycov2-cov2emacs-cmd filename)))
    (message command)
    (shell-command-to-string command)))

(provide 'pycov2)

;;; pycov2.el ends here
