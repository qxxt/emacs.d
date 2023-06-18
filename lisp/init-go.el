;;; init-go.el --- Configurations for go-mode.
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun go-format-and-import ()
  "Format and imports the required modules."
  (interactive)
  (unless (eq major-mode 'go-mode)
    (message "err 1")
    (error "Not in go-mode"))

  (gofmt)
  (if (bound-and-true-p eglot--managed-mode)
      (ignore-errors (eglot-code-actions nil nil "source.organizeImports" t))
    (goimports)))

(defun go-eval-buffer ()
  "Save current buffer as cache and run it with `go run`."
  (interactive)
  (go-format-and-import)
  (let ((b (current-buffer))
    (filepath (concat (getenv "HOME") "/.cache/gorun/" (format-time-string "%d-%m-%Y %H:%M:%S" (current-time)) ".go")))
    (with-temp-buffer
      (insert-buffer-substring b)
      (when (re-search-forward (rx "package"
                   (one-or-more whitespace)
                   (group (one-or-more any))
                   word-boundary)
                   nil t)
    (replace-match "main" nil nil nil 1))
      (write-region (point-min) (point-max) filepath))
    (shell-command (concat "go run '" filepath "'"))))

;; TODO
(defun goimports ()
  "Imports dependencies using goimport tool."
  (interactive))

(use-package go-mode
  :init
  (setq gofmt-command "gofumpt")

  :bind (:map go-mode-map
          ("C-c C-f" . go-format-and-import)
          ("C-c C-e" . go-eval-buffer))

  :hook
  (go-mode-hook . (lambda()
                (add-hook 'before-save-hook 'go-format-and-import))))

(provide 'init-go)
;;; init-go.el ends here
