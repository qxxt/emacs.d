;;; init-go.el --- Configurations for go-mode.
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(put 'gofmt-command 'safe-local-variable 'stringp)

(defun goimports ()
  "Imports dependencies using goimport tool."
  (interactive)
  (let ((gofmt-command "goimports")
        (gofmt-args nil))
    (gofmt)))

(defun go-format-and-import ()
  "Format and imports the required modules."
  (interactive)
  (unless (eq major-mode 'go-mode)
    (error "Not in go-mode"))
  (gofmt)

  ;; https://github.com/golang/go/issues/60230
  (if (bound-and-true-p eglot--managed-mode)
      (condition-case nil
          (eglot-code-actions nil nil "source.organizeImports" t)
        ('error
         (goimports)))

    (goimports)))

(use-package go-mode
  :init
  (setq gofmt-command "gofumpt")
  (setq gofmt-args (list "-extra"))

  :bind (:map go-mode-map
              ("C-c C-f" . go-format-and-import)
              ("C-c C-e" . mock-buffer))

  :hook
  (go-mode-hook . (lambda()
                    (add-hook 'before-save-hook 'go-format-and-import nil 'local))))

;; I’m using gofumpt with ’("--extra") arguments.
;; When projects set gofmt-command to "gofmt"
;; it will throw unknown parameter error.
(advice-add 'gofmt
            :before (lambda ()
                      (if (equal gofmt-command "gofmt")
                          (setq gofmt-args nil))))

(provide 'init-go)
;;; init-go.el ends here
