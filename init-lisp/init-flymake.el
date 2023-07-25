;;; init-flymake.el --- flymake configuration
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))

(provide 'init-flymake)
;;; init-flymake.el ends here
