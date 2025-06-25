;;; init-prog.el --- Default configurations for prog-mode.
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(use-package company
  :init
  (setq completion-ignore-case t)

  :config
  (global-company-mode))

;; (setq completion-cycle-threshold 3 ; TAB cycle if there are only few candidates
;;       tab-always-indent 'complete ; Enable indentation+completion using the TAB key.
;;       )

(use-package eglot
  :init
  (setq eglot-workspace-configuration
        ;; gopls configurations
        ;; https://github.com/golang/tools/blob/master/gopls/doc/settings.md
        '((:gopls .
                  ((formating.gofumpt . t)
                   (ui.completion.usePlaceholders . t)
                   (ui.diagnostic.staticcheck . t)))))

  :bind (:map eglot-mode-map
              ("C-;" . eglot-rename))

  :hook
  (eglot-managed-mode-hook . flymake-mode))

(use-package shfmt
  :bind (:map sh-mode-map
              ("C-c C-f" . shfmt-buffer))

  :hook
  (sh-mode-hook . shfmt-on-save-mode))

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

(use-package iedit
  :demand t)

(use-package flymake-popon
  :hook
  (flymake-mode-hook . flymake-popon-mode))

(add-hook 'prog-mode-hook
          #'(lambda()
              (show-paren-mode)
              (hs-minor-mode)
              (add-hook 'before-save-hook 'whitespace-cleanup)))

(define-key prog-mode-map (kbd "C-c +") 'hs-toggle-hiding) ;; Toggle hiding code block

(define-key prog-mode-map (kbd "M-.") 'xref-find-definitions) ;; Toggle hiding code block

(provide 'init-prog)
;;; init-prog.el ends here
