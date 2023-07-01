;;; init-prog.el --- Default configurations for prog-mode.
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package corfu
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-preview-current t)      ;; Disable current candidate preview
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-popupinfo-delay 0.5)

  :hook
  ((prog-mode-hook . corfu-mode)
   (org-mode-hook . corfu-mode)
   (corfu-mode-hook . corfu-popupinfo-mode)))

(setq completion-cycle-threshold 3 ; TAB cycle if there are only few candidates
      tab-always-indent 'complete ; Enable indentation+completion using the TAB key.
      )

(use-package eglot
  :init
  (setq-default eglot-workspace-configuration
        ;; gopls configurations
        ;; https://github.com/golang/tools/blob/master/gopls/doc/settings.md
        '((:gopls .
              ((formating.gofumpt . t)
               (ui.completion.usePlaceholders . t)
               (ui.diagnostic.staticcheck . t)))
          (:nil .
            ((formatting.command . "nixpkgs-fmt")))))

  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))

  :bind (:map eglot-mode-map
          ("C-c C-r" . eglot-rename))

  :hook
  (nix-mode-hook . eglot-ensure)
  (eglot-managed-mode-hook . flymake-mode))

(use-package shfmt
  :bind (:map sh-mode-map
              ("C-c C-f" . shfmt-buffer))

  :hook
  (sh-mode-hook . shfmt-on-save-mode))

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

(add-hook 'prog-mode-hook
      #'(lambda()
          (show-paren-mode)
          (follow-mode)
          (hs-minor-mode)
          (add-hook 'before-save-hook 'whitespace-cleanup)))

(define-key prog-mode-map (kbd "C-c C-u") 'comment-or-uncomment-region) ;; toggle comment region
(define-key prog-mode-map (kbd "C-c +") 'hs-toggle-hiding) ;; Toggle hiding code block

(provide 'init-prog)
;;; init-prog.el ends here
