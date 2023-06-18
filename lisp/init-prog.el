;;; init-prog.el --- Default configurations for prog-mode.
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package company
  :hook
  (eglot-managed-mode-hook . company-mode)
  (prog-mode-hook . company-mode)
  (prog-mode-hook . flymake-mode))

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
  (nix-mode-hook . eglot-ensure))

;; sh-mode
(use-package shfmt
  :bind (:map sh-mode-map
              ("C-c C-f" . shfmt-buffer))

  :hook
  (sh-mode-hook . shfmt-on-save-mode))

;; Nixos
(use-package nix-mode
  :mode "\\.nix\\'"
  :hook
  (nix-mode-hook . nix-prettify-mode)
  (nix-mode-hook . nix-format-before-save)

  :bind (:map nix-mode-map
          ("C-c C-f" . nix-format-buffer)))

;; Dockerfile-mode
(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

;; Prog mode hook
(add-hook 'prog-mode-hook
      #'(lambda()
          (display-line-numbers-mode)
          (show-paren-mode)
          (follow-mode)
          (hs-minor-mode)
          (add-hook 'before-save-hook 'whitespace-cleanup)
          (flymake-mode)))

(define-key prog-mode-map (kbd "C-c C-u") 'comment-or-uncomment-region) ;; toggle comment region
(define-key prog-mode-map (kbd "C-c +") 'hs-toggle-hiding) ;; Toggle hiding code block

(provide 'init-prog)
;;; init-prog.el ends here
