;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Fix Fullscreen GUI on KDE.
(setq frame-resize-pixelwise t)

;; Open GUI Emacs in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable mouse & touchscreen support in non-GUI Emacs. Enabled by
;; default on Termux.
(unless (display-graphic-p)
  (xterm-mouse-mode))

;; Indentation behavior and style
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Prettify symbols mode
;; (global-prettify-symbols-mode t)

;; Overide `read-only-mode' (C-x C-q) with `view-only-mode'.
(setq view-read-only t)

;; Highlight current line
(global-hl-line-mode)

;; Enable global line wrap
(global-visual-line-mode 1)

;; Save minibuffer histories
(savehist-mode)

;; Change saved customization settings file. This prevents clutter in
;; init.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; Load customization file, if exist.
(if (file-exists-p custom-file)
    (load custom-file))

;; Initialize package sources
(require 'package)

;; Add Melpa to package sources list
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Initialize package as `package-enable-at-startup' is set to nil in
;; early-init.el
(package-initialize)

;; Prefer Melpa over default repository
(setq package-archive-priorities '(("melpa" . 1)))

;; Selected packages
(setq package-selected-packages
      '(doom-themes rainbow-mode
         use-package
         yasnippet yasnippet-snippets
         go-snippets py-snippets common-lisp-snippets haskell-snippets
         rainbow-delimiters smartparens
         magit auto-package-update vertico marginalia
         company eglot
         go-mode
         ob-go org-bullets diff-hl
         nix-mode
         slime
         nixpkgs-fmt
         python-mode
         haskell-mode
         consult
         shfmt
         treemacs))

;; Check if `use-package' is exist and checks if
;; `package-archive-contents' is empty. Update `package-archive' and
;; install `use-package' if they aren't true isn't.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Ensure all use-package packages
;; (setq use-package-always-ensure t)

;; Defer loading packages unless explicitly demanded.
(setq use-package-always-defer t)

;; Disable suffix "-hook" for use-package's :hook
(setq use-package-hook-name-suffix nil)

;; `time-to-number-of-days' depends upon `time-date'
(require 'time-date)

(use-package auto-package-update
  :ensure t
  :demand t
  :init
  (setq auto-package-update-interval 1
        auto-package-update-delete-old-versions t
        auto-package-update-prompt-before-update t
        auto-package-update-show-preview t)

  :config
  ;; Update `package-archives' and upgrade packages when it's last
  ;; updated `auto-package-update-interval' days ago, with prompts.
  (when (and (<= auto-package-update-interval
                 (time-to-number-of-days
                  (time-since
                   (file-attribute-modification-time (file-attributes (concat package-user-dir "/archives/gnu/archive-contents"))))))
             (y-or-n-p "Update packages now? "))
    (package-refresh-contents)
    (auto-package-update-maybe)))

(use-package diff-hl
  :ensure t
  :demand t
  :hook
  ((prog-mode-hook vc-dir-mode-hook) . diff-hl-mode))

(use-package undo-fu
  :ensure t
  :demand t
  :bind (:map global-map
              ("C-x u" . undo-fu-only-undo)
              ("C-S-u" . undo-fu-only-redo)))

(use-package doom-themes
  :ensure t
  :demand t
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  :config
  (load-theme 'doom-solarized-light t))

(use-package treemacs
  :ensure t
  :demand t
  :init
  (setq treemacs-follow-after-init t
        treemacs-indentation 1
        treemacs-silent-refresh t
        treemacs-silent-filewatch t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc)

  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)

  :bind (:map global-map
        ([f8] . treemacs)
        ("C-c f" . treemacs-select-window))

  :hook
  ((prog-mode-hook vc-dir-mode-hook) . treemacs-display-current-project-exclusively))

(use-package vertico
  :ensure t
  :demand t
  :init
  (vertico-mode)
  (setq vertico-count 10
        vertico-resize t)

  :bind (:map vertico-map
              ("C-]" . vertico-scroll-up)
              ("C-[" . vertico-scroll-down)))

(use-package marginalia
  :ensure t
  :demand t
  :init
  (marginalia-mode))

;; Org-mode
(use-package ob-go
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((go . t))))

(use-package org-bullets
  :hook
  (org-mode-hook . org-bullets-mode))

;; Prog-mode
(load (expand-file-name "go.el" user-emacs-directory))

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

(use-package yasnippet
  :hook
  (prog-mode-hook . yas-minor-mode))

(use-package smartparens
  :hook
  (prog-mode-hook . smartparens-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook
  (prog-mode-hook . rainbow-mode))

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

;; Reevaluate init file
(defun reevaluate-init-file ()
  "Reevaluate init.el file for debugging."
  (interactive)
  (load user-init-file))

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
(define-key global-map (kbd "C-c C-w") 'fill-paragraph) ;; Wrap text
(define-key lisp-interaction-mode-map (kbd "C-c C-e") 'eval-buffer)

(custom-set-faces
 '(flymake-error ((t (:foreground "red" :weight bold))))
 '(mode-line ((t (:background "#9CCC65" :foreground "#424242" :box nil))))
 '(mode-line-buffer-id ((t (:foreground "#212121" :weight bold))))
 '(mode-line-inactive ((t (:background "#C5E1A5" :foreground "#424242" :box nil))))
 '(show-paren-match ((t (:background "blue" :foreground "white" :weight ultra-bold))))
 '(show-paren-mismatch ((t (:background "red" :foreground "white" :weight ultra-bold))))
 '(eglot-diagnostic-tag-unnecessary-face ((t (:underline (:color "red" :style wave))))))

(provide 'init)

;;; init.el ends here
