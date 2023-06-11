;;; Code:
;; Fix Fullscreen GUI on KDE.
(setq frame-resize-pixelwise t)

;; Open GUI Emacs in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable mouse & touchscreen support in non-GUI Emacs.
;; Enabled by default on Termux.
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

;; Save minibuffer histories
(savehist-mode)

;; Change saved customization settings file. This prevents clutter
;; in init.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; Load customization file, if exist.
(if (file-exists-p custom-file)
    (load custom-file))

;; Initialize package sources

;;; Commentary:
;;

(require 'package)

;; Add Melpa to package sources list
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Initialize package as `package-enable-at-startup' is set to nil in early-init.el
(package-initialize)

;; Prefer Melpa over default repository
(setq package-archive-priorities '(("melpa" . 1)))

;; Selected packages
(setq package-selected-packages
      '(use-package
         doom-themes rainbow-mode
         yasnippet rainbow-delimiters smartparens
         magit auto-package-update vertico marginalia
         company eglot
         go-mode
         ob-go org-bullets diff-hl
         nix-mode
         yasnippet-snippets
         ;; slime
         ))

;; Check if `use-package' is exist and  checks if `package-archive-contents' is empty.
;; Update `package-archive' and install `use-package' if they aren't true isn't.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;; Ensure all use-package packages
(setq use-package-always-ensure t)

;; Defer loading packages unless explicitly demanded.
(setq use-package-always-defer t)

;; Disable suffix "-hook" for use-package's :hook
(setq use-package-hook-name-suffix nil)

;; `time-to-number-of-days' depends upon `time-date'
(require 'time-date)
(use-package auto-package-update
  :demand t
  :init
  (setq auto-package-update-interval 1
        auto-package-update-delete-old-versions t
        auto-package-update-prompt-before-update t
        auto-package-update-show-preview t)
  :config

  ;; Update `package-archives' and upgrade packages when it's last updated
  ;; `auto-package-update-interval' days ago, with prompts.
  (when (and (<= auto-package-update-interval
                 (time-to-number-of-days
                  (time-since
                   (file-attribute-modification-time (file-attributes (concat package-user-dir "/archives/gnu/archive-contents"))))))
             (y-or-n-p "Update packages now? "))
    (package-refresh-contents)
    (auto-package-update-now)))

(use-package diff-hl
  :demand t
  :hook
  ((prog-mode-hook vc-dir-mode-hook) . turn-on-diff-hl-mode))

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-solarized-light t))

(use-package eglot
  :demand t
  :init
  (setq-default eglot-workspace-configuration
                ;; gopls configurations
                ;; https://github.com/golang/tools/blob/master/gopls/doc/settings.md
                '((:gopls .
                          ((formating.gofumpt . t)
                           (ui.completion.usePlaceholders . t)
                           (ui.diagnostic.staticcheck . t)))))
  :hook
  (eglot-managed-mode-hook . company-mode)
  :bind (:map eglot-mode-map
              ("C-c C-r" . eglot-rename)))

(defun go-format-and-import ()
  (interactive)
  (if (not (bound-and-true-p go-mode))
    (error "Not a go-mode"))
  (gofmt)
  (if (bound-and-true-p eglot--managed-mode)
      (ignore-errors (eglot-code-actions nil nil "source.organizeImports" t))))

(defun gorun-buffer ()
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

(use-package go-mode
  :demand t
  :init
  (setq gofmt-command "gofumpt")
  :bind (:map go-mode-map
              ("C-c C-p" . gorun-buffer))
  :config
  (add-hook 'go-mode-hook (lambda()
                            (add-hook 'before-save-hook 'go-format-and-import))))

(use-package vertico
  :demand t
  :init
  (vertico-mode)
  (setq vertico-count 10)
  (setq vertico-resize t)
  :bind (:map vertico-map
              ("C-]" . vertico-scroll-up)
              ("C-[" . vertico-scroll-down)))

(use-package marginalia
  :demand t
  :init
  (marginalia-mode))

(use-package ob-go
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((go . t))))

(use-package org-bullets
  :demand t
  :hook
  (org-mode-hook . org-bullets-mode))

(use-package company
  :demand t
  :hook
  (prog-mode-hook . company-mode)
  (prog-mode-hook . flymake-mode))

(use-package magit
  :demand t)

(use-package yasnippet
  :demand t
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

;; Reevaluate init file
;; (defun reevaluate-init-file ()
;;   (interactive)
;;   (load user-init-file))

;; Prog mode hook
(add-hook 'prog-mode-hook
          #'(lambda()
              (display-line-numbers-mode)
              (show-paren-mode)
              (follow-mode)
              (hs-minor-mode)
              (add-hook 'before-save-hook 'whitespace-cleanup)
              (flymake-mode)))

(define-key prog-mode-map (kbd "C-c C-u") 'comment-or-uncomment-region)
(define-key prog-mode-map (kbd "C-c +") 'hs-toggle-hiding)

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
