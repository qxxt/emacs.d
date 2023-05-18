;; Fix Fullscreen GUI on KDE.
(setq frame-resize-pixelwise t)

;; Open GUI Emacs in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable mouse & touchscreen support in non-GUI Emacs.
;; Enabled by default on Termux.
(unless (display-graphic-p)
  (xterm-mouse-mode))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Overide `read-only-mode' (C-x C-q) with `view-only-mode'.
(setq view-read-only t)

;; Change saved customization settings file. This prevents clutter
;; in init.el.
(setq custom-file "~/.emacs.d/custom.el")
;; Load customization file, if exist.
(if (file-exists-p custom-file)
    (load custom-file))

;; Initialize package sources
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
     magit auto-package-update
     company eglot
     go-mode))

;; Check if use-package is exist, install if it isn't.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package auto-package-update
  :ensure t
  :init
  (setq auto-package-update-interval 2
    auto-package-update-delete-old-versions t
    auto-package-update-prompt-before-update t
    auto-package-update-show-preview t)
  :config
  (auto-package-update-maybe))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
  (load-theme 'doom-solarized-light t))

(use-package eglot
  :ensure t
  :init
  (setq eglot-workspace-configuration
    '((:gopls usePlaceholders t))))

(use-package company :ensure t)
(use-package rainbow-mode :ensure t)
(use-package yasnippet :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package smartparens :ensure t)
(use-package go-mode :ensure t)

;; Reevaluate init file
;; (defun reevaluate-init-file ()
;;   (interactive)
;;   (load user-init-file))

;; Save current buffer as cache and run it with `go run`
(defun gorun-buffer ()
  (let ((b (current-buffer))
    (filepath (concat (getenv "HOME") "/.cache/gorun/" (format-time-string "%d-%m-%Y %H:%M:%S" (current-time)) ".go")))
    (with-temp-buffer
      (insert-buffer b)
      (when (re-search-forward (rx "package"
                   (one-or-more whitespace)
                   (group (one-or-more any))
                   word-boundary)
                   nil t)
    (replace-match "main" nil nil nil 1))
      (write-region (point-min) (point-max) filepath))
    (shell-command (concat "go run '" filepath "'"))))

;; Hook for modes derived from `prog-mode' (eg. `go-mode', `c-mode', etc.).
(defun prog-mode-derived-hook ()
  (when (derived-mode-p 'prog-mode)
    (show-paren-mode)
    (follow-mode)
    (hs-minor-mode)
    (rainbow-mode)
    (yas-minor-mode)
    (rainbow-delimiters-mode)
    (smartparens-mode)
    (add-hook 'before-save-hook 'whitespace-cleanup)))
(add-hook 'after-change-major-mode-hook #'prog-mode-derived-hook)

(add-hook 'eglot--managed-mode-hook 'company-mode)

(add-hook 'go-mode-hook
          (lambda() (add-hook 'before-save-hook 'gofmt nil 'local)))

(define-key prog-mode-map (kbd "C-c C-u") 'comment-or-uncomment-region)
(define-key prog-mode-map (kbd "C-c +") 'hs-toggle-hiding)
(define-key go-mode-map (kbd "C-c C-p") 'gorun-buffer)

(custom-set-faces
 '(flymake-error ((t (:foreground "red" :weight bold))))
 '(eglot-diagnostic-tag-unnecessary-face ((t (:underline (:color "red" :style wave)))))
 '(show-paren-match ((t (:background "blue" :foreground "white" :weight ultra-bold))))
 '(show-paren-mismatch ((t (:background "red" :foreground "white" :weight ultra-bold)))))
