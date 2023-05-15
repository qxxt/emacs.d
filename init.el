;; Fix Fullscreen GUI on KDE.
(setq frame-resize-pixelwise t)

;; Overide `read-only-mode' (C-x C-q) with `view-only-mode'.
(setq view-read-only t)

;; Change saved customization settings file. This prevents clutter
;; in init.el.
(setq custom-file "~/.emacs.d/custom.el")
;; Load customization file.
;; (load custom-file)

;; Enable mouse & touchscreen support in non-GUI Emacs.
;; Enabled by default on Termux.
(unless (display-graphic-p)
  (xterm-mouse-mode))

;; Initialize package sources
(require 'package)

;; Add Melpa to package sources list
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Initialize package as `package-enable-at-startup' is set to nil in early-init.el
(package-initialize)

;; Prefer Melpa over default repository
;; This is unnecessary as Melpa is added to the beginning of the
;; package sources list.
(setq package-archive-priorities '(("melpa" . 1)))


;; Selected packages
(setq package-selected-packages
      '(use-package
	 solarized-theme rainbow-mode
	 yasnippet rainbow-delimiters smartparens
	 magit auto-package-update
	 company lsp-mode lsp-ui flycheck
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

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-solarized-light t))

(use-package rainbow-mode :ensure t)
(use-package yasnippet :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package smartparens :ensure t)

;; Hook for modes derived from `prog-mode' (eg. `go-mode', `c-mode', etc.).
(defun prog-mode-derived-hook ()
  (when (derived-mode-p 'prog-mode)
    (rainbow-mode)
    (yas-minor-mode)
    (rainbow-delimiters-mode)
    (smartparens-mode)
    (follow-mode)))

(add-hook 'after-change-major-mode-hook #'prog-mode-derived-hook)
