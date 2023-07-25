;;; init-packages.el --- Packages to install. And elpa related configurations.
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'packages-update)

;; Selected packages
(setq package-selected-packages
      '(
	;; Interface
	modus-themes dimmer delight

	;; Package
	use-package auto-package-update

	;; Texts utils
	consult rainbow-mode rainbow-delimiters undo-fu iedit
	yasnippet

	;; Minibuffer
	vertico marginalia

	;; Git
	magit

	;; Treemacs
	treemacs treemacs-magit

	;; Prog-mode
	corfu eglot shfmt python-mode haskell-mode
	dockerfile-mode diff-hl

	;; Golang
	go-mode

	;; Rust
	rust-mode

	;; *Lisp
	sly lispy

	;; Org-mode
	org-bullets ob-go ob-rust

	;; Help
	which-key))

(setq package-update-interval 1)
(package-upgrades?)

(provide 'init-packages)
;;; init-packages.el ends here
