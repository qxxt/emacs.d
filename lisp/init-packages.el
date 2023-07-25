;;; init-packages.el --- Packages to install. And elpa related configurations.
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'time-date)

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
	slime lispy

	;; Org-mode
	org-bullets ob-go ob-rust

	;; Help
	which-key))

(use-package auto-package-update
  :ensure t
  :demand t
  :init
  (setq auto-package-update-interval 3
	auto-package-update-delete-old-versions t
	auto-package-update-prompt-before-update t
	auto-package-update-show-preview t)

  :config
  (defun apu-package-refresh-content? ()
    (when (and
	   (or (not package-archive-contents)
	       (not (> auto-package-update-interval
		       (time-to-number-of-days
			(time-since
			 (file-attribute-modification-time
			  (file-attributes
			   (concat package-user-dir "/archives/gnu/archive-contents"))))))))
	   (y-or-n-p "Refresh packages now?"))
      (package-refresh-contents)
      t))

  (defun apu-update-list? ()
    (let* ((package-to-update-list (apu--filter-quelpa-packages (apu--packages-to-install)))
	   update-then?)
      (if (null package-to-update-list)
	  (message "All packages up to date")
	(apu--write-preview-buffer (concat "[PACKAGES TO UPDATE]:\n" (mapconcat #'symbol-name package-to-update-list "\n")))
	(setq update-then? (y-or-n-p "Update packages now?"))
	(apu--hide-preview)
	update-then?)))

  (if (and (or (not (file-exists-p auto-package-update-last-update-day-path))
	       (not (> auto-package-update-interval (- (apu--today-day) (apu--read-last-update-day)))))
	   (apu-package-refresh-contents?)
	     (apu-update-list?))
      (auto-package-update-now)))

(provide 'init-packages)
;;; init-packages.el ends here
