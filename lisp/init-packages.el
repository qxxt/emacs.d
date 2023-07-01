;;; init-packages.el --- Packages to install. And elpa related configurations.
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Selected packages
(setq package-selected-packages
      '(
        ;; Interface
        modus-themes dimmer delight

        ;; Package
        use-package auto-package-update

        ;; Texts utils
        rainbow-mode rainbow-delimiters undo-fu iedit yasnippet
        yasnippet-snippets
        ;; go-snippets py-snippets common-lisp-snippets
        ;; haskell-snippets

        ;; Minibuffer
        vertico marginalia

        ;; Git
        magit diff-hl

        ;; Treemacs
        treemacs treemacs-magit

        ;; Prog-mode
        corfu eglot shfmt python-mode haskell-mode dockerfile-mode

        ;; Golang
        go-mode

        ;; Rust
        rust-mode

        ;; *Lisp
        slime

        ;; Org-mode
        org-bullets ob-go ob-rust

        ;; Misc
        which-key
        ))

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
  (if (and (<= auto-package-update-interval
               (time-to-number-of-days
                (time-since
                 (file-attribute-modification-time (file-attributes (concat package-user-dir "/archives/gnu/archive-contents"))))))
           (y-or-n-p "Update packages now? "))
      (auto-package-update-maybe)))

(provide 'init-packages)
;;; init-packages.el ends here
