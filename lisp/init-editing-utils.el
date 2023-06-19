;;; init-editing-utils.el --- Editiing utils.
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Prettify symbols mode
;; (global-prettify-symbols-mode t)

;; Auto pairing closures.
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))

(add-hook 'after-init-hook 'electric-indent-mode)
(add-hook 'after-init-hook 'electric-quote-mode)

;; Highlight current line
(global-hl-line-mode)

;; Enable global line wrap
(global-visual-line-mode)

;; Display line number if running GUI Emacs. Otherwise, display line number in modeline.
(if (display-graphic-p)
    (global-display-line-numbers-mode)
  (line-number-mode))

(setq-default view-read-only t ; Overide `read-only-mode' (C-x C-q) with `view-only-mode'.
              blink-cursor-interval 0.4 ; Length of cursor blink in seconds.
              case-fold-search t ; Searches ignore case.
              tab-width 4 ; 4 spaces indentation width.
              indent-tabs-mode nil ; Non tab indentation.
              make-backup-files nil ; Don't create a backup file.
              scroll-preserve-screen-position 'always
              tooltip-delay 1.5)

(add-hook 'after-init-hook 'transient-mark-mode) ; Adds color to marked region.

(use-package rainbow-delimiters
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook
  (prog-mode-hook . rainbow-mode))

(use-package undo-fu
  :ensure t
  :demand t
  :bind (:map global-map
          ("C-x u" . undo-fu-only-undo)
          ("C-S-u" . undo-fu-only-redo)))

(use-package yasnippet
  :init
  (yas-global-mode))

(define-key global-map (kbd "C-c C-w") 'fill-paragraph) ; Wrap text

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
