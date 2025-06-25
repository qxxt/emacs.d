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

;; Enable copy-paste on tui
(use-package xclip
  :unless window-system
  :config
  (xclip-mode))

;; Highlight current line
(global-hl-line-mode)

;; Enable global line wrap
(global-visual-line-mode)

;; Display line number if running GUI Emacs.
;; Otherwise, display line number in modeline.
(eval-during-frame-start
  (if (display-graphic-p)
      (progn
        (add-hook 'prog-mode-hook 'display-line-numbers-mode)
        (add-hook 'org-mode-hook 'display-line-numbers-mode)
        (add-hook 'text-mode-hook 'display-line-numbers-mode)
        (line-number-mode 0))
    (line-number-mode)))

(setq tab-width 2
      comment-empty-lines t ;; Make comment region comments empty lines.
      view-read-only t ;; Overide `read-only-mode' (C-x C-q) with `view-only-mode'.
      case-fold-search t ;; Searches ignore case.
      blink-cursor-interval 1
      blink-cursor-delay 1
      make-backup-files nil
      scroll-preserve-screen-position t
      tooltip-delay 1.5)

(setq-default indent-tabs-mode nil
              column-number-mode t
              transient-mark-mode t)

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

(define-key global-map (kbd "C-c C-w") 'fill-paragraph) ; Wrap text

(use-package pcre2el
  :config
  (pcre-mode)
  :bind (:map global-map
              ("C-c r" . query-replace-regexp)))

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
