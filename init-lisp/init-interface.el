;;; init-interface.el --- Interface related configurations such as fullscreen and mouse support.
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil)

;; Disable welcome message
;; (setq inhibit-startup-screen t)

;; Open GUI Emacs in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable mouse & touchscreen support in non-GUI Emacs. Enabled by
;; default on Termux.
(unless (display-graphic-p)
  (xterm-mouse-mode))

(setq window-resize-pixelwise t
      frame-resize-pixelwise t) ;; Fix Fullscreen GUI on KDE.

(provide 'init-interface)
;;; init-interface.el ends here
