;;; init-theme.el --- Theme.
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package modus-themes
  :ensure t
  :demand t
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-org-blocks 'gray-background
        modus-themes-common-palette-overrides
        '((underline-err "#ff0000")
          (underline-warning "#ffd700")
          (bg-added-fringe "#00ff00")
          (bg-changed-fringe "#ffff00")
          (bg-removed-fringe "#ff0000")))

  :config
  (load-theme 'modus-operandi t))

(use-package dimmer
  :demand t
  :init
  (setq dimmer-fraction 0.3
        dimmer-adjustment-mode :foreground
        dimmer-use-colorspace :rgb)

  :config
  (dimmer-mode))

(provide 'init-theme)
;;; init-theme.el ends here
