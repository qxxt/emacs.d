;;; init-theme.el --- Theme.
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package doom-themes
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  ;; (load-theme 'doom-solarized-dark-high-contrast t)
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (nerd-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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
