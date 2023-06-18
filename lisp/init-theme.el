;;; init-theme.el --- Theme.
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package doom-themes
  :ensure t
  :demand t
  :init
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)

  :config
  (load-theme 'doom-solarized-light t))

(provide 'init-theme)
;;; init-theme.el ends here
