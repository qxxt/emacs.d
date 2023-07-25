;;; init-treemacs.el --- Treemacs.
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package treemacs
  :ensure t
  :demand t
  :config
  (setq treemacs-follow-after-init t
        treemacs-silent-refresh t
        treemacs-silent-filewatch t
        treemacs-sorting 'mod-time-desc)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)

  (treemacs-hide-gitignored-files-mode nil)

  :bind (:map global-map
              ("C-c f" . treemacs-select-window)
              ([f8] . treemacs)))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(provide 'init-treemacs)
;;; init-treemacs.el ends here
