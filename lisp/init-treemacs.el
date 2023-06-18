;;; init-treemacs.el --- Treemacs.
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package treemacs
  :ensure t
  :demand t
  :init
  (setq treemacs-follow-after-init t
    treemacs-indentation 1
    treemacs-silent-refresh t
    treemacs-silent-filewatch t
    treemacs-is-never-other-window t
    treemacs-sorting 'alphabetic-case-insensitive-asc)

  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)

  :bind (:map global-map
    ([f8] . treemacs)
    ("C-c f" . treemacs-select-window))

  :hook
  ((prog-mode-hook vc-dir-mode-hook) . treemacs-display-current-project-exclusively))


(provide 'init-treemacs)
;;; init-treemacs.el ends here
