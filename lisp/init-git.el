;;; init-git.el --- Magit and Git
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :ensure t
  :demand t)

(use-package diff-hl
  :ensure t
  :demand t
  :hook ((prog-mode-hook vc-dir-mode-hook) . diff-hl-mode))

(use-package magit-delta
  :ensure t
  :demand t
  :hook (vc-dir-mode-hook . magit-delta-mode))

(provide 'init-git)
;;; init-git.el ends here
