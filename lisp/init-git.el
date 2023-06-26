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
  :init
  (global-diff-hl-mode)
  (diff-hl-margin-mode))

;; (use-package magit-delta
;;   :ensure t
;;   :demand t
;;   :init
;;   (magit-delta-mode 1))

(provide 'init-git)
;;; init-git.el ends here
