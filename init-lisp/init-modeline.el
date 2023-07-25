;;; init-modeline.el --- modeline.
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package diminish
  :ensure t
  :demand t
  :config
  (diminish 'hs-minor-mode)
  (diminish 'yas-minor-mode)
  (diminish 'rainbow-mode)
  (diminish 'eldoc-mode)
  (diminish 'visual-line-mode)
  (diminish 'auto-revert-mode))

(provide 'init-modeline)
;;; init-modeline.el ends here
