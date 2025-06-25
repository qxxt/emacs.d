;;; init-modeline.el --- modeline.  -*- lexical-binding: t; -*-
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package delight
  :config
  (delight 'rainbow-mode)
  (delight 'hs-minor-mode nil 'hideshow)
  (delight 'yas-minor-mode nil 'yasnippet)
  (delight 'counsel-mode nil 'counsel)
  (delight 'ivy-mode nil 'ivy)
  (delight 'which-key-mode nil 'which-key)
  (delight 'eldoc-mode nil 'eldoc))

(provide 'init-modeline)
;;; init-modeline.el ends here
