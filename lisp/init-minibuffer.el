;;; init-minibuffer.el --- Minibuffer.
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Save minibuffer histories
(savehist-mode)

(use-package vertico
  :ensure t
  :demand t
  :init
  (vertico-mode)
  (setq vertico-count 10
    vertico-resize t)

  :bind (:map vertico-map
          ("C-]" . vertico-scroll-up)
          ("C-[" . vertico-scroll-down)))

(use-package marginalia
  :ensure t
  :demand t
  :init
  (marginalia-mode))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
