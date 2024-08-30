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
  (setq vertico-count 10
        vertico-resize t)

  :config
  (vertico-mode)

  :bind (:map vertico-map
              ("C-n" . vertico-scroll-up)
              ("C-p" . vertico-scroll-down)))

(use-package marginalia
  :ensure t
  :demand t
  :config
  (marginalia-mode))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
