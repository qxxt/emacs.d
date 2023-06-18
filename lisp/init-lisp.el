;;; init-lisp.el --- *Lisp configurations.
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl"))

(define-key lisp-interaction-mode-map (kbd "C-c C-e") 'eval-buffer)

(provide 'init-lisp)
;;; init-lisp.el ends here
