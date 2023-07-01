;;; init-lisp.el --- *Lisp configurations.
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(define-advice elisp-get-fnsym-args-string (:around (func sym &rest r) docstring)
  "If SYM is a function, append its docstring."
  (concat
   (apply func sym r)
   (let* ((doc (and (fboundp sym) (documentation sym 'raw)))
          (oneline (and doc (substring doc 0 (string-match "\n" doc)))))
     (and oneline
          (stringp oneline)
          (not (string= "" oneline))
          (concat "\n" oneline)))))

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl"))

(add-hook 'emacs-lisp-mode-hook #'flymake-mode)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-buffer)

(provide 'init-lisp)
;;; init-lisp.el ends here
