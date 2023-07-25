;;; init-lisp.el --- *Lisp configurations.
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq initial-major-mode 'emacs-lisp-mode)
(setq initial-scratch-message (concat (string-trim initial-scratch-message) "\n(reset-messages-buffer)\n\n"))

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

(use-package sly
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl")
  :bind
  (:map sly-mode-map
	("C-c C-e" . sly-eval-buffer)))

(use-package lispy
  :ensure t
  :hook
  ((emacs-lisp-mode-hook . lispy-mode)
   (lisp-interaction-mode-hook . lispy-mode)))

(use-package macrostep
  :bind
  (:map emacs-lisp-mode-map
	("C-c C-=" . macrostep-mode)))

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-buffer)

(provide 'init-lisp)
;;; init-lisp.el ends here
