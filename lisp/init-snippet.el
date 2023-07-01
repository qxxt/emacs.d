;;; init-snippet.el --- Tempel
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package tempel
;;   ;; Require trigger prefix before template name when completing.
;;   :custom
;;   (tempel-trigger-prefix "<")

;;   :bind (
;;               ("M-+" . tempel-complete) ;; Alternative tempel-expand
;;               ("M-*" . tempel-insert))

;;   :init

;;   ;; Setup completion at point
;;   (defun tempel-setup-capf ()
;;     ;; Add the Tempel Capf to `completion-at-point-functions'.
;;     ;; `tempel-expand' only triggers on exact matches. Alternatively use
;;     ;; `tempel-complete' if you want to see all matches, but then you
;;     ;; should also configure `tempel-trigger-prefix', such that Tempel
;;     ;; does not trigger too often when you don't expect it. NOTE: We add
;;     ;; `tempel-expand' *before* the main programming mode Capf, such
;;     ;; that it will be tried first.
;;     (setq-local completion-at-point-functions
;;                 (cons #'tempel-expand
;;                       completion-at-point-functions)))

;;   (add-hook 'conf-mode-hook 'tempel-setup-capf)
;;   (add-hook 'prog-mode-hook 'tempel-setup-capf)
;;   (add-hook 'text-mode-hook 'tempel-setup-capf)

;;   ;; Optionally make the Tempel templates available to Abbrev,
;;   ;; either locally or globally. `expand-abbrev' is bound to C-x '.
;;   ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
;;   ;; (global-tempel-abbrev-mode)
;; )

;; ;; Load ~/.emacs.d/lisp directory
;; (let ((tempel-el (expand-file-name "tempel-collection/tempel-collection.el" user-emacs-directory)))
;;   (when (file-exists-p tempel-el)
;;     (load tempel-el)))

;; (use-package lsp-snippet-tempel
;;   :after (eglot tempel)
;;   :load-path "lsp-snippet"
;;   :hook
;;   (eglot-managed-mode-hook . lsp-snippet-tempel-eglot-init))

(use-package yasnippet
  :ensure t
  :demand t
  :delight
  :config
  ;; Adds personal snippets to snippet list.
  ;; (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))

  ;; Reload snippets.
  (yas-reload-all)

  :hook
  ((prog-mode-hook org-mode-hook) . yas-minor-mode))

(provide 'init-snippet)
;;; init-snippet.el ends here
