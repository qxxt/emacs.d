;;; init-snippet.el --- Tempel
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :config
  ;; Adds personal snippets to snippet list.
  ;; (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))

  ;; Reload snippets.
  (yas-reload-all)

  :hook
  ((prog-mode-hook org-mode-hook) . yas-minor-mode))

(provide 'init-snippet)
;;; init-snippet.el ends here
