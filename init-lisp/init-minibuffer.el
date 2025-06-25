;;; init-minibuffer.el --- Minibuffer.
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(savehist-mode)

(use-package helm
  :config
  (helm-mode)
  :bind (:map global-map
              ("M-x" . helm-M-x)
              ("C-x C-f" . helm-find-files)
              ("C-x r b" . helm-filtered-bookmarks)))


(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
