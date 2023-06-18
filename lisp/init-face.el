;;; init-face.el --- Faces config.
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(custom-set-faces
 '(flymake-error ((t (:foreground "red" :weight bold))))
 '(mode-line ((t (:background "#9CCC65" :foreground "#424242" :box nil))))
 '(mode-line-buffer-id ((t (:foreground "#212121" :weight bold))))
 '(mode-line-inactive ((t (:background "#C5E1A5" :foreground "#424242" :box nil))))
 '(show-paren-match ((t (:background "blue" :foreground "white" :weight ultra-bold))))
 '(show-paren-mismatch ((t (:background "red" :foreground "white" :weight ultra-bold))))
 '(eglot-diagnostic-tag-unnecessary-face ((t (:underline (:color "red" :style wave))))))

(provide 'init-face)
;;; init-face.el ends here
