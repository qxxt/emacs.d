;;; init-tramp.el --- *Lisp configurations.
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(if (boundp 'tramp-remote-path)
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-remote-path (list 'tramp-own-remote-path)))

(provide 'init-tramp)
;;; init-tramp.el ends here
