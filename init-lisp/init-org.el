;;; init-org.el --- Org-mode.
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Org-mode
(use-package ob-go
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((go . t))))

(use-package org-bullets
  :hook
  (org-mode-hook . org-bullets-mode))

(provide 'init-org)
;;; init-org.el ends here
