;;; init.el --- Load the full configuration
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

;; Load ~/.emacs.d/lisp directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Change saved customization settings file. This prevents clutter in
;; init.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; Load customization file, if exist.
(if (file-exists-p custom-file)
    (load custom-file))

;; Initialize package sources
(require 'package)

;; Add Melpa to package sources list
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Initialize package as `package-enable-at-startup' is set to nil in
;; early-init.el
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose t))

(setq use-package-always-ensure nil ; Ensure all use-package packages
      use-package-always-defer t ; Defer loading packages unless explicitly demanded.
      use-package-hook-name-suffix nil ; Disable suffix "-hook" for use-package's :hook
      )

(require 'init-packages)

(require 'init-interface)
(require 'init-theme)
(require 'init-buffer)
(require 'init-modeline)

(require 'init-editing-utils)
(require 'init-snippet)
(require 'init-minibuffer)
(require 'init-git)

(require 'init-treemacs) ; TODO. Finished enough, but can be improved

(require 'init-prog)
(require 'init-go)
(require 'init-lisp) ; TODO
(require 'init-org) ; TODO

(require 'init-face)

;; Reevaluate init file
(defun reevaluate-init-file ()
  "Reevaluate init.el file for debugging."
  (interactive)
  (load user-init-file))

(provide 'init)
;;; init.el ends here
