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

;; Check if `use-package' is exist and checks if
;; `package-archive-contents' is empty. Update `package-archive' and
;; install `use-package' if they aren't true isn't.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose t))

(setq use-package-always-ensure t ; Ensure all use-package packages
      use-package-always-defer t ; Defer loading packages unless explicitly demanded.
      use-package-hook-name-suffix nil ; Disable suffix "-hook" for use-package's :hook
      )

(require 'init-interface)
(require 'init-theme)
(require 'init-editing-utils)
(require 'init-minibuffer)
(require 'init-packages)
(require 'init-git)

(require 'init-treemacs)

(require 'init-prog)
(require 'init-go)
(require 'init-lisp)
(require 'init-org)

(require 'init-face)

;; Reevaluate init file
(defun reevaluate-init-file ()
  "Reevaluate init.el file for debugging."
  (interactive)
  (load user-init-file))

(provide 'init)
;;; init.el ends here
