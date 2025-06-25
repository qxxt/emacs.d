;;; init.el --- Load the full configuration
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Produce backtraces when errors occur
(setq debug-on-error t)

;; Load ~/.emacs.d/lisp directory
(add-to-list 'load-path (expand-file-name "init-lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tool-lisp" user-emacs-directory))

;; Change saved customization settings file to prevent cluttering init.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

;; Initialize package sources
(require 'package)

;; Add Melpa to package sources list
;; Using `push’ adds melpa to the beginning of the list,
;; prioritizing it over other package sources.
(push '("melpa" . "https://melpa.org/packages/") package-archives)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose t
        use-package-always-ensure t
        ;; use-package-always-defer (not (daemonp)) ; Defer if not run on daemon.
        use-package-hook-name-suffix nil
        use-package-compute-statistics t))

(require 'init-utils)

(require 'init-packages)
(require 'init-interface)
(require 'init-theme)
(require 'init-buffer)
(require 'init-modeline)
(require 'init-debugger)
(require 'init-tramp)

(require 'init-tree-sitter)
(require 'init-editing-utils)
(require 'init-snippet)
(require 'init-minibuffer)
(require 'init-git)

(require 'init-treemacs) ; TODO. Finished enough, but can be improved

(require 'init-prog)
(require 'init-go)
(require 'init-lisp)                    ; TODO
(require 'init-org)                     ; TODO
(require 'init-python)                  ; TODO

(require 'init-face)

;; (use-package-report)
(setq package-selected-packages (append package-selected-packages
                                        (hash-table-keys use-package-statistics)))

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
(put 'list-threads 'disabled nil)
