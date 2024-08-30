;;; init-buffer.el --- Buffer.
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom buffer-skip-regexp
  (rx bos (or (or "*Backtrace*" "*Compile-Log*" "*Completions*"
                  "*Messages*" "*package*" "*Warnings*"
                  "*Async-native-compile-log*"
                  "*GNU Emacs*" "*Help*")
              (seq "*EGLOT" (one-or-more anything))
              (seq "magit" (one-or-more anything))
              (seq "*Flymake" (one-or-more anything)))
      eos)
  "Regular expression matching buffers ignored by `next-buffer' and `previous-buffer'."
  :type 'regexp)

(defun buffer-skip-p (window buffer bury-or-kill)
  "Return t if BUFFER name matches `buffer-skip-regexp'."
  (string-match-p buffer-skip-regexp (buffer-name buffer)))

(setq switch-to-prev-buffer-skip 'buffer-skip-p)

(provide 'init-buffer)
;;; init-buffer.el ends here
