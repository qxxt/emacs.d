;;; init-debugger.el --- Some debugger
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun reset-messages-buffer ()
  "Reset the *Messages* buffer."
  (interactive)
  (if-let ((msg-buf (get-buffer "*Messages*")))
      (with-current-buffer msg-buf
	(let ((buffer-read-only nil))
	  (erase-buffer)))))

(defun split-messages-window ()
  "Add small windows on the right for *Messages* buffer."
  (interactive)
  (let ((w (split-window-horizontally 90)))
      (select-window w)
      (switch-to-buffer "*Messages*")))

(defun reevaluate-init-file ()
  "Reevaluate init.el file for debugging."
  (interactive)
  (load user-init-file))

(use-package which-key
  :demand t
  :config
  (which-key-mode))

(provide 'init-debugger)
;;; init-debugger.el ends here
