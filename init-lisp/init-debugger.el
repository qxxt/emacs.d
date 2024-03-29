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

;; Reevaluate init file
(defun reevaluate-init-file ()
  "Reevaluate init.el file for debugging."
  (interactive)
  (load user-init-file))

(use-package which-key
  :config
  (which-key-mode))

(provide 'init-debugger)
;;; init-debugger.el ends here
