;;; init-debugger.el --- Some debugger
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun reset-messages-buffer()
  "Reset the *Messages* buffer."
  (interactive)
  (with-current-buffer "*Messages*"
    (let ((buffer-read-only nil))
      (erase-buffer))))

;; Reevaluate init file
(defun reevaluate-init-file ()
  "Reevaluate init.el file for debugging."
  (interactive)
  (load user-init-file))

(provide 'init-debugger)
;;; init-debugger.el ends here
