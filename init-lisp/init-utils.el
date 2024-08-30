;;; init-utils.el --- Foo
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defmacro eval-when-daemon (&rest body)
  "Eval BODY when daemon."
  (declare (indent defun))
  (list 'if '(daemonp) (cons 'progn body)))

(defun eval-during-frame-start (&rest body)
  "Eval BODY when starting frame.

This to prevent BODY from being evaluated by daemon."
  (declare (indent defun))
  (add-hook 'after-make-frame-functions
	    (cons 'progn body)))

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

(defun list-difference (a b)
  (setq a (seq-uniq a)
	b (seq-uniq b))

  (seq-filter
   (lambda (x)
     (null (and (seq-contains-p a x)
		(seq-contains-p b x))))
   (seq-union a b)))

(provide 'init-utils)
;;; init-utils.el ends here
