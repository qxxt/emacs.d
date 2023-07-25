;;; init-formatter.el --- Default configurations for prog-mode.
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun format-current-buffer (command &optional args)
  "Asd COMMAND dsadsa ARGS."
  (and
   (stringp command)
   (stringp args)
   (executable-find command))
  (message "SADdsa"))

(message (file-name-nondirectory buffer-file-name))

(provide 'init-formatter)
;;; init-formatter.el ends here
