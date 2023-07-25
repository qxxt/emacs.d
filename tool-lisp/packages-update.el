;;; packages-update.el --- Simple tools for upgrading emacs packages
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)
(require 'time-date)

(defcustom package-update-interval 2
  "DAYS until `package-refresh-contents'."
  :type 'number
  :group 'package)

(defun package-refresh-contents? ()
  "Refresh package with prompt if `package-update-interval' days has passed."
  (if (and (or (not package-archive-contents)
	       (<= package-update-interval
		   (time-to-number-of-days
		    (time-since
		     (file-attribute-modification-time
		      (file-attributes
		       (concat package-user-dir "/archives/gnu/archive-contents")))))))
	   (y-or-n-p "Refresh packages now?"))
      (package-refresh-contents)))

(defun package-upgrades? ()
  "Upgrade packages with prompt."
  (interactive)
  (package-refresh-contents?)
  (let ((upgradable-packages (package--upgradeable-packages)))
    (if (null upgradable-packages)
	(message "All packages are up-to-date")
      (with-temp-buffer
	(unwind-protect
	    (progn
	      (rename-buffer "*Packages to update*")
	      (switch-to-buffer-other-window (current-buffer))
	      (insert "Package to update:\n")
	      (insert (mapconcat #'symbol-name upgradable-packages "\n"))
	      (y-or-n-p "Update these packages now?")
	      (mapc #'package-upgrade upgradable-packages))
	  (delete-window))))))

(provide 'packages-update)
;;; packages-update.el ends here
