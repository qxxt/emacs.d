;;; init-packages.el --- Packages to install. And elpa related configurations.
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'packages-upgrade-interactive)

(setq package-refresh-interval 2)
(pui--scheduler "07:00pm")

(provide 'init-packages)
;;; init-packages.el ends here
