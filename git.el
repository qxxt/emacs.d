(use-package magit
  :ensure t
  :demand t)

(use-package diff-hl
  :ensure t
  :demand t
  :hook ((prog-mode-hook vc-dir-mode-hook) . diff-hl-mode))

(use-package magit-delta
  :ensure t
  :demand t
  :hook (vc-dir-mode-hook . magit-delta-mode))
