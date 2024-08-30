;;; init-tree-sitter.el --- *Lisp configurations.
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (boundp 'treesit-language-source-alist)
  (push "/usr/local/lib/" treesit-extra-load-path)
  (setq treesit-language-source-alist
        '((go "https://github.com/tree-sitter/tree-sitter-go")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript"))))

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
