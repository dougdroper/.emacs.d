;;; ruby.el --- Ruby specific configuration

;;; Commentary:
;;

;;; Code:

(use-package ruby-mode
  :mode "\\.rb$")

(use-package enh-ruby-mode
  :hook (ruby-mode . enh-ruby-mode) )

(use-package haml-mode
  :mode "\\.haml$")

(use-package rspec-mode
  )

(use-package inf-ruby
  :hook (after-init . inf-ruby-switch-setup)
  :bind (:map inf-ruby-mode-map
              ("C-c i" . read-only-mode)))

(provide 'ruby)
;;; ruby.el ends here
