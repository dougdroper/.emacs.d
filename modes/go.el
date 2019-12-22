(use-package go-mode
  :mode "\\.go$"
  :bind (:map go-mode-map
	      ("<s-return>" . gofmt)))

(use-package go-eldoc
  :after go-mode
  :hook (go-mode . go-eldoc-setup))
  
(provide 'go)
