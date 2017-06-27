(setenv "GOPATH" (concat (getenv "HOME") "src/go"))
(add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))

(use-package go-mode
  :ensure t)

(use-package go-eldoc
  :ensure t)

(defun go-mode-setup ()
;;  (require 'go-mode-autoloads)
  (require 'go-mode)

  (go-eldoc-setup)
  (setq compile-command "go build -v && go test -v && go vet")
  (define-key (current-local-map) "\C-c\C-c" 'compile)

  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")

  (local-set-key (kbd "M-.") 'godef-jump))

(add-hook 'go-mode-hook 'go-mode-setup)

(provide 'go)
