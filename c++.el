;; C++ Stuffs
(defun my-c++-mode-hook ()
  (google-set-c-style)
  (google-make-newline-indent))

(add-hook 'c++-mode-hook
          (lambda ()
            (flycheck-mode)
            (my-c++-mode-hook)
            (editorconfig-mode)))

(provide 'c++)
