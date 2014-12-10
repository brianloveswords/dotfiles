;; load path
(setenv "GOPATH" "/usr/local/gocode")
(add-hook 'after-init-hook #'(lambda () (exec-path-from-shell-initialize)))
