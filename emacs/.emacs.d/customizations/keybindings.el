;; Key Bindings Section
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "C-x f"))
(global-unset-key (kbd "C-x d"))
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "M-["))

(global-set-key (kbd "<f5>") 'call-last-kbd-macro)
(global-set-key (kbd "<f11>") 'aquamacs-toggle-full-frame)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-o") 'split-line)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-\\") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-=") 'ispell-word)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-#") 'ispell-buffer)
(global-set-key (kbd "M-s") 'isearch-forward)
(global-set-key (kbd "M-r") 'isearch-backward)
(global-set-key (kbd "M-{") 'previous-buffer)
(global-set-key (kbd "M-}") 'next-buffer)
(global-set-key (kbd "M-o") 'swap-window-buffers)
(global-set-key (kbd "C-S-<tab>") #'(lambda nil (interactive)(other-window -1)))
(global-set-key (kbd "C-M-o") 'switch-to-other-buffer)
(global-set-key (kbd "C-M-=") 'enlarge-window)
(global-set-key (kbd "C-M--") 'shrink-window)
(global-set-key (kbd "C-M-w") 'backward-kill-sexp)
(global-set-key (kbd "C-M-q") 'kill-other-buffer)
(global-set-key (kbd "C-x e") 'delete-horizontal-space)
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-x m") 'magit-status)
(global-set-key (kbd "C-x t") 'jao-toggle-selective-display)
(global-set-key (kbd "C-x <tab>") 'indent-region)
(global-set-key (kbd "C-x f =") 'diff-buffer-with-file)
(global-set-key (kbd "C-x f d") 'dired)
(global-set-key (kbd "C-x f r") 'revert-buffer)
(global-set-key (kbd "C-x C-d") 'ido-find-file-in-tag-files)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-o") 'occur)
(global-set-key (kbd "C-x C-p") 'eval-print-last-sexp)
(global-set-key (kbd "C-x C-<tab>") 'indent-rigidly)
(global-set-key (kbd "C-c C-c") 'execute-extended-command)
(global-set-key (kbd "C-c C-e") 'eval-last-sexp)
(global-set-key (kbd "C-c C-p") 'eval-print-last-sexp)