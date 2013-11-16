;; Some custom functions

(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)

(defun insert-black-star ()
  "Insert the literal ★ character into the buffer"
  (interactive)
  (insert-char 9733 1))

(defun other-window-backward (&optional n)
  "Select the Nth previous window"
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(defun scroll-n-lines-ahead (&optional n)
  "Scroll ahead N lines (1 by default)"
  (interactive "P")
  (scroll-ahead (prefix-numeric-value n)))

(defun scroll-n-lines-behind (&optional n)
  "Scroll behind N lines (1 by default)"
  (interactive "P")
  (scroll-behind (prefix-numeric-value n)))

(defun occur-from-isearch ()
  "Perform an occur from the text of an isearch"
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string
             (regexp-quote isearch-string)))))

;; Key Bindings Section
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "C-x f"))
(global-unset-key (kbd "C-x d"))
(global-unset-key (kbd "C-x w"))
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "C-x C-r"))
(global-unset-key (kbd "M-["))
(global-unset-key (kbd "M-`"))

(global-set-key (kbd "<f5>") 'call-last-kbd-macro)
(global-set-key (kbd "<f11>") 'aquamacs-toggle-full-frame)
(global-set-key (kbd "<f12>") 'whitespace-cleanup)
(global-set-key (kbd "<f15>") 'rgrep)
(global-set-key (kbd "<up>") 'previous-line)
(global-set-key (kbd "<down>") 'next-line)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-o") 'split-line)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-\\") 'comment-or-uncomment-region)
(global-set-key (kbd "C--") 'er/contract-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-<return>") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-right)
(global-set-key (kbd "M-3") 'split-window-below)
(global-set-key (kbd "M-#") 'ispell-buffer)
(global-set-key (kbd "M-s") 'query-replace-regexp)
(global-set-key (kbd "M-r") 'query-replace-regexp)
(global-set-key (kbd "M-*") 'insert-black-star)
(global-set-key (kbd "M-N") 'mc/mark-next-like-this)
(global-set-key (kbd "M-P") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-M") 'magit-status)
(global-set-key (kbd "M-B") 'ido-switch-buffer)
(global-set-key (kbd "M-K") 'kill-buffer)
(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "C-S-<tab>") 'other-window-backward)
(global-set-key (kbd "M--") 'er/contract-region)
(global-set-key (kbd "M-=") 'er/expand-region)
(global-set-key (kbd "M-`") 'ibuffer)
(global-set-key (kbd "C-M-s") 'rgrep)
(global-set-key (kbd "C-M-o") 'switch-to-other-buffer)
(global-set-key (kbd "C-M-r") 'recentf-open-files)
(global-set-key (kbd "C-M-w") 'backward-kill-sexp)
(global-set-key (kbd "C-M-=") 'enlarge-window)
(global-set-key (kbd "C-M--") 'shrink-window)
(global-set-key (kbd "C-c i") 'org-clock-in)
(global-set-key (kbd "C-c o") 'org-clock-out)
(global-set-key (kbd "C-x e") 'delete-horizontal-space)
(global-set-key (kbd "C-x t") 'jao-toggle-selective-display)
(global-set-key (kbd "C-x <tab>") 'indent-region)
(global-set-key (kbd "C-x f =") 'diff-buffer-with-file)
(global-set-key (kbd "C-x f d") 'dired)
(global-set-key (kbd "C-x f r") 'revert-buffer)
(global-set-key (kbd "C-x w m") 'whitespace-mode)
(global-set-key (kbd "C-x w c") 'whitespace-cleanup)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x C-l") 'goto-line)
(global-set-key (kbd "C-x C-o") 'occur)
(global-set-key (kbd "C-x C-p") 'eval-print-last-sexp)
(global-set-key (kbd "C-x C-<tab>") 'indent-rigidly)
(global-set-key (kbd "C-c C-c") 'execute-extended-command)
(global-set-key (kbd "C-c C-e") 'eval-last-sexp)
(global-set-key (kbd "C-c C-p") 'eval-print-last-sexp)
(global-set-key (kbd "C-x C-r C-s") 'copy-to-register)
(global-set-key (kbd "C-x C-r C-y") 'insert-register)
(global-set-key (kbd "C-x C-r C-i") 'insert-register)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(define-key mode-specific-map [?a] 'org-agenda)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "C-o") 'occur-from-isearch)
(define-key completion-list-mode-map (kbd "q") 'delete-completion-window)
(define-key emacs-lisp-mode-map (kbd "M-J") 'eval-print-last-sexp)

(eval-after-load 'dired
  (lambda nil
    (define-key dired-mode-map (kbd "C-x C-q") 'wdired-change-to-wdired-mode)))

;; js2-mode specific
(add-hook 'js2-mode-hook
 (lambda nil
   (local-set-key (kbd "M-+") 'run-node-file)))
