(setq path (concat "/Users/brian/bin:" "/opt/local/bin:"
                   "opt/local/sbin:" "/usr/local/bin:"
                   "/usr/local/sbin:" "/bin:/usr/bin:"
                   "/usr/X11R6/bin:" "/usr/sbin:"
                   "/sbin:" "/usr/local/mysql/bin:"))

(setenv "PATH" path)

(push "/opt/local/bin" exec-path)

(defconst aquamacs
  (boundp 'aquamacs-version)
  "Are we running Aquamacs on OS X?")


;; Unique buffer names
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;; Load stuff
(add-to-list 'load-path "~/.emacs.d/")

(load-file "~/.emacs.d/load-directory.el")

;; Load personal customizations 
(mapcar 'load-directory '("~/.emacs.d/customizations"))


(add-to-list 'load-path "~/.emacs.d/libraries/")
(add-to-list 'load-path "~/.emacs.d/customizations/")


(add-to-list 'load-path "~/.emacs.d/libraries/muse/lisp")
(add-to-list 'load-path "~/.emacs.d/libraries/remember")
(add-to-list 'load-path "~/.emacs.d/libraries/g-client")
(add-to-list 'load-path "~/.emacs.d/libraries/js2")
(add-to-list 'load-path "~/.emacs.d/libraries/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/libraries/ecb-2.40")
(add-to-list 'load-path "~/.emacs.d/libraries/php-mode-1.5.0")
(add-to-list 'load-path "~/.emacs.d/libraries/yaml-mode.el")
(add-to-list 'load-path "~/.emacs.d/libraries/ack.el")
(add-to-list 'load-path "~/.emacs.d/libraries/yasnippet-0.6.1c")
(add-to-list 'load-path "~/.emacs.d/libraries/psvn.el")

;; CEDET STUFF
;; See cedet/common/cedet.info for configuration details.
(load-file "~/.emacs.d/libraries/cedet-1.0pre6/common/cedet.el")

;; Never use tabs, two spaces per indent
(setq c-basic-offset 2)
(setq css-indent-offset 2)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)


;; Enable EDE (Project Management) features
;; (global-ede-mode 1)

;; Enable EDE for a pre-existing C++ project
;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")


;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
;; (semantic-load-enable-code-helpers)
;; (semantic-load-enable-excessive-code-helpers)
;; (semantic-load-enable-semantic-debugging-helpers)

;; * This enables even more coding tools such as intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
(semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberent ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)

(require 'htmlize)
(require 're-builder+)
(require 'ecb)
(require 'ecb-autoloads)
(require 'window-numbering)
(require 'tempo-snippets)
(require 'php-mode)
(require 'yasnippet)
(require 'magit)

(yas/initialize)
(yas/load-directory "~/.emacs.d/libraries/yasnippet-0.6.1c/snippets")

(setq yas/root-directory "~/.emacs.d/my-snippets")
(yas/load-directory yas/root-directory)

(window-numbering-mode 1)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.module$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'file-coding-system-alist '("\\.txt\\'" mule-utf-8 . mule-utf-8))
(add-to-list 'file-coding-system-alist '("\\.org\\'" mule-utf-8 . mule-utf-8))

;; Org setup
(require 'org-install)
(define-key global-map (kbd "\C-c l") 'org-store-link)
(define-key global-map (kbd "\C-c a") 'org-agenda)
(define-key mode-specific-map [?a] 'org-agenda)

(require 'remember)
(setq org-remember-templates
      '(("Todo" ?t "* TODO [#B] %?\n  %u" "~/Documents/Notes/todo.org" "Tasks")
        ("Notes" ?n "* %u %?" "~/Documents/Notes/notes.org" "Notes")))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map (kbd "C-M-r") 'org-remember)

(defun my-org-mode-cust()
  (turn-off-flyspell)
  (define-prefix-command 'org-todo-state-map)
  (define-key org-mode-map (kbd "C-c x") 'org-todo-state-map)
  (define-key org-mode-map (kbd "M-j") 'org-meta-return)
  (define-key org-mode-map (kbd "C-<tab>") 'other-window)
  (define-key org-todo-state-map "x"
    #'(lambda nil (interactive) (org-todo "CANCELLED")))
  (define-key org-todo-state-map "d"
    #'(lambda nil (interactive) (org-todo "DONE")))
  (define-key org-todo-state-map "s"
    #'(lambda nil (interactive) (org-todo "STARTED")))
  (define-key org-todo-state-map "w"
    #'(lambda nil (interactive) (org-todo "WAITING"))))

;; JavaScript mode setup
(autoload 'js2-mode "js2" nil t)
(setq js2-basic-offset 2)
(setq js2-use-font-lock-faces t)

;; eshell setup
;; (add-hook 'eshell-mode-hook
;;           '(lambda nil
;;              (eshell/export "PATH=`$HOME`/bin:/opt/local/bin:opt/local/sbin:/usr/local/bin:/usr/local/sbin:/bin:/usr/bin:/usr/X11R6/bin:/usr/sbin:/sbin:/usr/local/mysql/bin:`$PATH`")
;;              (local-set-key "\C-u" 'eshell-kill-input)))

(defun my-c-mode-cust()
  (setq c-basic-offset 2))

(add-hook 'c-mode-hook 'my-c-mode-cust)
(add-hook 'org-mode-hook 'my-org-mode-cust)
(add-hook 'text-mode-hook 'turn-off-flyspell)
(add-hook 'latex-mode-hook #'(lambda nil (interactive) (turn-off-flyspell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aquamacs related customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make the command key act as meta.
(setq mac-command-modifier 'meta)

;; open *help* in current frame.
(setq special-display-regexps (remove "[ ]?\\*[hH]elp.*" special-display-regexps))

(defvar backup-dir
  (concat "/Users/" (user-login-name) "/.emacs.d/emacs-backups/"))

(setq backup-directory-alist
      (list
       (cons "." backup-dir)))

;; Custom functions
(defun jao-toggle-selective-display nil
  "Toggle a primitive form of code folding."
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(defun switch-to-other-buffer nil
  "Switch to the buffer on top of the buffer list"
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun swap-window-buffers nil
  "Swap buffer in current window for buffer in other window"
  (interactive)
  (let ((this-buffer (current-buffer))
        (that-buffer (window-buffer (next-window))))
    (list this-buffer that-buffer)
    (switch-to-buffer that-buffer)
    (switch-to-buffer-other-window this-buffer)))

(defalias 'qrr 'query-replace-regexp)
(defalias 'ws 'whitespace-mode)

(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(define-key completion-list-mode-map (kbd "q") 'delete-completion-window)


;; Ido mode
(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(defun my-ido-mode-cust()
  (setq ido-enable-flex-matching t)
  (define-key ido-file-completion-map (kbd "C-h") 'ido-delete-backward-updir)
  (define-key ido-completion-map (kbd "C-w") 'backward-kill-word))

(add-hook 'ido-setup-hook 'my-ido-mode-cust)


;; Dired mode
(defun my-dired-mode-cust()
  (define-key dired-mode-map (kbd "M-{") 'previous-buffer)
  (define-key dired-mode-map (kbd "M-}") 'next-buffer)
  (define-key dired-mode-map (kbd "C-x C-q") 'wdired-change-to-wdired-mode))

(add-hook 'dired-mode-hook 'my-dired-mode-cust)

;; Python mode customization
(add-hook 'python-mode-hook
          (lambda nil
            (define-key python-mode-map (kbd "C-h") 'python-backspace)))

;; Clean up of all those wacky backup files
(setq backup-by-copying t                 ;; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs.d/autosaves/")) ;; don't litter
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)


;; text and fill mode
(setq default-fill-column 78)

(unless (featurep 'xemacs)
  (provide 'emacs))

(when (and (featurep 'emacs) (load "~/nxml-mode/rng-auto.el" t))
  (defalias 'html-mode 'nxml-mode)
  (defalias 'xml-mode 'nxml-mode)
  (defalias 'html-helper-mode 'nxml-mode))

;; (setq magic-mode-alist ())

;; css-mode modifiers.
(defun css-insert-bracket ()
  '((self-insert-command)
    (indent-for-tab-command)))

(setq cssm-indent-function #'cssm-c-style-indenter)
(add-hook 'css-mode-hook
          '(lambda ()
             (define-key cssm-mode-map
               (read-kbd-macro "}")
               'css-insert-bracket)))

(setq cssm-mirror-mode nil)


;; I could do this better.
(when aquamacs
  (one-buffer-one-frame-mode 0)
  (setq confirm-kill-emacs 'y-or-n-p)
;;  (custom-set-variables '(aquamacs-styles-mode t))
  )


(defun clean-php-mode ()
  (setq c-basic-offset 2) ; 2 tabs indenting
  (setq indent-tabs-mode nil) ; spaces, not tabs
  (setq-mode-local indent-tabs-mode nil)
  (setq fill-column 78)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-close 'c-lineup-arglist-operators)
  (c-set-offset 'arglist-intro '+) ; for FAPI arrays and DBTNG
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-math) ;for DBTNG fields and values
  (fset 'yes-or-no-p 'y-or-n-p))

(add-hook 'php-mode-hook 'clean-php-mode)

(setq confirm-kill-emacs nil)
(setq display-buffer-reuse-frames nil)
(setq vc-delete-logbuf-window nil)

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(server-start)
(windmove-default-keybindings 'meta)

;; for fuzzy matching
(iswitchb-mode t)
(ido-mode t)
(icomplete-mode t)