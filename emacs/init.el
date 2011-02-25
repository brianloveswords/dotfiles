;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; First thing, disable the gui elements we don't want
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Load path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Load personal customizations
(setq customizations-dir (concat dotfiles-dir "customizations"))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path customizations-dir)

(if (file-exists-p customizations-dir)
    (mapc #'load (directory-files customizations-dir nil ".*el$")))

(add-to-list 'load-path "~/.emacs.d/libraries/")
(add-to-list 'load-path "~/.emacs.d/libraries/remember")
(add-to-list 'load-path "~/.emacs.d/libraries/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/libraries/php-mode-1.5.0")
(add-to-list 'load-path "~/.emacs.d/libraries/yaml-mode.el")
(add-to-list 'load-path "~/.emacs.d/libraries/yasnippet-0.6.1c")

;; JavaScript mode setup
(autoload 'espresso-mode "espresso")
(autoload 'js2-mode "js2-mode" nil t)

(autoload 'python-mode "python-mode")
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))
                                                       
(setq js2-basic-offset 2)
(setq js2-use-font-lock-faces t)

;; Never use tabs, two spaces per indent
(setq c-basic-offset 2)
(setq css-indent-offset 2)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

(require 'htmlize)
(require 're-builder+)
(require 'php-mode)
(require 'yasnippet)
(require 'magit)
(require 'uniquify)
(require 'org-install)
(require 'remember)
(require 'yaml-mode)
(require 'sass-mode)
(require 'haml-mode)
(require 'ibuffer)
(require 'js2-highlight-vars)
(require 'python-mode)
(require 'gist)

;; to use curl
(setq gist-use-curl t)

(yas/initialize)
(yas/load-directory "~/.emacs.d/libraries/yasnippet-0.6.1c/snippets")

(setq yas/root-directory "~/.emacs.d/my-snippets")
(yas/load-directory yas/root-directory)

(add-to-list 'auto-mode-alist '("\\.erb$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.mustache$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))

(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.module$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.install$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . yaml-mode)) ;;weird, I know. it seems to work better

(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-helper-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

(add-to-list 'file-coding-system-alist '("\\.txt\\'" mule-utf-8 . mule-utf-8))
(add-to-list 'file-coding-system-alist '("\\.org\\'" mule-utf-8 . mule-utf-8))

;; Unique buffer names
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;; Org setup
;; (setq org-remember-templates
;;       '(("Todo" ?t "* TODO [#B] %?\n  %u" "~/Documents/Notes/todo.org" "Tasks")
;;         ("Notes" ?n "* %u %?" "~/Documents/Notes/notes.org" "Notes")))
;; (add-hook 'remember-mode-hook 'org-remember-apply-template)

;; (defun my-org-mode-cust()
;;   (turn-off-flyspell)
;;   (define-prefix-command 'org-todo-state-map)
;;   (define-key org-mode-map (kbd "C-c x") 'org-todo-state-map)
;;   (define-key org-mode-map (kbd "M-j") 'org-meta-return)
;;   (define-key org-mode-map (kbd "C-<tab>") 'other-window)
;;   (define-key org-todo-state-map "x"
;;     #'(lambda nil (interactive) (org-todo "CANCELLED")))
;;   (define-key org-todo-state-map "d"
;;     #'(lambda nil (interactive) (org-todo "DONE")))
;;   (define-key org-todo-state-map "s"
;;     #'(lambda nil (interactive) (org-todo "STARTED")))
;;   (define-key org-todo-state-map "w"
;;     #'(lambda nil (interactive) (org-todo "WAITING"))))


;; (add-hook 'org-mode-hook 'my-org-mode-cust)
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

(defun unix-newlines nil
  (interactive)
  (set-buffer-file-coding-system 'unix))

(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq espresso-indent-level 2
        indent-tabs-mode nil
        c-basic-offset 2)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map [(meta control \;)] 
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
    (js2-highlight-vars-mode))
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(defalias 'qrr 'query-replace-regexp)
(defalias 'wm 'whitespace-mode)
(defalias 'wc 'whitespace-cleanup)
(defalias 'cy 'clipboard-yank)
(defalias 'ci 'org-clock-in)
(defalias 'co 'org-clock-out)

;; smart tab
(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
the minibuffer. Else, if mark is active, indents region. Else if
point is at the end of a symbol, expands it. Else indents the
current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (dabbrev-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (dabbrev-expand nil)
        (indent-for-tab-command)))))


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
  (setq ido-create-new-buffer 'always)
  (define-key ido-file-completion-map (kbd "C-h") 'ido-delete-backward-updir)
  (define-key ido-completion-map (kbd "C-w") 'backward-kill-word))


(add-hook 'ido-setup-hook 'my-ido-mode-cust)

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

;; Highlight todos, fixmes and bugs
(add-hook 'c-mode-common-hook
               (lambda ()
                (font-lock-add-keywords nil
                  '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

(add-hook 'js2-mode-hook 
          (lambda()
            (font-lock-add-keywords nil 
              '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):" 1 font-lock-warning-face prepend)))))

(setq confirm-kill-emacs nil)
(setq display-buffer-reuse-frames nil)
(setq vc-delete-logbuf-window nil)

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(windmove-default-keybindings 'meta)

;; ediff custo.
(setq ediff-split-window-function 'split-window-horizontally)

;; for fuzzy matching
(iswitchb-mode t)
(ido-mode t)
(icomplete-mode t)

;; default to truncate lines
(setq truncate-lines 1)


(defun compile-less-css ()
  "Compile LESS to CSS"
  (interactive)
  (if (string-match "\.less$" (buffer-file-name))
      (async-shell-command (concat "lessc " (buffer-file-name) "> main.css") nil nil)))

;; (add-hook 'after-save-hook 'compile-less-css)


;; themes!
(load-file "~/.emacs.d/themes/color-theme-almost-monokai.el")
(color-theme-almost-monokai)
