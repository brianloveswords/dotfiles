;; don't show the splash screen
(setq inhibit-splash-screen t)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; First thing, disable the gui elements we don't want
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path & customizations
(setq dotfiles-dir
      (file-name-directory (or (buffer-file-name) load-file-name)))
(setq customizations-dir (concat dotfiles-dir "customizations"))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path customizations-dir)
(if (file-exists-p customizations-dir)
    (mapc #'load (directory-files customizations-dir nil ".*el$")))

(add-to-list 'load-path "~/.emacs.d/vendor/")
(add-to-list 'load-path "~/.emacs.d/vendor/remember")
(add-to-list 'load-path "~/.emacs.d/vendor/tern/emacs")
(add-to-list 'load-path "~/.emacs.d/vendor/haskell-mode-2.8.0")
(add-to-list 'load-path "~/.emacs.d/vendor/org-8.0.1/lisp")
(add-to-list 'load-path "~/.emacs.d/vendor/yaml-mode.el")
(add-to-list 'load-path "~/.emacs.d/vendor/magit-1.2.0")
(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet-0.6.1c")

;; JavaScript mode setup
(autoload 'espresso-mode "espresso")
(autoload 'js2-mode "js2-mode" nil t)
(autoload 'python-mode "python-mode")
(autoload 'markdown-mode "markdown-mode.el" nil t)
(autoload 'tern-mode "tern.el" nil t)

(setq js2-use-font-lock-faces t)

;; Indentation related bidness
(setq js2-basic-offset 2)
(setq c-basic-offset 2)
(setq css-indent-offset 2)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(setq default-tab-width 2)

(defun toggle-tabs nil
  "Toggle tabs v spaces"
  (interactive)
  (setq indent-tabs-mode (if indent-tabs-mode nil 1)))

(require 'magit)
(require 'uniquify)
(require 'yaml-mode)
(require 'ibuffer)
(require 'js2-highlight-vars)
(require 'puppet-mode)
(require 'mustache-mode)
(require 'clojure-mode)
(require 'go-mode-load)
(require 'column-marker)
(require 'recentf)
(require 'yasnippet)

(yas/initialize)
(setq yas/root-directory "~/.emacs.d/my-snippets")
(yas/load-directory yas/root-directory)

(add-to-list 'auto-mode-alist '("\\.erb$"        . html-mode))
(add-to-list 'auto-mode-alist '("\\.mustache$"   . html-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$"  . html-mode))
(add-to-list 'auto-mode-alist '("\\.html$"       . html-mode))
(add-to-list 'auto-mode-alist '("\\.ejs$"        . html-mode))
(add-to-list 'auto-mode-alist '("\\.ru$"         . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rjs$"        . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$"    . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$"    . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile"        . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$"      . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$"   . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.inc$"        . php-mode))
(add-to-list 'auto-mode-alist '("\\.module$"     . php-mode))
(add-to-list 'auto-mode-alist '("\\.install$"    . php-mode))
(add-to-list 'auto-mode-alist '("\\.php$"        . php-mode))
(add-to-list 'auto-mode-alist '("fabfile$"       . python-mode))
(add-to-list 'auto-mode-alist '("wscript$"       . python-mode))
(add-to-list 'auto-mode-alist '("\\.json$"       . yaml-mode)) ;;weird, I know. it seems to work better
(add-to-list 'auto-mode-alist '("\\.yml$"        . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.js$"         . js2-mode))
(add-to-list 'auto-mode-alist '("\\.??sh$"       . sh-mode))
(add-to-list 'auto-mode-alist '("\\.pp$"         . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.plan$"       . org-mode))
(add-to-list 'auto-mode-alist '("\\.notes$"      . org-mode))
(add-to-list 'auto-mode-alist '("\\.m$"          . objc-mode))
(add-to-list 'auto-mode-alist '("\\.scss$"       . css-mode))
(add-to-list 'auto-mode-alist '("\\.md$"         . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.hogan.js$"   . mustache-mode))

(add-to-list 'file-coding-system-alist '("\\.txt\\'" mule-utf-8 . mule-utf-8))
(add-to-list 'file-coding-system-alist '("\\.org\\'" mule-utf-8 . mule-utf-8))

(setq visible-bell t)

;; Unique buffer names
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

(add-hook 'js2-mode-hook (lambda () (subword-mode 1)))
(add-hook 'markdown-mode-hook
          (lambda ()
            (longlines-mode 1)
            (flyspell-mode 1)))
(add-hook 'text-mode-hook 'turn-off-flyspell)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aquamacs related customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make the command key act as meta.
(setq mac-command-modifier 'meta)

;; open *help* in current frame.
(setq special-display-regexps (remove "[ ]?\\*[hH]elp.*" special-display-regexps))

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

(defun run-node-file ()
  (interactive)
  (save-buffer)
  (async-shell-command (concat "node " (buffer-file-name))))

(defun my-ido-mode-cust()
  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
  (define-key ido-file-completion-map (kbd "C-h") 'ido-delete-backward-updir)
  (define-key ido-completion-map (kbd "C-w") 'backward-kill-word))

(add-hook 'ido-setup-hook 'my-ido-mode-cust)

;; Clean up of all those wacky backup files
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/autosaves/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)


;; text and fill mode
(setq default-fill-column 72)


(unless (featurep 'xemacs)
  (provide 'emacs))

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
(defun my-todo-highlighter ()
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):" 1 font-lock-warning-face prepend))))

(add-hook 'c-mode-common-hook 'my-todo-highlighter)
(add-hook 'php-mode-hook 'my-todo-highlighter)
(add-hook 'js2-mode-hook 'my-todo-highlighter)

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; this allows for cmd+arrow key to switch windows
(windmove-default-keybindings 'meta)

(iswitchb-mode t)
(ido-mode t)
(icomplete-mode t)
(show-paren-mode 1)
(global-hl-line-mode 1)
(column-number-mode 1)
(delete-selection-mode 1)
(global-linum-mode 1)
(recentf-mode 1)

(setq confirm-kill-emacs nil)
(setq display-buffer-reuse-frames nil)
(setq vc-delete-logbuf-window nil)
(setq whitespace-line-column 72)
(setq truncate-lines 1)
(setq show-paren-style 'expression)
(setq ediff-split-window-function 'split-window-horizontally)

;; themes!
(load-file "~/.emacs.d/themes/color-theme-almost-monokai.el")
(color-theme-almost-monokai)
