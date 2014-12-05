;;; elm-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "elm-indent" "elm-indent.el" (21631 61334 0
;;;;;;  0))
;;; Generated autoloads from elm-indent.el

(autoload 'turn-on-elm-indent "elm-indent" "\
Turn on ``intelligent'' Elm indentation mode.

\(fn)" nil nil)

(autoload 'elm-indent-mode "elm-indent" "\
``Intelligent'' Elm indentation mode.
This deals with the layout rule of Elm.
\\[elm-indent-cycle] starts the cycle which proposes new
possibilities as long as the TAB key is pressed.  Any other key
or mouse click terminates the cycle and is interpreted except for
RET which merely exits the cycle.
Other special keys are:
    \\[elm-indent-insert-equal]
      inserts an =
    \\[elm-indent-insert-guard]
      inserts an |
    \\[elm-indent-insert-otherwise]
      inserts an | otherwise =
these functions also align the guards and rhs of the current definition
    \\[elm-indent-insert-where]
      inserts a where keyword
    \\[elm-indent-align-guards-and-rhs]
      aligns the guards and rhs of the region
    \\[elm-indent-put-region-in-literate]
      makes the region a piece of literate code in a literate script

Invokes `elm-indent-hook' if not nil.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "elm-indentation" "elm-indentation.el" (21631
;;;;;;  61334 0 0))
;;; Generated autoloads from elm-indentation.el

(autoload 'elm-indentation-mode "elm-indentation" "\
Elm indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs.  It supports
autofill-mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-elm-indentation "elm-indentation" "\
Turn on the elm-indentation minor mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "elm-mode" "elm-mode.el" (21631 61334 0 0))
;;; Generated autoloads from elm-mode.el

(add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))

(autoload 'elm-mode "elm-mode" "\
Major mode for editing Elm source code

\(fn)" t nil)

;;;***

;;;### (autoloads nil "elm-string" "elm-string.el" (21631 61334 0
;;;;;;  0))
;;; Generated autoloads from elm-string.el

(autoload 'elm-trim "elm-string" "\


\(fn STRING)" nil nil)

(autoload 'elm-string-take "elm-string" "\
Take n chars from string.

\(fn STRING N)" nil nil)

(autoload 'elm-is-prefix-of "elm-string" "\
Is x string a prefix of y string?

\(fn X Y)" nil nil)

;;;***

;;;### (autoloads nil nil ("elm-compile.el" "elm-font-lock.el" "elm-map.el"
;;;;;;  "elm-mode-pkg.el" "elm-preview.el" "elm-repl.el" "elm-util.el")
;;;;;;  (21631 61334 447435 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; elm-mode-autoloads.el ends here
