;;; prolog-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (run-prolog mercury-mode prolog-mode) "prolog"
;;;;;;  "prolog.el" (21157 15884 0 0))
;;; Generated autoloads from prolog.el

(autoload 'prolog-mode "prolog" "\
Major mode for editing Prolog code.

Blank lines and `%%...' separate paragraphs.  `%'s starts a comment
line and comments can also be enclosed in /* ... */.

If an optional argument SYSTEM is non-nil, set up mode for the given system.

To find out what version of Prolog mode you are running, enter
`\\[prolog-mode-version]'.

Commands:
\\{prolog-mode-map}
Entry to this mode calls the value of `prolog-mode-hook'
if that value is non-nil.

\(fn &optional SYSTEM)" t nil)

(autoload 'mercury-mode "prolog" "\
Major mode for editing Mercury programs.
Actually this is just customized `prolog-mode'.

\(fn)" t nil)

(autoload 'run-prolog "prolog" "\
Run an inferior Prolog process, input and output via buffer *prolog*.
With prefix argument ARG, restart the Prolog process if running before.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("prolog-pkg.el") (21157 15884 458553 0))

;;;***

(provide 'prolog-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; prolog-autoloads.el ends here
