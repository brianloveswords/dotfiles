;;; mediawiki-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (mediawiki-draft-buffer mediawiki-draft-page mediawiki-draft)
;;;;;;  "mediawiki" "mediawiki.el" (21345 14079 0 0))
;;; Generated autoloads from mediawiki.el

(autoload 'mediawiki-draft "mediawiki" "\
Open a temporary buffer in wikipedia mode for editing an wikipedia
 draft, which an arbitrary piece of data. After finishing the editing
 either use C-c C-k \\[mediawiki-draft-buffer] to send the data into
 the mediawiki-draft-data-file, or send  the buffer using C-x C-s
\\[mediawiki-save]  and insert it later into a wikipedia article.

\(fn)" t nil)

(autoload 'mediawiki-draft-page "mediawiki" "\


\(fn)" t nil)

(autoload 'mediawiki-draft-buffer "mediawiki" "\
Mediawiki-draft-buffer sends the contents of the current (temporary)
buffer to the mediawiki-draft-buffer, see the variable
mediawiki-draft-data-file.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("mediawiki-pkg.el") (21345 14079 681926
;;;;;;  0))

;;;***

(provide 'mediawiki-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mediawiki-autoloads.el ends here
