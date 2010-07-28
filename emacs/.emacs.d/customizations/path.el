;; load path
(setq path (concat "/Users/brian/bin:" "/opt/local/bin:"
                   "opt/local/sbin:" "/usr/local/bin:"
                   "/usr/local/sbin:" "/bin:/usr/bin:"
                   "/usr/X11R6/bin:" "/usr/sbin:"
                   "/sbin:" "/usr/local/mysql/bin:"))

(setenv "PATH" path)

(push "/opt/local/bin" exec-path)

