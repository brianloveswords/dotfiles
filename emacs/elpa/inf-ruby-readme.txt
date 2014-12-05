inf-ruby provides a REPL buffer connected to a Ruby subprocess.

If you're installing manually, you'll need to:
* drop the file somewhere on your load path (perhaps ~/.emacs.d)
* Add the following lines to your .emacs file:

   (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
   (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

Or, for enh-ruby-mode:

   (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

Installation via ELPA interface does the above for you
automatically.

Additionally, consider adding

   (add-hook 'after-init-hook 'inf-ruby-switch-setup)

to your init file to easily switch from common Ruby compilation
modes to interact with a debugger.
