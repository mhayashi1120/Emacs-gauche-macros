# Emacs-gauche-macros

Import [Gauche](https://practical-scheme.net/gauche/) macros to elisp with changing some conventions. Just import :heartbeat: macros.

- Desired to never conflict with GNU Emacs subr.el routines
- Short name symbol is using now, so maybe conflict to other package function.
TODO check in test GNU Emacs subr.el to avoid conflict of naming.

- cond-list
- srfi-and-let* (Originally `and-let*`)
- srfi-cond (Original `cond`)
- $
- cut, cute
- and-let1
- let1
- rlet1
- if-let1
