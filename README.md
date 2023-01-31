# Emacs-gauche-macros

Import [Gauche](https://practical-scheme.net/gauche/) macros to elisp with changing some conventions. Just import :heartbeat: macros.

- Desired to never conflict with GNU Emacs core routines.
- Short name symbol is using now, so maybe conflict to other package function.

## Macro

- cond-list
- srfi-and-let* (Originally `and-let*`)
- srfi-cond (Original `cond`)
- $
- cut, cute
- and-let1
- let1
- rlet1
- if-let1

# Test

```
make check
```

or

Something like:

```
make EMACS=/usr/local/emacs-28/bin/emacs check
```

You can change by: TODO

```
cp env.mk.sample env.mk

```




# TODO

