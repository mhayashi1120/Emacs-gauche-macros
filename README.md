# Emacs-gauche-macros

Import [Gauche](https://practical-scheme.net/gauche/) macros to elisp with changing
  some conventions. Just import :heartbeat: macros.

 This package is:
- Desired to never conflict with GNU Emacs core routines.
- Short name symbol is using now, so maybe conflict to other package function.
- This package will **NEVER** be uploaded to elpa since avoid namespace confliction.

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

### Specific version Emacs

Something like:

```
make EMACS=/usr/local/emacs-28/bin/emacs check
```

### Specific settings

You can change environment by:

```
cp env.mk.sample env.mk
```

and edit env.mk


# TODO

