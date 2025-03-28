(require 'gauche-macros)

($  (cut 'mapconcat #'identity <> ":")
    $ #'mapcar (cut #'format "-%d-" <>)
    $ #'mapcar #'1+
    $ #'mapcar #'string-to-number
    $ (cut #'split-string <> ",") "11,22,33")
;; -> "-12-:-23-:-34-"
