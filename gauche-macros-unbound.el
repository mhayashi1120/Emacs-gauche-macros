;; -*- lexical-binding: t; -*-

;; To check duplicated name with core routine (and `subr-x`)

(require 'subr-x)

(defconst shortname-macros
  '(
    cond-list
    srfi-and-let*
    srfi-cond
    $
    cut cute
    and-let1
    let1
    rlet1
    if-let1
    ))

(dolist (fn shortname-macros)
  (when (fboundp fn)
    (error "Failed start test since %s have already bound as core routine"
           fn)))
