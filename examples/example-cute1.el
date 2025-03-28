;; -*- lexical-binding: t; -*-

(require 'gauche-macros)

;; Same result this simple example
;; try edebug each form
(funcall (cut #'+ (+ 20 30) <>) 1)
;; -> 51

(funcall (cute #'+ (+ 20 30) <>) 1)
;; -> 51
