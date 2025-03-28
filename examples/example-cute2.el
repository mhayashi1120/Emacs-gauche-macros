;; -*- lexical-binding: t; -*-

(require 'gauche-macros)

(defvar gauche-macros-test-base 5)

(defvar cut1 (cut #'+ <> gauche-macros-test-base))
(defvar cute1 (cute #'+ <> gauche-macros-test-base))


(funcall cut1 1)
;; -> 6
(funcall cute1 1)
;; -> 6

(setq gauche-macros-test-base 50)
;; -> <void>

(funcall cut1 1)
;; -> 51
(funcall cute1 1)
;; -> 6
