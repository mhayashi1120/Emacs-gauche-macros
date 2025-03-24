;;; gauche-macros.el --- Gauche :heartbeat: macros -*- lexical-binding: t; -*-

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: lisp tools
;; Emacs: GNU Emacs
;; URL: https://github.com/mhayashi1120/Emacs-gauche-macros
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.8.3

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Import [Gauche](https://practical-scheme.net/gauche/) macros to elisp with changing
;;   some conventions. Just import :heartbeat: macros.
;;
;; This package is:
;; - Desired to never conflict with GNU Emacs core routines.
;; - Short name symbol is using now, so maybe conflict to other package function.
;; - This package will **NEVER** be uploaded to elpa since avoid namespace confliction.

;;; Code:

(require 'cl-lib)

(defmacro let1 (var expr &rest body)
  "Bind VAR to EXPR and evaluate BODY.

Same as
\(let ((VAR EXPR)) BODY)"
  (declare (indent 2) (debug (sexp form body)))
  `(let ((,var ,expr))
     ,@body))

(defmacro rlet1 (var expr &rest body)
  "Bind VAR to EXPR and return it value after evaluate BODY.

Same as
\(let ((VAR EXPR)) BODY VAR)"
  (declare (indent 2) (debug (sexp form body)))
  `(let ((,var ,expr))
     ,@body
     ,var))

(defmacro if-let1 (var expr then &rest else)
  "Bind VAR to EXPR in THEN/ELSE form.
THEN is only evaluated if EXPR is non-nil.

Not like gauche `if-let1' ELSE form accept multiple forms
like Emacs-Lisp style `if'."
  (declare (indent 2) (debug (sexp form form body)))
  `(let ((,var ,expr))
     (if ,var ,then ,@else)))

;; (let ((sample '((a . "A"))))
;;
;;   (list
;;    ;; Same as Emacs-Lisp cond.
;;    (srfi-cond
;;     ((assoc 'a sample)
;;      (list (cdr (assoc 'a sample)))))
;;
;;    ;; Above is rewrite as following.
;;    (srfi-cond
;;     ((assoc 'a sample) =>
;;      (lambda (v) (list (cdr v)))))
;;
;;    (srfi-cond
;;     ((assoc 'b sample) =>
;;      (lambda (v) (error "Should not found"))))
;;
;;    ;; Guard examples. Guard clause can continue TESTs
;;    (srfi-cond
;;     ((assoc 'a sample)
;;      (lambda (v) (string= (cdr v) "B")) =>
;;      (lambda (v) (list (cdr v))))
;;     ((assoc 'a sample)
;;      (lambda (v) (string= (cdr v) "A")) =>
;;      (lambda (v) (list (cdr v)))))
;;    ))

(defmacro srfi-cond (&rest clauses)
  "Like `cond' but SRFI extension.

CLAUSES ::= CLAUSE . CLAUSES | ()
CLAUSE ::= (TEST . BODY)
CLAUSE ::= (TEST => LAMBDA)
CLAUSE ::= (TEST GUARD => LAMBDA)

CLAUSE: Try each clause until TEST (optionally GUARD) return non-nil value
  and succeeding BODY/LAMBDA is evaluated.
LAMBDA: Function call with TEST result as an argument.
GUARD: Function call with TEST result as an argument.
  If the GUARD return nil, succeeding LAMBDA is not called and continue
  to the next CLAUSEs.

Emacs-Lisp `cond':
\(let (tmp)
  (cond
   ((setq tmp (string-to-number x))
     tmp)))

Above rewrite to:
\(srfi-cond
  ((string-to-number x) =>
   (lambda (v) v)))

NOTE: Cannot handle multiple-values since Emacs-Lisp doesn't have.
NOTE: Unlike scheme, not using `else' keyword using `t' same as Emacs-Lisp cond.

\[SRFI-61]
http://srfi.schemers.org/srfi-61/srfi-61.html"
  (declare (debug t))
  (cl-reduce
   (lambda (clause res)
     (let ((test (car clause)))
       (cond
        ;; (test => receiver)
        ((eq (cadr clause) '=>)
         (unless (= (length clause) 3)
           (error "Malformed `srfi-cond' test => expr"))
         (let ((v (make-symbol "v")))
           `(let ((,v ,test))
              (if ,v
                  (,(eval (cl-caddr clause)) ,v)
                ,res))))
        ;; (generator guard => receiver)
        ((eq (cl-caddr clause) '=>)        ; srfi-61
         (unless (= (length clause) 4)
           (error "Malformed `srfi-cond' test guard => expr"))
         (let ((v1 (make-symbol "v1")))
           `(let ((,v1 ,test))
              (if (and ,v1 (,(eval (cadr clause)) ,v1))
                  (,(eval (cl-cadddr clause)) ,v1)
                ,res))))
        (t
         `(if ,test
              ;; (test . body)
              (progn ,@(cdr clause))
            ,res)))))
   clauses
   :from-end t
   :initial-value nil))



;; Renamed and-let* -> srfi-and-let* since similar `and-let*' is introduced subr-x.el GNU Emacs
(defmacro srfi-and-let* (varlist &rest body)
  "Like `let' but only CLAW bind non-nil value.
Useful to avoid deep nesting of `let' and `and'/`when'/`if' test.

AND-LET* (CLAWS) BODY

CLAWS ::= \\='() | (CLAW . CLAWS)
CLAW  ::=  (VARIABLE EXPRESSION) | (EXPRESSION) | BOUND-VARIABLE

\(let ((v1 (some)))
  (when v1
    (let ((v2 (any)))
      (when v2
        (message \"Working!\")))))

above can rewrite as following:

\(srfi-and-let*
   ((v1 (some))
    (v2 (any)))
  (message \"Working!\"))

\[SRFI-2]
http://srfi.schemers.org/srfi-2/srfi-2.html
"
  (declare (indent 1) (debug let))
  (cl-reduce
   (lambda (v res)
     (cond
      ((atom v)
       ;; BOUND-VARIABLE
       `(and ,v ,res))
      ((= (length v) 1)
       ;; (EXPRESSION)
       `(and ,@v ,res))
      ((> (length v) 2)
       (error "Malformed `srfi-and-let*' %s" v))
      ((not (symbolp (car v)))
       (error "Malformed `srfi-and-let*' %s" v))
      (t
       ;; (VARIABLE EXPRESSION)
       `(let ((,(car v) ,(cadr v)))
          (and ,(car v) ,res)))))
   varlist
   :from-end t
   :initial-value `(progn ,@body)))

(defmacro and-let1 (var test &rest body)
  "Syntax sugar:
(and-let1 var test body ...) == (and-let* ((var test)) body ...)"
  (declare (indent 2) (debug (sexp form body)))
  `(srfi-and-let* ((,var ,test))
     ,@body))



;; Useful when constructing json.
;; (let ((input (make-hash-table)))
;;   (puthash 'a "A" input)
;;   (puthash 'b "B" input)
;;   (puthash 'c 10 input)
;;   (puthash 'd [1,2,3] input)
;;   (puthash 'j `((z . "Z") (y . "Y"))  input)
;;   (json-encode
;;    (cond-list
;;     (t
;;      (cons "Required" "Field"))
;;     ((gethash 'a input) =>
;;      (lambda (v) (cons "optionalField" v)))
;;     ((gethash 'not-found input) =>
;;      (lambda (v) (error "Should not found %s" v)))
;;     ((gethash 'c input) =>
;;      (lambda (v) (cons "optionalVector" (make-vector 5 v))))
;;     ((gethash 'd input) =>
;;      (lambda (v) (cons "optionalValue" (aref v 0))))
;;     ((gethash 'j input) =>
;;      (lambda (v) (cons "optionalChildJson" v)))
;;     ;; This will expanded to parent json
;;     ((gethash 'j input) => @
;;      (lambda (v) v))
;;     )
;;    ))

;; From gauche common-macros.scm
(defmacro cond-list (&rest clauses)
  "Expand CLAUSES succeeding form if TEST success.
`@' notation append EXPR or PROC results to result set.
\(TEST EXPR ...)
\(TEST => PROC)
\(TEST @ EXPR ...)
\(TEST => @ PROC)

e.g.

\(cond-list
  (t 1)
  (nil 2)
  (t \\='(3)))
  => (1 (3))

\(cond-list
  (t 1)
  (t @ \\='(2 3 4)))
  => (1 2 3 4)

\(cond-list
  (\\='a => (lambda (x) (list x)))
  (\\='b => @ (lambda (x) (list x)))
  (\\='c => @ (lambda (x) nil)))
  => ((a) b)

"
  (declare (debug t))
  (cl-reduce
   (lambda (clause accum)
     (when (null clause)
       (error "No matching clause %s" clause))
     (let ((test-result (make-symbol "result"))
           (test (car clause))
           (body (cdr clause)))
       `(let ((,test-result ,test))
          (append
           (if ,test-result
               ,(cond
                 ;; (TEST => @ PROC)
                 ((and (eq (car body) '=>)
                       (eq (cadr body) '@))
                  (unless (= (length (cddr body)) 1)
                    (error "Invalid clause %s" body))
                  (let ((proc (cl-caddr body)))
                    (unless (functionp proc)
                      (error "Form must be a function but %s" proc))
                    `(funcall ,proc ,test-result)))
                 ;; (TEST => PROC)
                 ((eq (car body) '=>)
                  (unless (= (length (cdr body)) 1)
                    (error "Invalid clause %s" body))
                  (let ((proc (cadr body)))
                    (unless (functionp proc)
                      (error "Form must be a function but %s" proc))
                    `(list (funcall ,proc ,test-result))))
                 ;; (TEST @ EXPR ...)
                 ((eq (car body) '@)
                  `(progn ,@(cdr body)))
                 ;; (TEST EXPR ...)
                 (t
                  `(list (progn ,@body))))
             nil)
           ,accum))))
   clauses
   :from-end t
   :initial-value nil))



;; TODO when eval the following with edebug-defun not working
;; ($  (cut 'mapconcat #'identity <> ":")
;;     $ #'mapcar (cut #'format "-%d-" <>)
;;     $ #'mapcar #'1+
;;     $ #'mapcar #'string-to-number
;;     $ (cut #'split-string <> ",") "11,22,33")

(defmacro $ (&rest args)
  "Convenience macro to chain functions.

\($ #\\='f a $ #\\='g d) => (f a (g d))
\($ #\\='f a $ #\\='g d $) => (lambda (x) (f a (g d x)))
\($ #\\='f a $ #\\='g d $*) => (lambda (&rest xs) (f a (apply #\\='g d xs)))

See `cut', `cute'
"
  (declare (debug t))
  (let ((accum '())
        (delay-funcall nil)
        (delay-apply nil)
        (expand* nil)
        (arg-sym nil))
    (mapc
     (lambda (x)
       (cond
        ((memq x '($ $*))
         (cond
          ((null accum)
           (cond
            ((eq x '$)
             (setq delay-funcall t))
            ((eq x '$*)
             (setq delay-apply t))
            (t
             (error "Assert")))
           (setq arg-sym (make-symbol "arg"))
           (setq accum (list arg-sym)))
          (t
           (setq accum
                 (cond
                  ((not expand*)
                   `((funcall ,@accum)))
                  (t
                   `((apply ,@accum)))))))
         (cond
          ((eq x '$)
           (setq expand* nil))
          ((eq x '$*)
           (setq expand* t))))
        (t
         (setq accum (cons x accum)))))
     (reverse (cons '$ args)))
    (cond
     (delay-funcall
      `(lambda (,arg-sym) ,@accum))
     (delay-apply
      `(lambda (&rest ,arg-sym) ,@accum))
     (t
      (car accum)))))

(defmacro cut (&rest exprs)
  "Convenience macro to generate function handle partial application.

NOTE: Unlike scheme, function symbol must be quoted. This behavior
   same as `mapcar', `mapc'.

\(cut #\\='a <>) => (lambda (arg) (a arg))
\(cut #\\='+ <> 2) => (lambda (arg) (+ arg 2))
\(cut <> 1 2) => (lambda (arg) (funcall arg 1 2))
\(cut #\\='+ 10 <...>) => (lambda (&rest args) (apply #\\='+ 10 args))
\(cut #\\='+ 1 <> 3 <>) => (lambda (arg1 arg2) (+ 1 arg1 3 arg2))

NOTE: To simplify this help, internally clearly using `funcall' or `apply'
  to expand the EXPRS.
"
  (declare (debug t))
  (let ((forms `())
        (args `())
        (tail* nil))
    (mapc
     (lambda (e)
       (cond
        ((eq e '<>)
         (let ((sym (make-symbol "arg")))
           (setq forms (cons sym forms))
           (setq args (cons sym args))))
        ((eq e '<...>)
         (when forms
           (error "Malformed cut. `<...>' should be last of the form."))
         (let ((sym (make-symbol "args")))
           (setq forms (list sym))
           (setq args (list '&rest sym))
           (setq tail* t)))
        (t
         (setq forms (cons e forms)))))
     (reverse exprs))
    (cond
     ((not tail*)
      (setq forms (cons #'funcall forms)))
     (t
      (setq forms (cons #'apply forms))))
    `(lambda (,@args) ,forms)))

(defmacro cute (&rest exprs)
  "Same as `cut' except non `<>' `<...>' EXPR is evaluated before
 construct the function.

\(cute \\='+ (+ 20 30) <>) => (let ((a 50)) (lambda (a1) (+ a a1)))

"
  (declare (debug t))
  (let ((forms '())
        (args '())
        (vars '())
        (tail* nil))
    (mapc
     (lambda (e)
       (cond
        ((eq e '<>)
         (let ((sym (make-symbol "arg")))
           (setq forms (cons sym forms))
           (setq args (cons sym args))))
        ((eq e '<...>)
         (when forms
           (error "Malformed cute. `<...>' should be last of the form."))
         (let ((sym (make-symbol "args")))
           (setq forms (list sym))
           (setq args (list '&rest sym))
           (setq tail* t)))
        (t
         (let ((sym (make-symbol "const")))
           (setq vars (cons (list sym e) vars))
           (setq forms (cons sym forms))))))
     (reverse exprs))
    (cond
     (tail*
      (setq forms (cons #'apply forms)))
     (t
      (setq forms (cons #'funcall forms))))
    `(let (,@vars)
       (lambda (,@args) ,forms))))

;;;
;;; Fontify
;;;

(defun gauche-macros--activate-font-lock-keywords ()
  (gauche-macros--append-font-lock-functional-keywords
   "let1" "rlet1" "if-let1"
   "srfi-and-let*" "and-let1"
   "srfi-cond" "cond-list"
   "cut" "cute"
   "$")
  (gauche-macros--append-font-lock-named-binding-keywords)
  (gauche-macros--append-font-lock-syntactic-keywords
   "=>" "@" "$" "$*"
   "<>" "<...>"))

(defun gauche-macros--append-font-lock-functional-keywords (&rest names)
  (font-lock-add-keywords
   nil
   `((,(concat
        "(\\("
        (regexp-opt names)
        "\\)\\(?:[\s\t\n]\\|$\\)")
      (1 font-lock-keyword-face nil t)))))

(defun gauche-macros--append-font-lock-syntactic-keywords (&rest names)
  (font-lock-add-keywords
   nil
   `((,(concat
        "\\_<"
        (regexp-opt names t)
        "\\_>")
      (1 font-lock-constant-face nil t)))))

(defun gauche-macros--append-font-lock-named-binding-keywords (&rest names)
  (font-lock-add-keywords
   nil
   `((,(concat
        "(\\("
        (regexp-opt names)
        "\\)\\>"
        "\\(?:"
        "[\s\t]+"
        "\\([^\s\t\n]+\\)"
        "\\)?")
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))

(defun gauche-macros--dynamic-indent (level-calculator)
  `(lambda (indent-point state)
     ;; (message "%s %s" (current-message) (list indent-point state))
     (let (top-indent partial-sexp)
       (save-excursion
         (backward-sexp)
         (setq top-indent (max (1- (current-column)) 0))
         (condition-case nil
             (while (and (not (eobp)) (< (point) indent-point))
               (let ((s (read (current-buffer))))
                 (setq partial-sexp (cons s partial-sexp)))
               (skip-chars-forward "\s\t\n\r\f"))
           (error)))
       (setq partial-sexp (nreverse partial-sexp))
       (cl-destructuring-bind (level normal-indent)
           (funcall ,level-calculator partial-sexp top-indent)
         ;; (message "%s %s" (current-message) (list level partial-sexp normal-indent))
         (lisp-indent-specform level state
                               indent-point normal-indent)))))


;;;
;;; Unloading
;;;

(defun gauche-macros-unload-function ()
  (remove-hook 'emacs-lisp-mode-hook
               'gauche-macros--activate-font-lock-keywords)
  nil)


;;;
;;; Loading
;;;

(add-hook 'emacs-lisp-mode-hook
          'gauche-macros--activate-font-lock-keywords)


(provide 'gauche-macros)

;;; gauche-macros.el ends here
