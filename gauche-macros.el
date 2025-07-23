;;; gauche-macros.el --- Gauche :heartbeat: macros -*- lexical-binding: t; -*-

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: lisp tools
;; Emacs: GNU Emacs
;; URL: https://github.com/mhayashi1120/Emacs-gauche-macros
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.9.2

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
(require 'pcase)

(when (< emacs-major-version 26)
  (require 'cl))

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

(defmacro srfi-cond (&rest clauses)
  "Like `cond' but SRFI extension.

CLAUSES ::= (CLAUSE . CLAUSES) | ()

CLAUSE ::= (TEST . BODY)
CLAUSE ::= (TEST => FUNCTION1)
CLAUSE ::= (TEST GUARD => FUNCTION1)

CLAUSE: Try each clause until TEST (optionally GUARD) return non-nil value
  and succeeding BODY/FUNCTION1 is evaluated.
FUNCTION1: Function call with TEST result as an argument.
GUARD: Function call with TEST result as an argument.
  If the GUARD return nil, succeeding FUNCTION1 is not called and continue
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
  (declare (debug
            ;; CLAUSES
            (&rest
             ;; CLAUSE
             (form
              &or
              ;; (TEST => FUNCTION1)
              ["=>" lambda-expr]
              ;; (TEST GUARD => FUNCTION1)
              [form "=>" lambda-expr]
              ;; (TEST . BODY)
              body))))
  (cl-reduce
   (lambda (clause res)
     (pcase clause
       (`(,test . ,rest)
        (pcase rest
          (`(=> ,func1)
           (let ((V (gensym))
                 (FUNC1 (gensym)))
             `(let ((,V ,test)
                    (,FUNC1 ,func1))
                (if ,V
                    (funcall ,FUNC1 ,V)
                  ,res))))
          (`(,guard => ,func1)
           (let ((V (gensym))
                 (GUARD (gensym))
                 (FUNC1 (gensym)))
             `(let ((,V ,test)
                    (,GUARD ,guard)
                    (,FUNC1 ,func1))
                (if (and ,V (funcall ,GUARD ,V))
                    (funcall ,FUNC1 ,V)
                  ,res))))
          ((pred (lambda (l) (memq '=> l)))
           (error "Malformed `srfi-cond' \"%.50s\"" rest))
          (body
           `(if ,test
                ;; (test . body)
                (progn ,@body)
              ,res))))
       (_
        (error "Malformed `srfi-cond'"))))
   clauses
   :from-end t
   :initial-value nil))



;; Renamed and-let* -> srfi-and-let* since similar `and-let*' is introduced subr-x.el GNU Emacs
(defmacro srfi-and-let* (claws &rest body)
  "Like `let' but only CLAW bind non-nil value.
Useful to avoid deep nesting of `let' and `and'/`when'/`if' test.

CLAWS ::= (CLAW . CLAWS) | ()

CLAW  ::= (VARIABLE EXPRESSION)
CLAW  ::= (EXPRESSION)
CLAW  ::= BOUND-VARIABLE

Example:

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
   claws
   :from-end t
   :initial-value `(progn ,@body)))

(defmacro and-let1 (var test &rest body)
  "Syntax sugar:
(and-let1 var test body ...) == (and-let* ((var test)) body ...)"
  (declare (indent 2) (debug (sexp form body)))
  `(srfi-and-let* ((,var ,test))
     ,@body))



;; From gauche common-macros.scm
(defmacro cond-list (&rest clauses)
  "Expand CLAUSES succeeding form if TEST success.
`@' notation append EXPR or FUNC1 results to result set.

CLAUSES ::= (CLAUSE . CLAUSES) | ()

CLAUSE  ::= (TEST [EXPR ...])
CLAUSE  ::= (TEST => FUNC1)
CLAUSE  ::= (TEST @ EXPR ...)
CLAUSE  ::= (TEST => @ FUNC1)

e.g.

\(cond-list
  (t 1)
  (nil 2)
  (t \\='(3))
  (4))
  => (1 (3) 4)

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
  (declare (debug
            ;; CLAUSES
            (&rest
             ;; CLAUSE
             (form
              &or
              ;; (TEST => FUNC1)
              ;; (TEST => @ FUNC1)
              ["=>" &or ["@" lambda-expr] lambda-expr]
              ;; (TEST @ EXPR ...)
              ["@" body]
              ;; (TEST EXPR ...)
              body))))
  (cl-reduce
   (lambda (clause accum)
     (pcase clause
       (`(,test . ,body)
        (let ((V (gensym)))
          `(let ((,V ,test))
             (append
              (if ,V
                  ,(pcase body
                     ;; (TEST => @ FUNC1)
                     (`(=> @ ,proc)
                      (let ((FUNC (gensym)))
                        `(let ((,FUNC ,proc))
                           (unless (functionp ,FUNC)
                             (error "Form must be a function but %s" ,FUNC))
                           (funcall ,FUNC ,V))))
                     ;; (TEST => FUNC1)
                     (`(=> ,proc)
                      (let ((FUNC (gensym)))
                        `(let ((,FUNC ,proc))
                           (unless (functionp ,FUNC)
                             (error "Form must be a function but %s" ,FUNC))
                           (list (funcall ,FUNC ,V)))))
                     ;; (TEST @ EXPR ...)
                     (`(@ . ,exprs)
                      `(progn ,@exprs))
                     ((pred (lambda (l) ; Disallow "=>" "@" syntax
                              (or (memq '=> l)
                                  (memq '@ l))))
                      (error "Malformed `cond-list' \"%.50s\"" body))
                     ;; (TEST)
                     (()
                      `(list ,V))
                     ;; (TEST EXPR ...)
                     (exprs
                      `(list (progn ,@exprs))))
                nil)
              ,accum))))
       (_
        (error "No matching clause %s" clause))))
   clauses
   :from-end t
   :initial-value nil))



(defmacro $ (&rest args)
  "Convenience macro to chain functions.

\($ #\\='f a $ #\\='g d) => (f a (g d))
\($ #\\='f a $ #\\='g d $) => (lambda (x) (f a (g d x)))
\($ #\\='f a $ #\\='g d $*) => (lambda (&rest xs) (f a (apply #\\='g d xs)))

See `cut', `cute'
"
  (declare (debug (&rest [&or "$" "$*" form])))
  (let ((accum '())
        (delay-funcall nil)
        (delay-apply nil)
        (expand* nil)
        (ARG nil))
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
           (setq ARG (gensym))
           (setq accum (list ARG)))
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
      `(lambda (,ARG) ,@accum))
     (delay-apply
      `(lambda (&rest ,ARG) ,@accum))
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
  (declare (debug (&rest [&or "<>" "<...>" sexp])))
  (let ((forms `())
        (args `())
        (tail* nil))
    (mapc
     (lambda (e)
       (cond
        ((eq e '<>)
         (let ((ARG (gensym)))
           (setq forms (cons ARG forms))
           (setq args (cons ARG args))))
        ((eq e '<...>)
         (when forms
           (error "Malformed cut. `<...>' should be last of the form."))
         (let ((ARGS (gensym)))
           (setq forms (list ARGS))
           (setq args (list '&rest ARGS))
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
  (declare (debug (&rest [&or "<>" "<...>" form])))
  (let ((forms '())
        (args '())
        (vars '())
        (tail* nil))
    (mapc
     (lambda (e)
       (cond
        ((eq e '<>)
         (let ((ARG (gensym)))
           (setq forms (cons ARG forms))
           (setq args (cons ARG args))))
        ((eq e '<...>)
         (when forms
           (error "Malformed cute. `<...>' should be last of the form."))
         (let ((ARGS (gensym)))
           (setq forms (list ARGS))
           (setq args (list '&rest ARGS))
           (setq tail* t)))
        (t
         (let ((ARG (gensym)))
           (setq vars (cons (list ARG e) vars))
           (setq forms (cons ARG forms))))))
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
