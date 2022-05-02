;;; gauche-macros.el --- -*- lexical-binding: t -*-

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords:
;; Emacs: GNU Emacs
;; Package-Requires: ((cl-lib "0.3"))


;;; Commentary:
;;

(require 'cl-lib)

;;; Code:

(defmacro let1 (var expr &rest body)
  "Bind VAR to EXPR and evaluate BODY.

Same as
\(let ((VAR EXPR)) BODY)"
  (declare (indent 2))
  `(let ((,var ,expr))
     ,@body))

(defmacro rlet1 (var expr &rest body)
  "Bind VAR to EXPR and return it value after evaluate BODY.

Same as
\(let ((VAR EXPR)) BODY VAR)"
  (declare (indent 2))
  `(let ((,var ,expr))
     ,@body
     ,var))

(defmacro if-let1 (var expr then &rest else)
  "Bind VAR to EXPR in THEN/ELSE form.
THEN is only evaluated if EXPR is non-nil.

Not like gauche if-let1 ELSE accept multiple forms
like emacs-lisp style `if'."
  (declare (indent 2))
  `(let ((,var ,expr))
     (if ,var ,then ,@else)))

(defmacro srfi-cond (&rest clauses)
  "Like `cond' but some SRFI like extension.

CLAUSES ::= CLAUSE . CLAUSES
CLAUSE ::= (TEST)
CLAUSE ::= (TEST BODY)
CLAUSE ::= (TEST => LAMBDA)
CLAUSE ::= (TEST GUARD => LAMBDA)

CLAUSE: Try each clause until TEST (optionally GUARD) return non-nil value
  and last form is evaluated.
LAMBDA: Call with one arg non-nil result of previous TEST
GUARD: Call with one arg non-nil result of previous TEST.
  If the GUARD return nil, following LAMBDA is not called.

TODO example

NOTE: Cannot handle multiple-values.
NOTE: Unlike scheme, not using `else' keyword.

\[SRFI-61]
http://srfi.schemers.org/srfi-61/srfi-61.html"
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
              ;; (test) 
              ;; (test body1 body2 ...)
              (progn ,@(cdr clause))
            ,res)))))
   clauses
   :from-end t
   :initial-value nil))



(defmacro and-let* (varlist &rest body)
  "Like `let' but only CLAW bind non-nil value.
Useful to avoid deep nesting of `let' and `and'/`when'/`if' test.

AND-LET* (CLAWS) BODY

CLAWS ::= '() | (cons CLAW CLAWS)
CLAW  ::=  (VARIABLE EXPRESSION) | (EXPRESSION) | BOUND-VARIABLE

\(let ((v1 (some)))
  (when v1
    (let ((v2 (any)))
      (when v2
        (message \"Working!\")))))

above can rewrite as following:

\(and-let* ((v1 (some))
           (v2 (any)))
   (message \"Working!\"))

\[SRFI-2]
http://srfi.schemers.org/srfi-2/srfi-2.html
"
  (declare (indent 1))
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
       (error "Malformed `and-let*' %s" v))
      ((not (symbolp (car v)))
       (error "Malformed `and-let*' %s" v))
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
  (declare (indent 2))
  `(and-let* ((,var ,test))
     ,@body))



;; From gauche common-macros.scm
(defmacro cond-list (&rest clauses)
  "Expand CLAUSES last form if TEST success.
`@' notation append EXPR or PROC results to result set.
\(TEST EXPR ...)
\(TEST => PROC)
\(TEST @ EXPR ...)
\(TEST => @ PROC)

e.g.

\(cond-list (t 1) (nil 2) (t '(3)))
  => (1 (3))

\(cond-list (t 1) (t @ '(2 3 4)))
  => (1 2 3 4)

\(cond-list ('a => (lambda (x) (list x)))
           ('b => @ (lambda (x) (list x)))
           ('c => @ (lambda (x) nil)))
  => ((a) b)

"
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



(defmacro $ (&rest args)
  "Convenience macro to chain functions.

NOTE: Unlike scheme function, symbol must be quoted. This behavior same as `mapcar', `mapc'.

($ 'f a $ 'g d) => (f a (g d))
($ 'f a $ 'g d $) => (lambda (x) (f a (g d x)))
($ 'f a $ 'g d $*) => (lambda (&rest xs) (f a (apply 'g d xs)))

See `cut', `cute'
"
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
           (setq arg-sym (gensym))
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

NOTE: Unlike scheme function, symbol must be quoted. This behavior same as `mapcar', `mapc'.

(cut 'a <>) => (lambda (arg) (a arg))
(cut '+ <> 2) => (lambda (arg) (+ arg 2))
(cut <> 1 2) => (lambda (arg) (funcall arg 1 2))
(cut '+ 10 <...>) => (lambda (&rest args) (apply '+ 10 args))

NOTE: Internally certainly using `funcall' and `apply' to call elisp function.
 To simplify this help, omit in this help description. 
"
  (let ((forms `())
        (args `())
        (tail* nil))
    (mapc
     (lambda (e)
       (cond
        ((eq e '<>)
         (let ((sym (gensym "arg")))
           (setq forms (cons sym forms))
           (setq args (cons sym args))))
        ((eq e '<...>)
         (when forms
           (error "Malformed cut. `<...>' should be last of the form."))
         (let ((sym (gensym "args")))
           (setq forms (list sym))
           (setq args (list '&rest sym))
           (setq tail* t)))
        (t
         (setq forms (cons e forms)))))
     (reverse exprs))
    (cond
     ((not tail*)
      (setq forms (cons 'funcall forms)))
     (t
      (setq forms (cons 'apply forms))))
    `(lambda (,@args) ,forms)))

(defmacro cute (&rest exprs)
  "Same as `cut' except non `<>' `<...>' EXPR evaluated before construct function.

(cute '+ (+ 20 30) <>) => (let ((a 50)) (lambda (a1) (+ a a1)))

"
  (let ((forms '())
        (args '())
        (vars '())
        (tail* nil))
    (mapc
     (lambda (e)
       (cond
        ((eq e '<>)
         (let ((sym (gensym "arg")))
           (setq forms (cons sym forms))
           (setq args (cons sym args))))
        ((eq e '<...>)
         (when forms
           (error "Malformed cute. `<...>' should be last of the form."))
         (let ((sym (gensym "args")))
           (setq forms (list sym))
           (setq args (list '&rest sym))
           (setq tail* t)))
        (t
         (let ((sym (gensym "const")))
           (setq vars (cons (list sym e) vars))
           (setq forms (cons sym forms))))))
     (reverse exprs))
    (cond
     (tail*
      (setq forms (cons 'apply forms)))
     (t
      (setq forms (cons 'funcall forms))))
    `(let (,@vars)
       (lambda (,@args) ,forms))))

;;;
;;; Fontify
;;;

(defun gauche-macros--activate-font-lock-keywords ()
  (gauche-macros--append-font-lock-functional-keywords
   "let1" "rlet1" "if-let1"
   "and-let*"
   "srfi-cond" "cond-list"
   "cut"
   "$"
   )
  (gauche-macros--append-font-lock-named-binding-keywords)
  (gauche-macros--append-font-lock-syntactic-keywords
   "=>" "@" "$" "$*"
   )
  )

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
