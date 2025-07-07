;; -*- lexical-binding: t; -*-

(require 'gauche-macros)
(require 'ert)

(ert-deftest cond-list1 ()
  :tags '(gauche-macros)
  (should (equal (cond-list
                  ;; normal
                  (1 1)
                  ;; body is function
                  (2 => (lambda (x) x))
                  ;; function call and @ expansion
                  (3 => @ (lambda (x) (make-list x x)))
                  ;; @ expand
                  (4 @ '(4 5 6))
                  (nil 1)
                  ;; ok test is function call
                  (((lambda () t)) 7)
                  ;; ng test is function call
                  (((lambda () nil)) 8)
                  (t 9 10)
                  ;; missing EXPR return TEST result
                  (11)
                  )
                 '(1 2 3 3 3 4 5 6 7 10 11))))

(ert-deftest cond-list2 ()
  :tags '(gauche-macros)
  (should
   (equal
    (cond-list
     ;; empty body
     (nil)
     (t))
    '(t))))

(defun gauche-macro-test-add10 (i)
  (+ i 10))

(defvar gauche-macro-test-var3 #'gauche-macro-test-add10)

(ert-deftest cond-list3 ()
  :tags '(gauche-macros)
  (should
   (equal
    (cond-list
     ;; empty body
     (1 => #'gauche-macro-test-add10)
     (5 => gauche-macro-test-var3))
    '(11 15))))

(ert-deftest srfi-and-let1 ()
  :tags '(gauche-macros)
  ;; (VARIABLE EXPRESSION)
  (should (equal (srfi-and-let* ((a nil)) a) nil))
  (should (equal (srfi-and-let* ((a t)) a) t))
  (should (equal (srfi-and-let* ((a 1) (b 2)) (list a b)) '(1 2)))
  ;; (EXPRESSION)
  (should (equal (srfi-and-let* ((1)) 3) 3))
  (should (equal (srfi-and-let* ((nil)) 3) nil))
  (should (equal (srfi-and-let* (((identity t))) 3) 3))
  (should (equal (srfi-and-let* (((identity nil))) 3) nil))
  ;; BOUND-VARIABLE
  (should (equal (let ((a 1)) (srfi-and-let* (a) 3)) 3))
  (should (equal (let ((a nil)) (srfi-and-let* (a) 3)) nil))
  )



(ert-deftest $-0001 ()
  :tags '(gauche-macros)

  (should (equal ($ #'format "%s-%s" "A" "B")
                 "A-B"))
  (should (equal ($ #'format "%s-%s" $* #'identity '("A" "B"))
                 "A-B"))
  (should (equal (funcall ($ #'format "%s-%s" $*) "A" "B")
                 "A-B"))
  (should (equal (apply ($ #'format "%s-%s" $*) '("A" "B"))
                 "A-B"))
  (should (equal (funcall ($ #'format "-%s-" $) "AAA")
                 "-AAA-"))
  (should (equal ($ #'format ":%s:" "BBB")
                 ":BBB:"))
  (should (equal ($ #'format "_%s_" $ #'concat "AB:" "CD")
                 "_AB:CD_"))
  (should (equal (funcall ($ #'format "_%s_" $ #'concat "AB:" $) "CD")
                 "_AB:CD_"))
  (should (equal (funcall ($ 'format "_%s_" $ #'concat $*) "AB:" "CD")
                 "_AB:CD_"))
  (should (equal (apply ($ #'format "_%s_" $ #'concat $*) '("AB:" "CD"))
                 "_AB:CD_"))
  (should (equal ($ #'format "_%s_" $ #'concat $* #'identity '("AB:" "CD"))
                 "_AB:CD_"))

  (should (equal ($ #'format "_%s_" $* #'list $ #'concat "AB:" "CD")
                 "_AB:CD_"))

  (should (equal ($ #'format "-%s-" $ #'number-to-string $ '* 2 5)
                 "-10-"))

  (should (equal (funcall ($ $) (lambda () "A"))
                 "A"))
  (should (equal (funcall ($ $*) (lambda (a) (format "-%s-" a)) ":B:")
                 "-:B:-"))
  )

;; TODO test more edge case
(ert-deftest $-0002 ()
  :tags '(gauche-macros)
  (should (equal ($ #'format "_%s_" $ (cut #'mapconcat #'identity <> ":") '("AA" "BB"))
                 "_AA:BB_"))
  (should (equal (funcall ($ #'format "_%s_" $ (cut #'mapconcat #'identity <> ":") $) '("AA" "BB"))
                 "_AA:BB_"))
  (should (equal ($ #'format "-%s:%s:%s-" $* #'mapcar (cut #'* 2 <>) '(2 3 4))
                 "-4:6:8-"))
  (should (equal (funcall ($ #'format "-%s:%s:%s-" $* #'mapcar (cut '* 2 <>) $) '(3 4 5))
                 "-6:8:10-"))
  (should (equal (funcall ($ #'format "-%s:%s:%s-" $* #'mapcar (cut '* 2 <>) $*) '(4 5 6))
                 "-8:10:12-"))
  )



;; TODO test more edge case
(ert-deftest cut1 ()
  :tags '(gauche-macros)
  (should (equal (mapcar (cut #'split-string <> ",") (list "A,B,C" "1-2-3"))
                 '(("A" "B" "C") ("1-2-3"))))
  (should (equal (funcall (cut #'split-string <> <>) "A,B,C" ",") '("A" "B" "C")))
  (should (equal (apply (cut #'split-string <...>) '("A,B,C" ",")) '("A" "B" "C")))
  (should (equal (funcall (cut #'format "1: %s 2: %s" <...>) "A" "B") "1: A 2: B"))

  ;;from gauche info
  (should (equal (funcall (let ((a 5)) (cut #'cons (+ a 1) <>)) 7) '(6 . 7)))
  (should (equal (funcall (cut #'list 1 <> 3 <> 5) 2 4) '(1 2 3 4 5)))
  (should (equal (funcall (cut #'list)) nil))
  (should (equal (funcall (cut #'list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6)))
  (should (equal (funcall (cut <> 1 2) #'+) 3))
  (should (equal (mapcar (cut #'* 2 <>) '(1 2 3 4)) '(2 4 6 8)))
  )

(ert-deftest cut2 ()
  :tags '(gauche-macros)

  (let ((f (cut (lambda (x) (+ x 2)) <>)))
    (should (equal (funcall f 1) 3))
    (should (equal (funcall f 5) 7)))
  )

(ert-deftest cute1 ()
  :tags '(gauche-macros)
  (should (equal (mapcar (cute #'split-string <> (char-to-string ?\-)) (list "A,B,C" "1-2-3"))
                 '(("A,B,C") ("1" "2" "3"))))
  (should (equal (funcall (cute #'format "1: %s 2: %s" <...>) "A" "B") "1: A 2: B"))
  (should (equal (funcall (cute #'split-string <> <>) "A,B,C" ",") '("A" "B" "C")))
  (should (equal (apply (cute #'split-string <...>) '("A,B,C" ",")) '("A" "B" "C")))
  (should (equal (funcall (let ((a 5)) (cute #'cons (+ a 1) <>)) 7) '(6 . 7)))
  (should (equal (funcall (cute #'list 1 <> 3 <> 5) 2 4) '(1 2 3 4 5)))
  (should (equal (funcall (cute #'list)) nil))
  (should (equal (funcall (cute #'list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6)))
  (should (equal (funcall (cute <> 1 2) #'+) 3))
  (should (equal (mapcar (cute #'* 2 <>) '(1 2 3 4)) '(2 4 6 8)))
  )

(ert-deftest cute2 ()
  :tags '(gauche-macros)

  (let ((f (cute (lambda (x) (+ x 2)) <>)))
    (should (equal (funcall f 1) 3))
    (should (equal (funcall f 5) 7)))
  )

(defun gauche-macros-test--generator (n)
  (let ((m n))
    (lambda (arg) (member m arg))))

(ert-deftest srfi-cond1 ()
  :tags '(gauche-macros)
  (should (equal (srfi-cond (t t)) t))
  (should (equal (srfi-cond (nil t)) nil))
  (should (equal (srfi-cond ((member 1 '(1 2)) 'a) ((member 1 '(1 2)) 'b)) 'a))
  (should (equal (srfi-cond ((member 3 '(1 2)) 'a) ((member 1 '(1 2)) 'b)) 'b))
  (should (equal (srfi-cond ((member 3 '(1 2)) 'a) (t 'c)) 'c))

  ;; test => expr
  (should (equal (srfi-cond
                  ((member 1 '(1 2 3 4)) =>
                   (lambda (x) (cdr x))))
                 '(2 3 4)))
  (should (equal (srfi-cond
                  ((member 1 '(1 2 3 4)) =>
                   (gauche-macros-test--generator 3)))
                 '(3 4)))
  ;; test guard => expr
  (should (equal (srfi-cond
                  ((member 1 '(1 2 3 4))
                   (lambda (xs) (= (car xs) 1))  =>
                   (lambda (xs) (mapcar '1+ xs))))
                 '(2 3 4 5)))
  (should (equal (srfi-cond
                  ((member 1 '(1 2 3 4))
                   (lambda (xs) (= (car xs) 2))  =>
                   (lambda (xs) (mapcar '1+ xs))))
                 nil))
  (should (equal (srfi-cond
                  ((member 1 '(1 2 3 4))
                   (gauche-macros-test--generator 1)  =>
                   (gauche-macros-test--generator 4)))
                 '(4)))
  (should (equal (srfi-cond
                  ((member 1 '(1 2 3 4))
                   (gauche-macros-test--generator 5)  =>
                   (gauche-macros-test--generator 4)))
                 nil))
  (should (equal (srfi-cond (t)) nil))

  )

(defun gauche-macros-test1-func1 (l)
  (mapcar
   (lambda (v) (+ v 5))
   l))

(defvar gauche-macros-test1-var1 #'gauche-macros-test1-func1)

(defun gauche-macros-test1-guard1 (l)
  (cl-every #'integerp l))

(defvar gauche-macros-test1-var2 #'gauche-macros-test1-guard1)

(ert-deftest srfi-cond2 ()
  :tags '(gauche-macros)

  (should
   (equal
    (srfi-cond
     ((member 3 '(1 2 3 4 5)) => #'gauche-macros-test1-func1))
    '(8 9 10)))

  (should
   (equal
    (srfi-cond
     ((member 3 '(1 2 3 4 5)) => gauche-macros-test1-var1))
    '(8 9 10)))

  (should
   (equal
    (srfi-cond
     ((member 3 '(1 2 3 4 5)) #'gauche-macros-test1-guard1 => gauche-macros-test1-var1))
    '(8 9 10)))

  (should
   (equal
    (srfi-cond
     ((member 3 '(1 2 3 4 5)) gauche-macros-test1-var2 => gauche-macros-test1-var1))
    '(8 9 10)))

  (should
   (equal
    (srfi-cond
     ((member 3 '(1 2 3.5 4 5)) gauche-macros-test1-var2 => gauche-macros-test1-var1)
     (t 'false))
    'false)))

(provide 'gauche-macros-test)
