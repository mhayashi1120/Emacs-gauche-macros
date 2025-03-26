(defvar sample '((a . "A")))

 ;; Same as Emacs-Lisp cond.
(srfi-cond
 ((assoc 'a sample)
  (list (cdr (assoc 'a sample))))
 ((assoc 'NONE sample)
  (list (cdr (assoc 'NONE sample)))))
;; -> '("A")

 ;; Above is rewrite as following.
(srfi-cond
 ((assoc 'a sample) =>
  (lambda (v) (list (cdr v))))
 ((assoc 'NONE sample) =>
  (lambda (v) (list (cdr v)))))
;; -> '("A")

(srfi-cond
 ((assoc 'b sample) =>
  (lambda (v) (list (cdr v)))))
;; -> nil

;; Guard examples. Guard clause can continue TESTs
(srfi-cond
 ((assoc 'a sample)
  (lambda (v) (string= (cdr v) "B")) =>
  (lambda (v) (list (cdr v))))
 ((assoc 'a sample)
  (lambda (v) (string= (cdr v) "A")) =>
  (lambda (v) (list (cdr v)))))
;; -> ("A")
