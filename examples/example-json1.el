(require 'gauche-macros)

;; Useful when constructing json.
(defvar input (make-hash-table))

(puthash 'a "A" input)
(puthash 'b "B" input)
(puthash 'c 10 input)
(puthash 'd [1,2,3] input)
(puthash 'j `((z . "Z") (y . "Y"))  input)

(json-encode
 (cond-list
  (t
   (cons "Required" "Field"))
  ((gethash 'a input) =>
   (lambda (v) (cons "optionalField" v)))
  ((gethash 'not-found input) =>
   (lambda (v) (error "Should not found %s" v)))
  ((gethash 'c input) =>
   (lambda (v) (cons "optionalVector" (make-vector 5 v))))
  ((gethash 'd input) =>
   (lambda (v) (cons "optionalValue" (aref v 0))))
  ((gethash 'j input) =>
   (lambda (v) (cons "optionalChildJson" v)))
  ;; This will expanded to parent json
  ((gethash 'j input) => @
   (lambda (v) v))
  )
 )
;; -> "{\"Required\":\"Field\",\"optionalField\":\"A\",\"optionalVector\":[10,10,10,10,10],\"optionalValue\":1,\"optionalChildJson\":{\"z\":\"Z\",\"y\":\"Y\"},\"z\":\"Z\",\"y\":\"Y\"}"
