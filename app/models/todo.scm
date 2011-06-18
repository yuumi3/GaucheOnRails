;;;
;;;
;;;
(define-module todo
  (use rails.active-record)
  (export <todo>))
(select-module todo)

(define-class <todo> (<active-record>) ())

(provide "todo")
