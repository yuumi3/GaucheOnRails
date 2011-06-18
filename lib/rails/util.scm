;;;
(define-module rails.util
  (use srfi-1)
  (use srfi-19)
  (use srfi-27)

  (export scan-template-params template-var? x->symbol x->string slot-ref->string
	  sort-symbol reorder pair-list random-64bit-integer class-name-word->string))
(select-module rails.util)

(define *max-cid* (expt 2 64))

(define (scan-template-params tree)
  (define (find-all-tmpl-params tree)
    (cond ((pair? tree)
	   (append (scan-template-params (car tree)) (scan-template-params (cdr tree))))
	  ((template-var? tree) (list tree))
	  (else ())))
  (delete-duplicates (find-all-tmpl-params tree)))

(define (template-var? s)
  (and (symbol? s) 
       (#/^@\w/ (symbol->string s))))

(define-method x->string ((e <date>))   (date->string e "~Y-~m-~d"))

(define-method x->symbol((s <string>)) (string->symbol s))
(define-method x->symbol ((s <keyword>)) (string->symbol (keyword->string s)))

(define (slot-ref->string obj slot)
  (if (slot-bound? obj slot)
      (x->string (slot-ref obj slot))
      ""))

(define (sort-symbol lst) 
  (sort lst 
	(lambda(x y) (string<? (symbol->string x) (symbol->string y)))))

(define (reorder values order-from order-to)
  (let1 value-pair (pair-list order-from values)
    (map (lambda(v) (cdr (assq v value-pair))) order-to)))

(define (pair-list key-list value-list)
  (if (null? key-list)
      ()
      (acons (car key-list) (car value-list) (pair-list (cdr key-list)(cdr value-list)))))

(define (random-64bit-integer)
  (random-integer *max-cid*))

(define (class-name-word->string klass)
  (regexp-replace #/<(.+)>/ (symbol->string (class-name klass)) "\\1"))

(provide "rails/util")
