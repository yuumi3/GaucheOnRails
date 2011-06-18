;;;
;;;
(define-module rails.action-controller
  (use www.cgi) 
  (use rails.egosh)
  (use rails.action-view)
  (use rails.util)
  (export <action-controller> define-action params render-template param-hash redirect define-continuation get-cont))
(select-module rails.action-controller)

(define *conts* (make-hash-table 'eqv?))

(define-class <action-controller> ()
  ((controller :init-keyword :controller :init-value "")
    (params :init-keyword :params :init-value ())
    (id :init-keyword :id :init-value #f)))

(define-macro (define-action name . body)
  (let ((tmpl-vars (scan-template-params body)))
    `(define (,name *ac-env*)
       (let (,@(map (lambda(x) (list x #f)) tmpl-vars) (*redirect-url* #f) (@cont@ #f))
	 ,@body
	 (cond (*redirect-url* (render-redirect *redirect-url*))
	       (@cont@
		(render-template (tmpl-file-name *ac-env* ',name)
				 *ac-env*
				  ',(cons '@cont@ tmpl-vars)
				  ,(cons 'list (cons '@cont@ tmpl-vars))))
	       (else
		(render-template (tmpl-file-name *ac-env* ',name)
				 *ac-env*
				 ',tmpl-vars
				 ,(cons 'list tmpl-vars))))))))

(define-macro (params id)
  (if (eq? id :id) 
      `(ref *ac-env* 'id)
      `(param-hash (ref *ac-env* 'params) ,(keyword->string id)))) 

(define-macro (redirect . options)
  (let-keywords options ((action "") . rest)
    `(set! *redirect-url* 
	   (string-append "/" (ref *ac-env* 'controller) "/" (symbol->string ,action)))))

(define-macro (define-continuation name . func)
  (let ((tmpl-vars (scan-template-params func)))
  `(set! @cont@
	 (push-cont! 
	  (lambda(*ac-env*)
	    (let ((*redirect-url* #f) (@cont@ #f))
	    ,@func
	    (cond (*redirect-url* (render-redirect *redirect-url*))
		  (@cont@
		   (render-template (tmpl-file-name *ac-env* ',name)
				    *ac-env*
				    ',(cons '@cont@ tmpl-vars)
				    ,(list (cons '@cont@ tmpl-vars))))
		  (else
		   (render-template (tmpl-file-name *ac-env* ',name)
				    *ac-env*
				    ',tmpl-vars
				    ,(list tmpl-vars))))))))))

(define-macro (flash . options)
  #f)

(define (param-hash param-pair key)
  (let ((param-hash (make-hash-table))
	(param-pattn (string->regexp #`"^,|key|\\[(\\w+)\\]$")))
    (for-each (lambda(p)
		(cond ((rxmatch param-pattn (car p)) => 
		       (lambda(m) (hash-table-put! param-hash (make-keyword (m 1)) (cadr p))))
		      (else #f)))
	      param-pair)
    param-hash))

(define (render-template tmpl-file *ac-env* tmpl-vars tmpl-values . options)
  (print #`"   render template: ,tmpl-file")
  (let-keywords options ((cont-id #f) . rest)
    (let1 tmpl (compile-template-file tmpl-file)
      (unless (equal? (sort-symbol (cdr (cadr tmpl))) (sort-symbol tmpl-vars))
	(error "Template variable missmatch in " tmpl-file))
      (apply (eval tmpl ())
	     *ac-env* 
	     (reorder tmpl-values tmpl-vars (cdr (cadr tmpl)))))))

(define (render-redirect url)
  (print #`"   redirect: ,url")
  #`"<html><head><meta http-equiv=\"refresh\" content=\"0;url=,url\"></head><body></body></html>")

(define (get-cont params)
  (hash-table-get *conts* 
		  (cgi-get-parameter "@cont@" params :convert string->number)
		  #f))

(define (push-cont! cont) 
  (let1 cid (random-64bit-integer)
    (cond ((hash-table-get *conts* cid #f) (push-cont! cont))
          (else (hash-table-put! *conts* cid cont) cid)))) 

(define (tmpl-file-name *ac-env* name)
  (string-append (ref *ac-env* 'controller) "/" (symbol->string name)))

(provide "rails/action-controller")
