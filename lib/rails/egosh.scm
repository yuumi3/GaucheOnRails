;;;
(define-module rails.egosh
  (use rails.util)
  (use file.util)
  (export compile-template-file compile-template *default-template-dir* *default-template-ext*))
(select-module rails.egosh)

(define *default-template-dir* ".")
(define *default-template-ext* ".ghtml")

(define (compile-template-file tmpl-file)
  (compile-template 
   (file->string
    (if (#/\.\w+$/ tmpl-file)
	(string-append *default-template-dir* "/" tmpl-file)
	(string-append *default-template-dir* "/" tmpl-file *default-template-ext*)))))
	 
(define (compile-template template)
  (let ((port (open-output-string))
	(compiled ()))
    (define (compile-elem templ)
      (cond ((string=? templ "") #t)
	    ((#/^<%=(.+?)%>/ templ) =>
	     (lambda(m)
	       (display #`"(display (x->string ,(m 1)) *p*)" port)
	       (compile-elem (m 'after))))
	    ((#/^<%(.+?)%>/ templ) =>
	     (lambda(m)
	       (display #`",(m 1)" port)
	       (compile-elem (m 'after))))
	    ((#/^(.+?)<%/ templ) =>
	     (lambda(m)
	       (display #`"(display \",(esc-q (m 1))\" *p*)" port)
	       (compile-elem (string-append "<%" (m 'after)))))
	    (else (display #`"(display \",(esc-q templ)\" *p*)" port))))
    (display "(call-with-output-string (lambda(*p*)" port)
    (compile-elem template)
    (display "))" port)
    (set! compiled (read-from-string (get-output-string port)))
    `(lambda ,(cons '*ac-env* (scan-template-params compiled)) ,compiled)))
;    `(lambda ,(scan-template-params compiled) ,compiled)))

(define (esc-q s)
  (regexp-replace-all #/\"/ s "\\\\\""))

(provide "rails/egosh")



