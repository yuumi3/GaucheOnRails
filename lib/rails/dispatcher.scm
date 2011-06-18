(define-module rails.dispatcher
  (use rails.action-controller)
  (export dispatch))
(select-module rails.dispatcher)

(define (dispatch meth path params headers)
  (cond ((#/^\/(\w+)\/(\w+)(\/(\d+))?/ path) =>
	 (lambda(maps)
	   (cond ((get-cont params) =>
		  (lambda (func) (func (action-controller-env (maps 1) params (maps 4)))))
		 ((action-controller-method (maps 1) (maps 2)) =>
		  (lambda(method)
		    (method (action-controller-env (maps 1) params (maps 4)))))
		 (else (error-html "Method not found")))))
	 (else (error-html "Illegal controller/action path"))))

(define (action-controller-method controller-s method-s)
  (let ((controller (string->symbol (string-append controller-s "-controller")))
	(method (string->symbol method-s)))
    (guard (e (else (report-error e) #f))
	   (unless (find-module controller) (eval `(use ,controller) ()))
	   (global-variable-ref controller method))))

(define (action-controller-env controller params id)
  (if id
      (make <action-controller> :controller controller :params params :id (string->number id))
      (make <action-controller> :controller controller :params params)))

(define (error-html msg)
  #`"<html><head><title>error</title><body><h2>,|msg|</h2></body></html>")

(provide "rails/dispatcher")