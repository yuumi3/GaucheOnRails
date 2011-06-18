#!/usr/bin/env gosh
;;;
;;; 小型httpサーバー
;;;
(use gauche.net) 
(use gauche.reload) 
(use util.match) 
(use file.util)
(use rfc.822) 
(use rfc.uri) 
(use www.cgi) 

(add-load-path "/Users/yy/Project/GaucheRails/lib")  ; ^^);
(reload-modified-modules)

(use rails.active-record)
(use rails.action-controller)
(use rails.action-view)
(use rails.dispatcher)
(use rails.egosh)
(use rails.util)

(define *mini-server-port* 8080)

(define (run-server) 
  (add-load-path "./app/models")
  (add-load-path "./app/controllers")
  (set! *default-template-dir* "./app/views")
  (let1 server-sock (make-server-socket 'inet *mini-server-port* :reuse-addr? #t)
	(print #`"Start mini-server port=,*mini-server-port*")
	(guard (e (else (socket-close server-sock) (raise e)))
	       (let loop ((client (socket-accept server-sock))) 
		 (guard (e (else (socket-close client) (raise e))) 
			(handle-request (get-request (socket-input-port client)) 
					(socket-output-port client)) 
			(socket-close client)) 
		 (loop (socket-accept server-sock)))))) 

(define (get-request iport) 
  (rxmatch-case (read-line iport) 
		 [test eof-object? 'bad-request] 
		 [#/^(GET)\s+(\S+)\s+HTTP\/\d+\.\d+$/ (_ method abs-path) 
		     (list* method abs-path #f (rfc822-header->list iport))] 
		 [#/^(POST)\s+(\S+)\s+HTTP\/\d+\.\d+$/ (_ method abs-path) 
		     (let* ((headers (rfc822-header->list iport))
			    (post-data (string-incomplete->complete
					(read-block 
					 (x->integer (rfc822-header-ref headers "content-length"))
					 iport))))
		       (list* method abs-path post-data headers))] 
		 [#/^[A-Z]+/ () 'not-implemented] 
		 [else 'bad-request])) 

(define (handle-request request oport) 
  (match request 
	 ['bad-request     (display "HTTP/1.1 400 Bad Request\r\n\r\n" oport)] 
	 ['not-implemented (display "HTTP/1.1 501 Not Implemented\r\n\r\n" oport)] 
	 [(method abs-path post-data . headers) 
	  (receive (auth path q frag) (uri-decompose-hierarchical abs-path) 
		   (receive
		    (status mime-type content)
		    (render-content method path 
				    (cgi-parse-parameters :query-string (or post-data q ""))
				    headers)
		    (print #`"   response : ,status ,mime-type size=,(string-size content)")
		    (display #`"HTTP/1.1 ,status\r\n" oport)
		    (display #`"Content-Type: ,mime-type\r\n" oport) 
		    (display #`"Content-Length: ,(string-size content)\r\n" oport) 
		    (display "\r\n" oport) 
		    (display content oport)))]))

(define (render-content method path params headers)
  (cond ((string=? path "/") (render-by-file "/index.html"))
	((#/\.\w+$/ path) (render-by-file path))
	(else 
	 (guard 
	  (e (else (report-error e)
		   (values "200 OK" "text/html; charset=utf-8"
			   "<html><body><h2>Internal error</h2></body></html>")))
	  (print #`"-- application : ,method ,path ,params")
	  (values "200 OK" "text/html; charset=utf-8"
			(dispatch method path params headers))))))

(define (render-by-file path)
  (let ((ext (regexp-replace  #/^.*\.(\w+)$/ path "\\1"))
	(file-path (regexp-replace #/^\/(.*)$/ path "\\1")))
    (cond ((open-input-file file-path :if-does-not-exist #f) =>
	   (lambda(port)
	     (print #`"-- static content: ,file-path")
	     (values "200 OK" (hash-table-get *mini-mime-types* ext)
		     (read-block (file-size file-path) port))))
	  (else 
	   (print #`"++ conten not found: ,file-path")
	   (values "404 Not Found" "text/html"
			"<html><head><title>404 Not Found</title></head><body><h1>Not Found</h1></body></html>")))))

(define (main args)
  (run-server)
  0)


(define *mini-mime-types* (make-hash-table 'string=?))
(hash-table-put! *mini-mime-types* "html" "text/html; charset=utf-8")
(hash-table-put! *mini-mime-types* "htm" "text/html; charset=utf-8")
(hash-table-put! *mini-mime-types* "css" "text/css")
(hash-table-put! *mini-mime-types* "js" "text/javascript")
(hash-table-put! *mini-mime-types* "gif" "image/gif")
(hash-table-put! *mini-mime-types* "gif" "image/gif")
(hash-table-put! *mini-mime-types* "gif" "image/gif")
(hash-table-put! *mini-mime-types* "jpg" "image/jpeg")
(hash-table-put! *mini-mime-types* "jpeg" "image/jpeg")
(hash-table-put! *mini-mime-types* "png" "image/png")

