#!/usr/bin/env gosh
;;;
;;; Generator
;;;
;;;   rails ApplicationPath
;;;
;;;   script/generator scaffold ModelName [field:type, field:type] 
;;;
(use file.util)
(use srfi-13)

(add-load-path "/Users/yy/Project/GaucheRails/lib")  ; ^^);
(use rails.egosh)
(use rails.util)

(define *template-dir* "/Users/yy/Project/GaucheRails/templates")

(define (main args)
  (if (#/rails$/ (car args))
      (rails (cdr args))
      (generator (cdr args))))

(define (rails args)
  (if (pair? args)
      (if (file-exists? (car args))
	  (usage #`"Error: ,(car args) aready exists")
	  (generate-rails-project (car args)))
      (usage "Usage: rails /path/to/your/app [options]")))


(define (generator args)
  (if (and (pair? args)
	   (string=? (car args) "scaffold")
	   (pair? (cdr args)))
      (generate-scaffold (cdr args))
      (usage "script/generator scaffold ModelName [field:type field:type ...]")))

(define (generate-rails-project path)
  (create-dirs path
   '("app"
     "app/controllers"
     "app/models"
     "app/views"
     "app/views/layouts"
     "script"
     ))
  (copy-files path *template-dir*
   '("app/controllers/application.scm"
     "script/server"
     "script/generate"
     )))

(define (create-dirs top-dir dir-list)
  (for-each 
   (lambda(dir)
     (let1 path (string-append top-dir "/" dir)
       (make-directory* path)
       (print #`"      create  ,path")))
   dir-list))

(define (copy-files top-dir tmpl-dir file-list)
  (for-each 
   (lambda(file)
     (let1 path (string-append top-dir "/" file)
       (copy-file (string-append tmpl-dir "/" file) path :keep-mode #t)
       (print #`"      create  ,path")))
   file-list))

(define (generate-scaffold args)
  (let* ((model (car args))
	 (models (pluralize model))
	 (fields (field-alist (cdr args)))
	 (columns (map (lambda(x) (car x)) fields)))
    (create-dirs "."
		 (list #`"app/views/,models"
		   ))
    (copy-templ-files "." *template-dir*
		      model
		      models
		      columns
		      '("app/controllers/NAMES-controller.scm"
			"app/models/NAME.scm"
			"app/views/NAMES/index.ghtml"
			"app/views/NAMES/show.ghtml"
			"app/views/NAMES/edit.ghtml"
			"app/views/NAMES/new.ghtml"
			))))

(define (copy-templ-files top-dir tmpl-dir name names columns file-list)
  (set! *default-template-dir* tmpl-dir)
  (for-each 
   (lambda(file)
     (let* ((dst-file (regexp-replace #/NAME/ (regexp-replace #/NAMES/ file names) name))
	    (dst-path (string-append top-dir "/" dst-file))
	    (tmpl (compile-template-file file))
	    (contents
	     ((eval `(lambda(name names columns) ,(caddr tmpl))()) name names columns))
	    (contents
	     (regexp-replace-all #/\[%/ (regexp-replace-all #/%\]/ contents "%>") "<%")))
       (call-with-output-file dst-path (lambda(port) (display contents port)))
       (print #`"      create  ,dst-path")))
   file-list))

(define (usage msg)
  (print msg)
  (exit))

(define (field-alist lst)
  (map (lambda (x) 
	 (cond ((#/^(.+):(.+)$/ x) => (lambda(m) (cons (m 1) (m 2))))
	       (else ())))
       lst))

(define (pluralize s)
  (string-append s "s"))
