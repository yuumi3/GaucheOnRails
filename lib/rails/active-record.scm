;;;
;;;
(define-module rails.active-record
  (use dbi)
  (use srfi-1)
  (use srfi-13)
  (use srfi-19)
  (use gauche.collection)
  (use gauche.sequence)
  (use rails.util)
  (export <active-record> <active-record-meta> make find save update_attributes delete))
(select-module rails.active-record)


(define-class <active-record-meta> (<class>)
  ())

(define-class <active-record> ()
  ((initialized  :allocation :each-subclass)
   (db-conn :allocation :class)
   (table-name :allocation :each-subclass)
   (column-names :allocation :each-subclass)
   (column-types :allocation :each-subclass))
  :metaclass <active-record-meta>)

(define-method make ((klass <active-record-meta>) . initargs)
  (cond ((class-slot-bound? klass 'initialized) (next-method))
	(else
	 (receive (table-name column-names column-types) (table-info klass)
		  (set! klass (eval (redefine-slot klass column-names) (interaction-environment)))
		  (set! (class-slot-ref klass 'table-name) table-name)
		  (set! (class-slot-ref klass 'column-names) column-names)
		  (set! (class-slot-ref klass 'column-types) column-types)
		  (set! (class-slot-ref klass 'initialized)  #t)
		  (apply next-method (cons klass initargs))))))
		  
(define (class->table-name klass)
  (regexp-replace-all #/-/ (string-append (class-name-word->string klass) "s") "_"))

(define (table-info klass)
  (unless (class-slot-bound? klass 'db-conn)
	  (set! (class-slot-ref klass 'db-conn) (db-connect)))
  (let* ((table-name (class->table-name klass)))
    (receive (column-names column-types) 
	     (db-column-info (class-slot-ref klass 'db-conn) table-name)
	     (values table-name column-names column-types))))

(define (redefine-slot klass slots-names)
  (let* ((slots (map (lambda(n)
		       `(,(string->symbol n) :init-keyword ,(make-keyword n)
			 :accessor ,(string->symbol #`",|n|-of")))
		     slots-names)))
    (eval `(define-class ,(ref klass 'name) 
	     (,(ref (car (class-direct-supers klass)) 'name)) ,slots)
	  (interaction-environment))))

(define-method db-conn ((klass <active-record-meta>))
  (ar-slot-ref klass 'db-conn))

(define-method table-name  ((klass <active-record-meta>))
  (ar-slot-ref klass 'table-name))

(define-method ar-slot-ref ((klass <active-record-meta>)(name <symbol>))
  (if (class-slot-bound? klass 'initialized)
      (class-slot-ref klass name)
      (slot-ref (make klass) name)))
  	   
(define-method find ((klass <active-record-meta>)(id <integer>))
  (let* ((sql #`"select * from ,(table-name klass) where id = ?")
	 (query (dbi-prepare (db-conn klass) sql))
	 (row (first-row (dbi-execute query id))))
    (print #`"   sql: ,sql (id = ,id)")
    (set-ar-columns! (make klass) row)))

(define-method find ((klass <active-record-meta>)(key <keyword>))
  (let* ((sql #`"select * from ,(table-name klass)")
	 (query (dbi-prepare (db-conn klass) sql))
	 (rows (dbi-execute query)))
    (print #`"   sql: ,sql")
    (map (lambda(r) (set-ar-columns! (make klass) r)) rows)))


(define-method first-row ((self <collection>))
  (find (lambda(n) #t) self))

(define (set-ar-columns! ar row)
  (for-each (lambda(v n t)
	      (if v
		  (set! (ref ar (string->symbol n)) 
			(db-value->scheme-object v t))))
	    row
	    (ref ar 'column-names)
	    (ref ar 'column-types))
  ar)

(define-method save ((ar <active-record>)) 
  (cond ((slot-bound? ar 'id)
	 (set! (ref ar 'updated_at) (current-date))
	 (db-update (ref ar 'db-conn)
		    (ref ar 'table-name)
		    (ar->string-hash ar)
		    #`"id = ,(ref ar 'id)"))
	(else 
	 (set! (ref ar 'created_at) (current-date))
	 (db-insert (ref ar 'db-conn)
		    (ref ar 'table-name)
		    (ar->string-hash ar))))
  #t)

(define-method update_attributes ((ar <active-record>)(params <hash-table>))
  (update-slot-by-hash ar params)
  (save ar))

(define-method make ((klass <active-record-meta>)(params <hash-table>))
  (let1 ar (make klass)
    (update-slot-by-hash ar params)
    ar))

(define-method delete ((ar <active-record>))
  (db-delete (ref ar 'db-conn) (ref ar 'table-name) #`"id = ,(ref ar 'id)"))

(define (update-slot-by-hash ar hash)
  (hash-table-for-each
   hash
   (lambda(id value)
     (slot-set! ar (x->symbol id)
		(db-value->scheme-object value (get-column-type ar (x->symbol id)))))))

(define (ar->string-hash ar)
  (let1 hash (make-hash-table 'string=?)
    (for-each
     (lambda(n t)
       (if (slot-bound? ar (string->symbol n))
	   (hash-table-put! hash n
			    (scheme-object->db-value (ref ar (string->symbol n)) t))))
     (ref ar 'column-names)
     (ref ar 'column-types))
    hash))

(define (get-column-type ar column)
  (let1 ix (find-index (lambda(c) (string=? c (symbol->string column))) (ref ar 'column-names))
    (if ix (ref (ref ar 'column-types) ix) #f)))

  
;;;
;;; RDB interface
;;;

(define *data-source-name* "dbi:mysql:db=yy")
(define (db-connect)
  (let ((conn (dbi-connect *data-source-name*)))
    (dbi-do conn "SET NAMES utf8")
    conn))

(define (db-column-info conn table-name)
  (let* ((query (dbi-prepare conn "select column_name,data_type from information_schema.columns where table_name = ? order by ordinal_position"))
	 (result (dbi-execute query table-name)))
    (values
     (list->vector (map (lambda(row) (ref row 0)) result))
     (list->vector (map (lambda(row) (ref row 1)) (ref result 'rows)))
     )))

(define (db-insert db-conn table hash)
  (let1 sql
      #`"insert into ,table(,(string-join (hash-table-keys hash) \",\")) values(,(string-join (hash-table-values hash) \",\"))"
    (print #`"   sql: ,sql")
    (dbi-do db-conn sql)))
   
(define (db-update db-conn table hash where)
  (let* ((assign (string-join
		  (hash-table-map hash (lambda(c v) (string-append c  "=" v)))
		  ","))
	 (sql #`"update ,table set ,assign where ,where"))
    (print #`"   sql: ,sql")
    (dbi-do db-conn sql)))

(define (db-delete db-conn table where)
  (let1	sql #`"delete from ,table where ,where"
    (print #`"   sql: ,sql")
    (dbi-do db-conn sql)))

(define *db-value->scheme-object-func* (make-hash-table 'string=?))
(hash-table-put! *db-value->scheme-object-func* "int" string->number)
(hash-table-put! *db-value->scheme-object-func* "varchar" cons*)
(hash-table-put! *db-value->scheme-object-func* "text" cons*)
(hash-table-put! *db-value->scheme-object-func* "date" (lambda(s) (string->date s "~Y-~m-~d")))
(hash-table-put! *db-value->scheme-object-func* "datetime" (lambda(s) (string->date s "~Y-~m-~d ~H:~M:~S")))

(define (single-quote s) (string-append "'" s "'"))

(define *scheme-object->db-value-func* (make-hash-table 'string=?))
(hash-table-put! *scheme-object->db-value-func* "int" number->string)
(hash-table-put! *scheme-object->db-value-func* "varchar" single-quote)
(hash-table-put! *scheme-object->db-value-func* "text" single-quote)
(hash-table-put! *scheme-object->db-value-func* "date"
		 (lambda(s) (single-quote (date->string s "~Y-~m-~d"))))
(hash-table-put! *scheme-object->db-value-func* "datetime"
		 (lambda(s) (single-quote (date->string s "~Y-~m-~d ~H:~M:~S"))))


(define (db-value->scheme-object value type)
  (cond ((ref *db-value->scheme-object-func* type #f) => (lambda(conv) (conv value)))
	(else value)))

(define (scheme-object->db-value value type)
  (cond ((ref *scheme-object->db-value-func* type #f) => (lambda(conv) (conv value)))
	(else value)))


(provide "rails/active-record")
