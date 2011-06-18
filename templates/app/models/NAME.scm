;;;
;;;
;;;
(define-module <%=name%>
  (use rails.active-record)
  (export <<%=name%>>))
(select-module <%=name%>)

(define-class <<%=name%>> (<active-record>) ())

(provide "<%=name%>")
