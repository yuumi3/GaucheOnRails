;;;
;;;
(define-module <%=names%>-controller
  (use rails.active-record)
  (use rails.action-controller)
  (use rails.action-view)
  (use <%=name%>)
  (extend application)
  (export index show new edit))
(select-module <%=names%>-controller)

(define-action index
  (set! @<%=names%> (find <<%=name%>> :all)))

(define-action show
  (set! @<%=name%> (find <<%=name%>> (params :id))))

(define-action destory
    (delete (find <<%=name%>> (params :id)))
  (redirect :action 'index))

(define-action new
  (define-continuation new
    (when (save (make <<%=name%>> (params :<%=name%>)))
      (flash :notice "<%=name%> was successfully created.")
      (redirect :action 'index))))

(define-action edit
  (set! @<%=name%> (find <<%=name%>> (params :id)))
  (define-continuation edit
   (when (update_attributes @<%=name%> (params :<%=name%>))
     (flash :notice "<%=name%> was successfully updated.")
     (redirect :action 'index))))

(provide "<%=names%>-controller")

