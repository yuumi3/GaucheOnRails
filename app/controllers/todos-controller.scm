;;;
;;;
(define-module todos-controller
  (use rails.active-record)
  (use rails.action-controller)
  (use rails.action-view)
  (use todo)
  (extend application-controller)
  (export index show new edit))
(select-module todos-controller)

(define-action index
  (set! @todos (find <todo> :all)))

(define-action show
  (set! @todo (find <todo> (params :id))))

(define-action destory
    (delete (find <todo> (params :id)))
  (redirect :action 'index))

(define-action new
  (define-continuation new
    (when (save (make <todo> (params :todo)))
      (flash :notice "Todo was successfully created.")
      (redirect :action 'index))))

(define-action edit
  (set! @todo (find <todo> (params :id)))
  (define-continuation edit
   (when (update_attributes @todo (params :todo))
     (flash :notice "Todo was successfully updated.")
     (redirect :action 'index))))

;(define-action edit
;  (set! @todo (find <todo> (params :id))))

;(define-action update
;  (update_attributes (find <todo> (params :id)) (params :todo))
;  (redirect :action 'index))

(provide "todos-controller")

