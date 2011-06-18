;;;
;;;
(define-module rails.action-view
  (use rails.util)
  (export link-to form-tag form-end text-field submit-tag cont/form-tag
	  link-to% form-tag%))
(select-module rails.action-view)

;; TODO このマクロ定義を自動生成したい
(define-macro (link-to . args)
  `(link-to% *ac-env* ,@args))

(define-macro (form-tag . args)
  `(form-tag% *ac-env* ,@args))
  
(define (link-to% *ac-env* label . options)
  (let-keywords options
      ((action "") (id #f) (confirm #f). rest)
    (let* ((url (string-append "/" (ref *ac-env* 'controller) "/"))
	   (js ""))
      (set! url (string-append url action))
      (if id (set! url (string-append url "/" (number->string (ref id 'id)))))
      (if confirm (set! js " onclick=\"return confirm('Are you sure?');\""))
      #`"<a href=\",url\",|js|> ,label </a>")))

(define (form-tag% *ac-env* . options)
  #`"<form action=\",(build-url *ac-env* options)\" method=\"post\">")

(define (form-end)
  "</form>")

(define (submit-tag label)
  #`"<input type=\"submit\" value=\",label\">")

(define (text-field model attr)
  (if (is-a? model <string>)
      #`"<input type=\"text\" name=\",|model|[,|attr|]\" value=\"\">"
      (let1 model-name (class-name-word->string (class-of model))
	#`"<input type=\"text\" name=\",|model-name|[,|attr|]\" value=\",(slot-ref->string  model (string->symbol attr))\">")))

(define (cont/form-tag cont)
  #`"<input type=\"hidden\" name=\"@cont@\" value=\",cont\">")

(define (build-url  *ac-env* options)
  (let-keywords options
      ((action "") (id #f) . rest)
    (let1 url (string-append "/" (ref *ac-env* 'controller) "/")
      (set! url (string-append url action))
      (if id  
	  (set! url (string-append url "/" (number->string (ref id 'id)))))
      url)))
  

(provide "rails/action-view")

