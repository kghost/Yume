(use-modules (srfi srfi-1))

(define transform
  (lambda (p)
    (letrec
      ((do-transform
	 (lambda (result inline non-inline)
	   (if (car result)
	     (inline (cadr result))
	     (non-inline (cadr result)))))

       (transform-begin
	 (lambda (name index env expressions)
	   (let ((expr-name (string-append name (number->string index) "_e")))
	     (if (pair? expressions)
	       (if (eq? '() (cdr expressions))
		 (transform expr-name env (car expressions))
		 (do-transform
		   (transform expr-name env (car expressions))
		   (lambda (result)
		     (transform-begin name (+ index 1) env (cdr expressions)))
		   (lambda (result)
		     `(#f (yume:label
			    (yume:lambda-cps
			      ,(string-append name (number->string index))
			      (cps scope)
			      (,result
				,(do-transform
				   (transform-begin name (+ index 1) env (cdr expressions))
				   (lambda (result)
				     `(yume:continue-new
					(yume:lambda-continue
					  ,(string-append name (number->string index))
					  (cps scope result)
					  (yume:continue-call cps ,result))
					cps #f))
				   (lambda (result)
				     `(yume:continue-new
					(yume:lambda-continue
					  ,(string-append name (number->string index))
					  (cps scope result)
					  (,result cps scope))
					cps scope)))
				scope))
			    ,(string-append name (number->string index)))))))
	       (raise (list "transform-error" "begin is not list" expressions))))))

       (transform-quote
	 (lambda (name env expression)
	   (if (and (pair? expression) (null-list? (cdr expression)))
	     `(#t (quote ,(car expression)))
	     (raise (list "transform-error" "quote" expression)))))

       (transform-define
	 (lambda (name env define)
	   (let ((variable (car define)) (value (cadr define)) (null (cddr define)))
	     (if (null-list? null)
	       `(#f (yume:label
		      (yume:lambda-cps
			,name
			(cps scope)
			,(do-transform
			   (transform (string-append name "_e") env value)
			   (lambda (result)
			     `(yume:continue-call cps (yume:global-add ,variable ,result)))
			   (lambda (result)
			     `(,result
				(yume:continue-new
				  (yume:lambda-continue
				    ,name
				    (cps scope result)
				    (yume:continue-call cps (yume:global-add ,variable result)))
				  cps #f)
				scope))))
		      ,name))
	       (raise (list "transform-error" "define" define))))))

       (transform-instant
	 (lambda (name env expression)
	   `(#t ,expression)))

       (transform-lambda
	 (lambda (name env params)
	   (let ((args (car params)) (expressions (cdr params)))
	     (if (letrec ((every ; return true if all element in list satisfy f, even if list is not a list
			    (lambda (f list)
			      (if (pair? list)
				(and (f (car list)) (every f (cdr list)))
				(if (null-list? list)
				  #t
				  (f list))))))
		   (every symbol? args))
	       `(#t (yume:procedure-new
		      ,(do-transform
			 (transform-begin (string-append name "_f_s") 0 (cons args env) expressions)
			 (lambda (result)
			   `(yume:label
			      (yume:lambda-cps
				,name
				(cps scope)
				(yume:continue-call cps ,result))
			      ,name))
			 (lambda (result) result))
		      scope ,(length args) #f))
	       (raise (list "transform-error" "lambda args error" args))))))

       (transform-params
	 (lambda (name index env params)
	   (if (pair? params)
	     (do-transform
	       (transform-params name (+ index 1) env (cdr params))
	       (lambda (result)
		 (do-transform
		   (transform (string-append name (number->string index) "_e") env (car params))
		   (lambda (result-e)
		     `(#t (yume:cons ,result-e ,result)))
		   (lambda (result-e)
		     `(#f (yume:label
			    (yume:lambda-cps
			      ,(string-append name (number->string index))
			      (cps scope)
			      (,result-e
				(yume:continue-new
				  (yume:lambda-continue
				    ,(string-append name (number->string index))
				    (cps scope result)
				    (yume:continue-call cps (yume:cons result ,result)))
				  cps #f)
				scope))
			    ,(string-append name (number->string index)))))))
	       (lambda (result)
		 `(#f (yume:label
			(yume:lambda-cps
			  ,(string-append name (number->string index))
			  (cps scope)
			  (,result
			    ,(do-transform
			       (transform (string-append name (number->string index) "_e") env (car params))
			       (lambda (result-e)
				 `(yume:continue-new
				    (yume:lambda-continue
				      ,(string-append name (number->string index))
				      (cps scope result)
				      (yume:continue-call cps (yume:cons ,result-e result)))
				    cps #f))
			       (lambda (result-e)
				 `(yume:continue-new
				    (yume:lambda-continue
				      ,(string-append name (number->string index) "_1")
				      (cps scope result)
				      (,result-e
					(yume:continue-new
					  (yume:lambda-continue
					    ,(string-append name (number->string index) "_2")
					    (cps scope result)
					    ; borrow scope to pass result
					    (yume:continue-call cps (yume:cons result scope)))
					  cps result)
					scope))
				    cps scope))))
			  scope)
			,(string-append name (number->string index))))))
	     (if (null-list? params)
	       '(#t (yume:quote ()))
	       (raise "transform-error" "call params is not list")))))

       (transform-call
	 (lambda (name env params)
	   `(#f (yume:label
		  (yume:lambda-cps
		    ,name
		    (cps scope)
		    ,(do-transform
		       (transform (string-append name "_call") env (car params))
		       (lambda (result-fun)
			 (do-transform
			   (transform-params (string-append name "_p") 0 env (cdr params))
			   (lambda (result)
			     `(yume:procedure-call ,result-fun cps ,result))
			   (lambda (result)
			     `(,result
				(yume:continue-new
				  (yume:lambda-continue
				    ,name
				    (cps scope result)
				    (yume:procedure-call ,result-fun cps result))
				  cps #f)
				scope))))
		       (lambda (result)
			 `(,result
			    ,(do-transform
			       (transform-params (string-append name "_p") 0 env (cdr params))
			       (lambda (result)
				 `(yume:continue-new
				    (yume:lambda-continue
				      ,name
				      (cps scope result)
				      (yume:procedure-call result cps ,result))
				    cps #f))
			       (lambda (result)
				 `(yume:continue-new
				    (yume:lambda-continue
				      ,(string-append name "_1")
				      (cps scope result)
				      (,result
					(yume:continue-new
					  (yume:lambda-continue
					    ,(string-append name "_2")
					    (cps scope result)
					    ; borrow scope to pass procedure being called
					    (yume:procedure-call scope cps result))
					  cps result)))
				    cps scope)))
			    scope))))
		  ,name))))

       (transform-variable
	 (lambda (name env variable)
	   (letrec ((variable-frame-find
		      (lambda (a frame)
			(cond ((pair? frame)
			       (if (eq? variable (car frame))
				 (list 'yume:car a)
				 (variable-frame-find (list 'yume:cdr a) (cdr frame))))
			      (else (if (eq? variable frame)
				      a
				      #f)))))
		    (variable-find
		      (lambda (a env)
			(if (null-list? env)
			  `(yume:global-get ,variable)
			  (let ((r (variable-frame-find (list 'yume:car a) (car env))))
			    (if r
			      r
			      (variable-find (list 'yume:cdr a) (cdr env))))))))
	     (list #t (variable-find 'scope env)))))

       (transform-set!
	 (lambda (name env variable)
	   'set!))

       (transform-if
	 (lambda (name env args)
	   (let ((test (car args)) (true (cadr args)) (false (caddr args)) (null (cdddr args)))
	     (if (eq? '() null)
	       (do-transform
		 (transform (string-append name "_test") env test)
		 (lambda (result)
		   (if result
		     (transform (string-append name "_true") env true)
		     (transform (string-append name "_false") env false)))
		 (lambda (result)
		   `(#f (yume:label
			  (yume:lambda-cps
			    ,name
			    (cps scope)
			    (,result
			      (yume:continue-new
				(yume:lambda-continue
				  ,name
				  (cps scope result)
				  (if result
				    ,(do-transform
				       (transform (string-append name "_true") env true)
				       (lambda (result)
					 `(yume:continue-call (yume:continue-cps cps) ,result))
				       (lambda (result)
					 `(,result cps scope)))
				    ,(do-transform
				       (transform (string-append name "_false") env false)
				       (lambda (result)
					 `(yume:continue-call (yume:continue-cps cps) ,result))
				       (lambda (result)
					 `(,result cps scope)))))
				cps scope)
			      scope))
			  ,name))))
	       (raise (list "transform-error" "syntax error: if" args))))))

       (transform
	 (lambda (name env expression)
	   (cond ((pair? expression)
		  (let ((op (car expression))(args (cdr expression)))
		    (cond ((eq? op 'quote) (transform-quote name env args))
			  ((eq? op 'define) (transform-define name env args))
			  ((eq? op 'lambda) (transform-lambda name env args))
			  ((eq? op 'if) (transform-if name env args))
			  (else (transform-call name env expression)))))
		 ((symbol? expression) (transform-variable name env expression))
		 (else (transform-instant name env expression)))))
       )

      (cadr (transform "g" '() p)))))
