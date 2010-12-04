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
         (lambda (name env expressions)
           (let ((expr-name (string-append name ":e")))
             (if (pair? expressions)
               (if (eq? '() (cdr expressions))
                 (transform expr-name env (car expressions))
                 (do-transform
                   (transform expr-name env (car expressions))
                   (lambda (result)
                     (transform-begin (string-append name ">") env (cdr expressions)))
                   (lambda (result)
                     `(#f (yume:label
                            (lambda (cps scope)
                              (,result
                                ,(do-transform
                                   (transform-begin (string-append name ">") env (cdr expressions))
                                   (lambda (result)
                                     `(yume:continue-new
                                        (lambda (context result)
                                          (yume:continue-call (yume:continue-cps context) ,result))
                                        cps #f))
                                   (lambda (result)
                                     `(yume:continue-new
                                        (lambda (context result)
                                          (,result
                                            (yume:continue-cps context)
                                            (yume:continue-scope context)))
                                        cps scope)))
                                scope))
                            ,name)))))
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
                      (lambda (cps scope)
                        ,@(do-transform
                            (transform (string-append name ":e") env value)
                            (lambda (result)
                              `(yume:continue-call (yume:continue-cps context) (yume:global-set ,variable ,result)))
                            (lambda (result)
                              `(,result
                                 (yume:continue-new
                                   (lambda (context result)
                                     (yume:continue-call (yume:continue-cps context) (yume:global-set ,variable result)))
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
                         (transform-begin (string-append name ":lambda") (cons args env) expressions)
                         (lambda (result)
                           `(yume:label
                              (lambda (cps scope)
                                (yume:continue-call cps ,result))
                              ,name))
                         (lambda (result) result))
                      ,(length args) #f scope))
               (raise (list "transform-error" "lambda args error" args))))))

       (transform-params
         (lambda (name env params)
           (if (pair? params)
             (do-transform
               (transform-params (string-append name "<") env (cdr params))
               (lambda (result)
                 (do-transform
                   (transform (string-append name ":e") env (car params))
                   (lambda (result-e)
                     `(#t (cons ,result-e ,result)))
                   (lambda (result-e)
                     `(#f (yume:label
                            (lambda (cps scope)
                              (,result-e
                                (yume:continue-new
                                  (lambda (context result)
                                    (yume:continue-call (yume:continue-cps context) (cons result ,result)))
                                  cps #f)
                                scope))
                            ,name)))))
               (lambda (result)
                 `(#f (yume:label
                        (lambda (cps scope)
                          (,result
                            ,(do-transform
                               (transform (string-append name ":e") env (car params))
                               (lambda (result-e)
                                 `(yume:continue-new
                                    (lambda (context result)
                                      (yume:continue-call (yume:continue-cps context) (cons ,result-e result)))
                                    cps #f))
                               (lambda (result-e)
                                 `(yume:continue-new
                                    (lambda (context result)
                                      (,result-e
                                        (yume:continue-new
                                          (lambda (context result)
                                            (yume:continue-call (yume:continue-cps context) (cons result (yume:continue-scope context))))
                                          (yume:continue-cps context) result)
                                        (yume:continue-scope context)))
                                    cps scope))))
                          scope)
                        ,name))))
             (if (null-list? params)
               '(#t '())
               (raise "transform-error" "call params is not list")))))

       (transform-call
         (lambda (name env params)
           `(#f (yume:label
                  (lambda (cps scope)
                    ,(do-transform
                       (transform (string-append name ":fun") env (car params))
                       (lambda (result-fun)
                         (do-transform
                           (transform-params (string-append name "<") env (cdr params))
                           (lambda (result)
                             `(yume:procedure-call ,result-fun cps ,result))
                           (lambda (result)
                             `(,result
                                (yume:continue-new
                                  (lambda (context result)
                                    (yume:procedure-call ,result-fun (yume:continue-cps context) result))
                                  cps #f)
                                scope))))
                       (lambda (result)
                         `(,result
                            ,(do-transform
                               (transform-params (string-append name "<") env (cdr params))
                               (lambda (result)
                                 `(yume:continue-new
                                    (lambda (context result)
                                      (yume:procedure-call result (yume:continue-cps context) ,result))
                                    cps #f))
                               (lambda (result)
                                 `(yume:continue-new
                                    (lambda (context result)
                                      (,result
                                        (yume:continue-new
                                          (lambda (context result)
                                            (yume:procedure-call (yume:continue-scope context) (yume:continue-cps context) result))
                                          (yume:continue-cps context) (yume:continue-scope context))))
                                    cps scope)))
                            scope))))
                  ,name))))

       (transform-variable
         (lambda (name env variable)
           (letrec ((variable-frame-find
                      (lambda (a frame)
                        (cond ((pair? frame)
                               (if (eq? variable (car frame))
                                 (list 'car a)
                                 (variable-frame-find (list 'cdr a) (cdr frame))))
                              (else (if (eq? variable frame)
                                      a
                                      #f)))))
                    (variable-find
                      (lambda (a env)
                        (if (null-list? env)
                          `(yume:global-get ,(list 'quote variable))
                          (let ((r (variable-frame-find (list 'car a) (car env))))
                            (if r
                              r
                              (variable-find (list 'cdr a) (cdr env))))))))
             (list #t (variable-find 'scope env)))))

       (transform-set!
         (lambda (name env variable)
           'set!))

       (transform-if
         (lambda (name env args)
           (let ((test (car args)) (true (cadr args)) (false (caddr args)) (null (cdddr args)))
             (if (eq? '() null)
               (do-transform
                 (transform (string-append name ":test") env test)
                 (lambda (result)
                   (if result
                     (transform (string-append name ":true") env true)
                     (transform (string-append name ":false") env false)))
                 (lambda (result)
                   `(yume:label
                      (lambda (cps scope)
                        (,result
                          (yume:continue-new
                            (lambda (context result)
                              (if result
                                ,(do-transform
                                   (transform (string-append name ":true") env true)
                                   (lambda (result)
                                     `(yume:continue-call (yume:continue-cps cps) ,result))
                                   (lambda (result)
                                     `(,result
                                        (yume:continue-cps context) (yume:continue-scope context))))
                                ,(do-transform
                                   (transform (string-append name ":false") env false)
                                   (lambda (result)
                                     `(yume:continue-call (yume:continue-cps cps) ,result))
                                   (lambda (result)
                                     `(,result
                                        (yume:continue-cps context) (yume:continue-scope context))))))
                            cps scope)
                          scope))
                      ,name)))
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
