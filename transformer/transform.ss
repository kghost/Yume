#lang scheme

(require srfi/1)

(define transform
  (lambda (p)
    (letrec
        ((transform-begin
          (lambda (name env expressions)
            (let ((expr-name (string-append name ":e")))
              (if (pair? expressions)
                  `(yume:label
                    (lambda (cps scope)
                      (,(transform expr-name env (car expressions))
                       ,(if (null-list? (cdr expressions))
                            'cps
                            `(yume:continue-new
                              (lambda (context result)
                                (,(transform-begin (string-append name ">") env (cdr expressions))
                                 (yume:continue-cps context) (yume:continue-scope context)))
                              cps scope))
                       scope))
                    ,name)
                  (raise (list "transform-error" "begin is not list" expressions))))))
         
         (transform-quote
          (lambda (name env expression)
            (if (and (pair? expression) (null-list? (cdr expression)))
                `(yume:label (lambda (cps scope) (yume:continue-call cps ,(car expression))) ,name)
                (raise (list "transform-error" "quote" expression)))))
         
         (transform-define
          (lambda (name env define)
            (let ((variable (car define)) (value (cadr define)) (null (cddr define)))
              (if (null-list? null)
                  `(yume:label
                    (lambda (cps scope)
                      (,(transform (string-append name ":e") env value)
                       (yume:continue-new
                        (lambda (context resulte)
                          (yume:global-set ,variable result)
                          (yume:continue-call (yume:continue-cps context) (yume:undefined)))
                        cps #f)
                       scope))
                    ,name)
                  (raise (list "transform-error" "define" define))))))
         
         (transform-instant
          (lambda (name env expression)
            `(yume:label (lambda (cps scope) (yume:continue-call cps ,expression)) ,name)))
         
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
                  `(yume:label
                    (lambda (cps scope)
                      (yume:continue-call
                       cps
                       (yume:procedure-new
                        (lambda (cps scope)
                          (,(transform-begin (string-append name ":lambda") (cons args env) expressions) cps scope))
                        ,(length args) #f scope)))
                    ,name)
                  (raise (list "transform-error" "lambda args error" args))))))
         
         (transform-params
          (lambda (name env params)
            (if (pair? params)
                `(yume:label
                  (lambda (cps scope)
                    (,(transform-params (string-append name "<") env (cdr params))
                     (yume:continue-new
                      (lambda (context result)
                        (,(transform (string-append name ":e") env (car params))
                         (yume:continue-new
                          (lambda (context result)
                            (yume:continue-call (yume:continue-cps context) (cons result (yume:continue-scope context))))
                          (yume:continue-cps context) result)
                         (yume:continue-scope context)))
                      cps scope)
                     scope))
                  ,name)
                (if (null-list? params)
                    `(yume:label (lambda (cps scope) (yume:continue-call cps '()))
                                 ,name)
                    (raise "transform-error" "call params is not list")))))
         
         (transform-call
          (lambda (name env params)
            `(yume:label (lambda (cps scope)
                           (,(transform-params (string-append name "<") env params)
                            (yume:continue-new
                             (lambda (context result)
                               (yume:procedure-call (car result) (yume:continue-cps context) (cdr result)))
                             cps #f)
                            scope))
                         ,name)))
         
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
              `(yume:label
                (lambda (cps scope)
                  (yume:continue-call cps ,(variable-find 'scope env)))
                ,name))))
         
         (transform
          (lambda (name env expression)
            (cond ((pair? expression)
                   (let ((op (car expression))(args (cdr expression)))
                     (cond ((eq? op 'quote) (transform-quote name env args))
                           ((eq? op 'define) (transform-define name env args))
                           ((eq? op 'lambda) (transform-lambda name env args))
                           (else (transform-call name env expression)))))
                  ((symbol? expression) (transform-variable name env expression))
                  (else (transform-instant name env expression)))))
         )
      
      (list (transform-begin "g" '() p) '(yume:continue-new (lambda (continue result) (write result)) #f #f) ''()))))

(transform '((quote x)))