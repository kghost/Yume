(use-modules (srfi srfi-1))
(use-modules (ice-9 rw))

(define write-string write-string/partial)

(define compile
  (lambda (output p)
    (letrec ((tokens 
               `((yume:label
                   ,(lambda (p)
                      (let ((name (caddr p)) (fun (cadr p)))
                        (lambda ()
                          (let ((inner ((compile fun))))
                            (write-string "var " output)
                            (write-string name output)
                            (write-string " = function (cps, scope) {" output)
                            (newline output)
                            (inner)
                            (newline output)
                            (write-string "};" output)
                            (newline output))
                          (lambda () (write-string name))))))

                 (lambda
                   ,(lambda (p)
                      (if (null-list? (cdddr p))
                        (compile (caddr p))
                        (raise (list "internal-compile-error" p "contains multiple statements")))))

                 ))

             (compile
               (lambda (p)
                 (cond ((pair? p)
                        (let ((token (assq (car p) tokens)))
                          (if token
                            ((cadr token) p)
                            (lambda ()
                              (let ((fun ((compile (car p))))
                                    (params (reverse (fold
                                                       (lambda (p tail)
                                                         (cons
                                                           ((compile p))
                                                           tail))
                                                       '()
                                                       (cdr p)))))
                                (lambda ()
                                  (fun)
                                  (write-char #\( output)
                                  (pair-fold
                                    (lambda (param tail)
                                      (or (eq? param params) (write-string ", " output))
                                      ((car param)))
                                    '()
                                    params)
                                  (write-string ")" output)))))))
                       (else
                         (lambda ()
                           (lambda ()
                             (cond
                               ((eq? p '()) (write-string "yume:null-list")) ; FIXME: null-list should only appear inside quote
                               ((symbol? p) (write-string (symbol->string p) output))
                               ((boolean? p) (write-string (if p
                                                             "yume:boolean-true"
                                                             "yume:boolean-false")))
                               ((number? p) (write-string "yume:number-new(") (write p) (write-char #\)))
                               ((char? p) (write-string "yume:char-new(") (write-char p) (write-char #\)))
                               ((string? p) (write-string "yume:string-new(\"") (write-string p) (write-string "\")")) ; XXX: if string contains " ?!
                               ((vector? p) (raise "TODO"))
                               (else (raise (list "internal-compile-error" "unknown data" p)))))))))))

      (((compile p))))))

(compile (current-output-port) '(yume:label
                                  (lambda (cps scope)
                                    (yume:procedure-call
                                      (yume:procedure-new
                                        (yume:label
                                          (lambda (cps scope)
                                            (yume:procedure-call
                                              (yume:global-get (quote cons))
                                              cps
                                              (cons 2 (cons 3 (quote ())))))
                                          "g:fun:lambda:e")
                                        0
                                        #f
                                        scope)
                                      cps
                                      '()))
                                  "g")
         )

