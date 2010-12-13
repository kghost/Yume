; convert primitive forms to hygienic forms

(define-syntax define
  (syntax-rules
    ()
    ((define (variable . formals) . body)
     ($define variable (lambda formals . body)))
    ((define variable value)
     ($define variable value))))

(define-syntax lambda
  (syntax-rules
    ()
    ((lambda variable . body)
     ($lambda variable (begin . body)))))

(define-syntax if
  (syntax-rules
    ()
    ((if test true)
     (and test true))
    ((if test true false)
     ($if test true false))))

(define-syntax set!
  (syntax-rules
    ()
    ((set! variable value)
     ($set! variable value))))

(define-syntax begin
  (syntax-rules
    ()
    ((begin body)
     body)
    ((begin . body)
     ($begin . body))))

