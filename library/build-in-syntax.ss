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
    () ; binding varialbe begin with | means need to be renamed
    ((lambda (|vars ...) . body)
     ($lambda (|vars ...) . body))))

(define-syntax if
  (syntax-rules
    ()
    ((if test true)
     (if test true (begin)))
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

