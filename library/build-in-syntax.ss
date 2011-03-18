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
    (=>) ; binding varialbe begin with | means need to be renamed
    ((lambda (|vars ...) . body)
     ($lambda (|vars ...) (begin . body)))
    ((lambda (|var1 . var*) => (vars ...) . body)
     (lambda var* => (vars ... |var1) . body))
    ((lambda |var => () . body)
     ($lambda |var (begin . body)))
    ((lambda |var => (vars ...) . body)
     ($lambda (vars ... . |var) (begin . body)))
    ((lambda vars . body)
     (lambda vars => () . body))))

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

(define-syntax quasiquote
  (syntax-rules
    (unquote =>)
    ((quasiquote => (x . y)) (cons (quasiquote x) (quasiquote => y)))
    ((quasiquote => y) (quote y))
    ((quasiquote (unquote x)) x)
    ((quasiquote (x . y)) (cons (quasiquote x) (quasiquote => y)))
    ((quasiquote x) (quote x))))

; XXX : shadow pretty-print
(define-syntax pretty-print
  (syntax-rules
    ()
    (x (begin))))
