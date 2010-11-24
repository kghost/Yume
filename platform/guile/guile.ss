#lang scheme

(define yume:label (lambda args (car args)))

(define yume:halt (lambda (reason) (raise reason)))

(define yume:global-get
  (lambda (variable)
    variable))

(define yume:continue-new
  (lambda (f cps scope)
    (cons f (cons cps scope))))

(define yume:continue-call
  (lambda (continue result)
    ((car continue) (cdr continue) result)))

(define yume:continue-cps
  (lambda (continue-ctx)
    (car continue-ctx)))

(define yume:continue-scope
  (lambda (continue-ctx)
    (cdr continue-ctx)))

(define yume:procedure-new
  (lambda (f args-length args-is-variable scope)
    (cons (cons 'procedure (cons args-length args-is-variable)) (cons f scope))))

(define yume:procedure-call
  (lambda (procedure cps args)
    (let ((procedure-tag (car procedure)) (p (cdr procedure)))
    (if (eq? 'procedure (car procedure-tag))
        (let ((procedure-info (cdr procedure-tag)))
          (let ((args-length (car procedure-info)) (args-is-variable (cdr procedure-info)))
            (if args-is-variable
                (yume:halt "variable arguments not supported")
                (if (eq? args-length (length args))
                 ((car p) cps (cons args (cdr p)))
                 (yume:halt (list "wrong-number-of-args" procedure args))))))
           (yume:halt (list "not-a-procedure" procedure))))))
