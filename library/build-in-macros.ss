(define-syntax cond
  (syntax-rules (else =>)
                ((cond (else result1 result2 ...))
                 (begin result1 result2 ...))
                ((cond (test => result))
                 (let ((temp test))
                   (if temp (result temp))))
                ((cond (test => result) clause1 clause2 ...)
                 (let ((temp test))
                   (if temp
                     (result temp)
                     (cond clause1 clause2 ...))))
                ((cond (test)) test)
                ((cond (test) clause1 clause2 ...)
                 (let ((temp test))
                   (if temp
                     temp
                     (cond clause1 clause2 ...))))
                ((cond (test result1 result2 ...))
                 (if test (begin result1 result2 ...)))
                ((cond (test result1 result2 ...)
                       clause1 clause2 ...)
                 (if test
                   (begin result1 result2 ...)
                   (cond clause1 clause2 ...)))))

(define-syntax case
  (syntax-rules (else)
                ((case (key ...)
                   clauses ...)
                 (let ((atom-key (key ...)))
                   (case atom-key clauses ...)))
                ((case key
                   (else result1 result2 ...))
                 (begin result1 result2 ...))
                ((case key
                   ((atoms ...) result1 result2 ...))
                 (if (memv key '(atoms ...))
                   (begin result1 result2 ...)))
                ((case key
                   ((atoms ...) result1 result2 ...)
                   clause clauses ...)
                 (if (memv key '(atoms ...))
                   (begin result1 result2 ...)
                   (case key clause clauses ...)))))

(define-syntax and
  (syntax-rules ()
                ((and) #t)
                ((and test) test)
                ((and test1 test2 ...)
                 (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
                ((or) #f)
                ((or test) test)
                ((or test1 test2 ...)
                 (let ((x test1))
                   (if x x (or test2 ...))))))

(define-syntax let
  (syntax-rules ()
                ((let ((name val) ...) body1 body2 ...)
                 ((lambda (name ...) body1 body2 ...)
                  val ...))
                ((let tag ((name val) ...) body1 body2 ...)
                 ((letrec ((tag (lambda (name ...)
                                  body1 body2 ...)))
                    tag)
                  val ...))))

(define-syntax let*
  (syntax-rules ()
                ((let* () body1 body2 ...)
                 (let () body1 body2 ...))
                ((let* ((name1 val1) (name2 val2) ...)
                   body1 body2 ...)
                 (let ((name1 val1))
                   (let* ((name2 val2) ...)
                     body1 body2 ...)))))


 (define-syntax letrec ; from http://community.schemewiki.org/?scheme-faq-macros
   (syntax-rules ()
                 ((_ ((var init) ...) . body)
                  (let ((var 'undefined) ...)
                    (let ((var (let ((temp init)) (lambda () (set! var temp))))
                          ...
                          (bod (lambda () . body)))
                      (var) ... (bod))))))

(define-syntax begin
  (syntax-rules ()
                ((begin exp ...)
                 ((lambda () exp ...)))))

(define-syntax do
  (syntax-rules ()
                ((do ((var init step ...) ...)
                   (test expr ...)
                   command ...)
                 (letrec
                   ((loop
                      (lambda (var ...)
                        (if test
                          (begin
                            'undefined
                            expr ...)
                          (begin
                            command
                            ...
                            (loop (do "step" var step ...)
                                  ...))))))
                   (loop init ...)))
                ((do "step" x)
                 x)
                ((do "step" x y)
                 y)))
