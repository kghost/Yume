#lang scheme

(require srfi/1)

(define expand
  (lambda (p)
    (letrec ((rules (lambda (p) (partition (lambda (e) (eq? (car e) 'define-syntax)) p)))
             
             (prepare-rules
              (lambda (rule)
                (list (cadr rule) (cddr (caddr rule))))) ; TODO: check syntax, support auxiliary keywords
             
             (f (lambda () #f))
             
             (match-rule-set
              (lambda (rule-set p)
                (letrec ((match-rec
                          (lambda (pre rule p)
                            ; pre => ((variable . bind) ...)
                            ; XXX: can recursive rule contains '... ?!?!?! assume not.
                            (cond ((pair? p) (let ((next (map cddr (match rule (car p))))) ; car is variable aleady known, cadr is #f as we assume rule can't contains '...
                                               (match-rec (fold-right (lambda (p n tail) (cons (cons (car p) (cons n (cdr p))) tail)) '() pre next) rule (cdr p))))
                                  ((null-list? p) (map (lambda (e) (cons (car e) (cons #t (reverse (cdr e))))) pre))
                                  (else (raise (list "expand-error" "'... not matching a list"))))))
                         
                         (match ; -> ((variable . binding) ...) or #f if not match, binding => (#t . bind) if is recursive or (#f . bind)
                             (lambda (rule p)
                               (cond ((symbol? rule) (list (cons rule (cons #f p))))
                                     ((pair? rule) (if (pair? p)
                                                       (let ((a (match (car rule) (car p))))
                                                         (if a
                                                             (if (equal? (cdr rule) '(...)) ; XXX: seems '... can only appear at the end of list, or need a fsm processer :(
                                                                 (match-rec (map (lambda (e) (cons (car e) (list (cddr e)))) a) (car rule) (cdr p))
                                                                 (append a (match (cdr rule) (cdr p))))
                                                             (f)))
                                                       (f)))
                                     ((null-list? rule) (if (null-list? p) '() (f)))
                                     (else (raise "internal-expand-error" "1"))))))
                  
                  (if (null-list? rule-set)
                      (raise (list "expand-error" p "don't match"))
                      (let ((result (match (caar rule-set) p)))
                        (if result
                            (cons (cadar rule-set) result)
                            (match-rule-set (cdr rule-set) p)))))))
             
             (expand-term
              (lambda (rule-set p)
                (let* ((result (match-rule-set rule-set p)) (expr (car result)) (binding (cdr result)))
                  (write expr)
                  (write binding))))
             
             (expand-rules-checked
              (lambda (rules p)
                (letrec ((try-expand-term
                          (lambda (p)
                            (if (pair? p)
                                (let ((rule (assq (car p) rules)))
                                  (if rule
                                      (expand-rules-checked rules (expand-term (cadr rule) p))
                                      (map try-expand-term p))) ; FIXME: map cann't process non-list
                                p))))
                  (map try-expand-term p)))) ; FIXME: map cann't process non-list
             
             (expand
              (lambda (rules p)
                (expand-rules-checked (map prepare-rules rules) p))))
      
      
      (call-with-values
       (lambda () (rules p))
       expand))))

(expand '(
          (define-syntax let
            (syntax-rules ()
              ((let ((var expr) ...) body ...)
               ((lambda (var ...) body ...) expr ...))))
          
          (let ((a 1)(b 2)) (+ b a))
          ))