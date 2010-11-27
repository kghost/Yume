#lang scheme

(require srfi/1)

(define expand
  (lambda (p)
    (letrec ((rules (lambda (p) (partition (lambda (e) (eq? (car e) 'define-syntax)) p)))
             
             (prepare-rules
              (lambda (rule)
                (list (cadr rule) (cddr (caddr rule))))) ; TODO: check syntax, support auxiliary keywords
             
             (match-rule-set
              (lambda (rule-set p)
                (letrec ((match-rec-empty-binding
                          (lambda (pre rule)
                            (cond ((pair? rule) (match-rec-empty-binding (match-rec-empty-binding pre (cdr rule)) (car rule)))
                                  ((symbol? rule) (cons (list rule) pre))
                                  (null-list? pre))))
                         
                         (match-rec
                          (lambda (pre rule p)
                            ; pre => ((variable . bind) ...)
                            ; XXX: can recursive rule contains '... ?!?!?! assume not.
                            (cond ((pair? p) (let ((next (match rule (car p)))) ; car is variable aleady known, cadr is #f as we assume rule can't contains '...
                                               (and next (match-rec (fold-right (lambda (p n tail) (cons (cons (car p) (cons n (cdr p))) tail)) '() pre (map cadr next)) rule (cdr p)))))
                                  ((null-list? p) (map (lambda (e) (list (car e) (cons #t (cdr e)))) pre))
                                  (else (raise (list "expand-error" "'... not matching a list"))))))
                         
                         (match ; -> ((variable . binding) ...) or #f if not match, binding => (#t . bind) if is recursive or (#f . bind)
                             (lambda (rule p)
                               (cond ((symbol? rule) (list (list rule (cons #f p))))
                                     ((pair? rule) (if (equal? (cdr rule) '(...)) ; XXX: seems '... can only appear at the end of list, or need a fsm processer :(
                                                       (match-rec (match-rec-empty-binding '() (car rule)) (car rule) p)
                                                       (if (pair? p)
                                                           (let ((head (match (car rule) (car p))) (tail (match (cdr rule) (cdr p))))
                                                             (if (and head tail)
                                                                 (append head tail)
                                                                 #f))
                                                           #f)))
                                     ((null-list? rule) (if (null-list? p) '() #f))
                                     (else (raise "internal-expand-error" "1"))))))
                  
                  (if (null-list? rule-set)
                      (raise (list "expand-error" p "don't match"))
                      (let ((result (match (caar rule-set) p)))
                        (if result
                            (cons (cadar rule-set) result)
                            (match-rule-set (cdr rule-set) p)))))))
             
             (pair-fold-right-all
              (lambda (kons-tail kons clist)
                (cond ((pair? clist) (kons clist (pair-fold-right-all kons-tail kons (cdr clist))))
                      (else (kons-tail clist)))))
             
             (fold-right-all
              (lambda (kons-tail kons clist)
                (cond ((pair? clist) (kons (car clist) (fold-right-all kons-tail kons (cdr clist))))
                      (else (kons-tail clist)))))
             
             (map-all
              (lambda (proc clist)
                (fold-right-all (lambda (a) (proc a))
                                (lambda (a d) (cons (proc a) d))
                                clist)))
             
             (binding-need
              (lambda (pre rule)
                (cond ((pair? rule) (binding-need (binding-need pre (cdr rule)) (car rule)))
                      ((symbol? rule) (cons rule pre))
                      (null-list? pre))))
             
             (binding-pop-frame ; -> (poped-binding . remaining-binding) or (#f . binding) if at lease one binding is at end
              (lambda (binding need)
                (fold (lambda (e tail) ; it isn't ordered, so use fold instead of fold-right
                        (if (and (car tail) (find (lambda (v) (eq? v (car e))) need))
                            (let ((bind (cadr e)))
                              (if (car bind)
                                  (if (null-list? (cdr bind)) ; it is a recursive bind
                                      (cons #f binding) ; but at end return #f
                                      (cons (cons (list (car e) (cadr bind)) (car tail))
                                            (cons (list (car e) (cons #t (cddr bind))) (cdr tail))))
                                  (cons (cons e (car tail))
                                        (cons e (cdr tail)))))
                            tail))
                      (cons '() '()) binding)))
             
             (expand-term
              (lambda (rule-set p)
                (let* ((result (match-rule-set rule-set p))
                       (expr (car result)) (binding (cdr result)))
                  (letrec
                      ((apply-rule-rec
                        (lambda (result rule binding)
                          (let ((need (binding-need '() rule)))
                            (apply unfold-right (append (list (lambda (seed) (not (car seed)))
                                                              (lambda (seed) (apply-rule rule (car seed)))
                                                              (lambda (seed) (binding-pop-frame (cdr seed) need))
                                                              (binding-pop-frame binding need))
                                                        result)))))
                       
                       (apply-rule
                        (lambda (rule binding) ; -> result
                          (cond ((pair? rule) (pair-fold-right-all
                                               (lambda (lis) (apply-rule lis binding))
                                               (lambda (lis next)
                                                 (cond ((eq? (car lis) '...) next)
                                                       ((and (pair? (cdr lis)) (eq? (cadr lis) '...))
                                                        (apply-rule-rec next (car lis) binding))
                                                       (else (cons (apply-rule (car lis) binding) next))))
                                               rule))
                                ((symbol? rule) (let ((found (assq rule binding)))
                                                  (if found
                                                      (if (caadr found)
                                                          (raise (list "extend-error" rule "'... depth not match"))
                                                          (cdadr found))
                                                      rule)))
                                (else rule))))
                       )
                    (apply-rule expr binding))
                  )))
             
             (expand-rules-checked
              (lambda (rules p)
                (letrec ((try-expand-term
                          (lambda (p)
                            (if (pair? p)
                                (let ((rule (assq (car p) rules)))
                                  (if rule
                                      (expand-rules-checked rules (expand-term (cadr rule) p))
                                      (map-all try-expand-term p)))
                                p))))
                  (map-all try-expand-term p))))
             
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


(expand '(
          (define-syntax foo 
            (syntax-rules () 
              ((foo (a ...) (b ...) ...) '(((a b) ...) ...)))) 
          (foo (1 2) (3 4) (5 6 7))
          ))