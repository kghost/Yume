; This implementation is not IEEE- or R5RS-compliant,
; for the following reasons:
;
; This implementation does not redefine procedures
; like READ, WRITE, DISPLAY, and NEWLINE to ensure
; that they use the redefined PEEK-CHAR, READ-CHAR,
; WRITE-CHAR, and so forth.  That should be easy
; for an implementor to do, however.
;
; This implementation obtains an end-of-file object
; by reading a Unix-specific file, /dev/null.

(define open-input-string 0)  ; assigned below
(define open-output-string 0) ; assigned below
(define get-output-string 0)  ; assigned below

; We have to remember the original procedures before
; we can define new ones.

(define ur-vector? vector?)
(define ur-vector-length vector-length)
(define ur-vector-ref vector-ref)
(define ur-vector-set! vector-set!)
(define ur-input-port? input-port?)
(define ur-output-port? output-port?)
(define ur-close-input-port close-input-port)
(define ur-close-output-port close-output-port)
(define ur-peek-char peek-char)
(define ur-read-char read-char)
(define ur-write-char write-char)

; IEEE/ANSI Scheme insists that we define any global
; variables that we are going to assign.  R5RS Scheme
; apparently does not require this.

(define vector? vector?)
(define vector-length vector-length)
(define vector-ref vector-ref)
(define vector-set! vector-set!)
(define input-port? input-port?)
(define output-port? output-port?)
(define close-input-port close-input-port)
(define close-output-port close-output-port)
(define peek-char peek-char)
(define read-char read-char)
(define write-char write-char)

(let ((ur-vector? ur-vector?)
      (ur-vector-length ur-vector-length)
      (ur-vector-ref ur-vector-ref)
      (ur-vector-set! ur-vector-set!)
      (ur-input-port? ur-input-port?)
      (ur-output-port? ur-output-port?)
      (ur-close-input-port ur-close-input-port)
      (ur-close-output-port ur-close-output-port)
      (ur-peek-char ur-peek-char)
      (ur-read-char ur-read-char)
      (ur-write-char ur-write-char)
      (eof (call-with-input-file "/dev/null" read-char))
      (input-string-tag (list 'input-string-tag))
      (output-string-tag (list 'output-string-tag)))
  
  (define (error)
    (display "You're not supposed to do that!")
    (newline)
    (if #f #f))
  
  (define (restrict f pred?)
    (lambda (x . rest)
      (if (pred? x)
          (apply f x rest)
          (error))))
  
  (define (my-vector? x)
    (and (ur-vector? x)
         (not (input-string? x))
         (not (output-string? x))))
  
  (define (input-string? x)
    (and (ur-vector? x)
         (positive? (ur-vector-length x))
         (eq? input-string-tag (ur-vector-ref x 0))))
  
  (define (output-string? x)
    (and (ur-vector? x)
         (positive? (ur-vector-length x))
         (eq? output-string-tag (ur-vector-ref x 0))))
  
  (define (selector pred? i)
    (lambda (x)
      (if (pred? x)
          (ur-vector-ref x i)
          (error))))
  
  (define (setter pred? i)
    (lambda (x y)
      (if (pred? x)
          (begin (ur-vector-set! x i y)
                 (if #f #f))
          (error))))
  
  (set! vector?       my-vector?)
  (set! vector-length (restrict ur-vector-length my-vector?))
  (set! vector-ref    (restrict ur-vector-ref  my-vector?))
  (set! vector-set!   (restrict ur-vector-set! my-vector?))
  
  (let ()
    
    ; The guts of the implementation begin here.
    
    (define (make-input-string s)
      (vector input-string-tag #t s (string-length s) 0))
    
    (define input-string:open?  (selector input-string? 1))
    (define input-string:open?! (setter   input-string? 1))
    (define input-string:string (selector input-string? 2))
    (define input-string:size   (selector input-string? 3))
    (define input-string:next   (selector input-string? 4))
    (define input-string:next!  (setter   input-string? 4))
    
    (define (make-output-string)
      (vector output-string-tag #t '()))
    
    (define output-string:open?     (selector output-string? 1))
    (define output-string:open?!    (setter   output-string? 1))
    (define output-string:contents  (selector output-string? 2))
    (define output-string:contents! (setter   output-string? 2))
    
    (set! open-input-string make-input-string)
    (set! open-output-string make-output-string)
    (set! get-output-string
          (lambda (x)
            (list->string (reverse (output-string:contents x)))))
    
    (set! input-port?
          (lambda (x)
            (or (ur-input-port? x)
                (input-string? x))))
    
    (set! output-port?
          (lambda (x)
            (or (ur-output-port? x)
                (output-string? x))))
    
    (set! close-input-port
          (lambda (x)
            (if (input-string? x)
                (input-string:open?! x #f)
                (ur-close-input-port x))))
    
    (set! close-output-port
          (lambda (x)
            (if (output-string? x)
                (output-string:open?! x #f)
                (ur-close-output-port x))))
    
    (set! peek-char
          (lambda args
            (if (null? args)
                (ur-peek-char)
                (let ((x (car args)))
                  (if (input-string? x)
                      (let ((s (input-string:string x))
                            (i (input-string:next x))
                            (n (input-string:size x)))
                        (if (input-string:open? x)
                            (if (< i n)
                                (string-ref s i)
                                eof)
                            (error)))
                      (ur-peek-char x))))))
    
    (set! read-char
          (lambda args
            (if (null? args)
                (ur-read-char)
                (let ((x (car args)))
                  (if (input-string? x)
                      (let ((s (input-string:string x))
                            (i (input-string:next x))
                            (n (input-string:size x)))
                        (if (input-string:open? x)
                            (if (< i n)
                                (let ((c (string-ref s i)))
                                  (input-string:next! x (+ i 1))
                                  c)
                                eof)
                            (error)))
                      (ur-read-char x))))))
    
    (set! write-char
          (lambda (c . rest)
            (if (null? rest)
                (ur-write-char c)
                (let ((x (car rest)))
                  (if (output-string? x)
                      (if (output-string:open? x)
                          (output-string:contents!
                           x
                           (cons c (output-string:contents x)))
                          (error))
                      (ur-write-char c x))))))
    
    (if #f #f)))
