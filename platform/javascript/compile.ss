(use-modules (srfi srfi-1))

(load "../../interpreter/reader.ss")
(load "../../expander/expand.ss")
(load "../../transformer/transform.ss")
(load "../../compiler/javascript.ss")

(define build-in-macros (interpret (open-input-file "library/build-in-macros.ss")))
(define program (interpret (current-input-port)))

(compile (current-output-port) (transform (expand (cons 'begin (append build-in-macros program)))))
(newline)

