(define-syntax curry
  (syntax-rules ()
    ((_ (arg0) body ...) (lambda (arg0) body ...))
    ((_ (arg0 arg1 ...) body ...)
     (lambda (arg0 . rest)
       (define applied (curry (arg1 ...) body ...))
       (if (null? rest) applied
           (apply applied rest))))))

(define greet (curry (a b)
                     (+ a b)))
             

(display (greet 1 2))
(display ((greet 1) 2))

(define foo 5)

(call-with-current-continuation 
(lambda (exit)
(dynamic-wind
       (lambda () (display "b "))
       (lambda () (+ -2 (call-with-current-continuation (lambda (exit2) (set! foo exit2) (display "t") 99))))
       (lambda () (exit 9) (display " e"))
))
)

