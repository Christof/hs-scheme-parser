(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))

(define (list . objs) objs)
(define (id obj) obj)

(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))

(define (curry func arg1) (lambda (arg) (apply func (cons arg1 (list arg)))))
