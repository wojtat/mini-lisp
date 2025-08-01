(define factorial (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))

(define fact factorial)

(define f10 (fact 10))

f10
