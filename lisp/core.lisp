(define a 10)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (flip func)
  (lambda (x y) (func y x)))


(define (inc x)
  (+ 1 x))

(define +2 (compose inc inc))

(define (empty? list)
  (= list '()))

(define (reduce func acc list)
  (if (empty? list)
    acc
    (reduce func (func acc (first list)) (rest list))))


(define (mapping func)
  (lambda (acc x)
    (cons (func x) acc)))

(define (map func list)
  (reverse (reduce (mapping func) '() list)))
