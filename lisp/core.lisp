(define a 10)

(define (empty? list)
  (= list '()))

(define (reduce func acc list)
  (if (empty? list)
    acc
    (reduce func (func acc (first list)) (rest list))))
