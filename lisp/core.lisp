(define a 10)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (empty? list)
  (= list '()))

(define (reduce- func acc list)
  (if (empty? list)
    acc
    (reduce func (func acc (first list)) (rest list))))

(define (reduce func list)
  (if (empty? list)
    nil
    (reduce- func (first list) (rest list))))


(define (map func list)
  (reduce (compose cons func) '() list))
