(define a 10)

; Numbers
(define (inc x)
  (+ 1 x))


; Lambda
(define (compose f g)
  (lambda (x) (f (g x))))

(define (flip func)
  (lambda (x y) (func y x)))



; List
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

; Control flow
(define-syntax (unless test then else)
  '(if (= ~test false)
     ~then
     ~else))

(define-syntax (when test then)
  '(if ~test
     ~then
     'nil))

(define-syntax (let var val body)
  '((lambda (~var) ~body) ~val))