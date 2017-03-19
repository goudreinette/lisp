(define a 10)


(define (inc x)
  (+ 1 x))


(define (compose f g)
  (lambda (x) (f (g x))))

(define (flip func)
  (lambda (x y) (func y x)))

(define (second list)
  (first (rest list)))

(define (list . args)
  args)

(define (pair a b)
 '(~a ~b))

(define (first-two list)
  (pair (first list) (second list)))

(define (empty? list)
  (= list '()))

(define (pairs list)
  (define (iter acc remainder)
    (if (empty? remainder)
      (reverse acc)
      (iter (cons (first-two remainder) acc)
            (rest (rest remainder)))))
  (iter () list))

(define (reduce func acc list)
  (if (empty? list)
    acc
    (reduce func (func acc (first list)) (rest list))))

(define (mapping func)
  (lambda (acc x)
    (cons (func x) acc)))

(define (map func list)
  (reverse (reduce (mapping func) '() list)))


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



(define-syntax (do . forms)
  (list (cons 'lambda (cons () forms))))