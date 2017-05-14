(define a 10)

(define nums '(1 2 3 4))





(define (inc x)
  (+ 1 x))

(define (id x)
  x)


(define (compose f g)
  (lambda (x) (f (g x))))

(define (flip func)
  (lambda (x y) (func y x)))

(define (second list)
  (first (rest list)))

(define (last list)
  (first (reverse list)))

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

(define (pairs* . items)
  (pairs items))

(define (reduce f init seq) 
  (if (empty? seq) 
      init 
      (reduce f (f init (first seq)) 
                (rest seq)))) 



(define (mapping func)
  (lambda (acc x)
    (cons (func x) acc)))

(define (map func list)
  (reverse (reduce (mapping func) '() list)))

(define (list* . items)
  (reduce (flip cons)
          (first (reverse items)) 
          (rest (reverse items))))

(define-syntax (unless test then)
  '(if (= ~test false)
     ~then
     nil))

(define-syntax (when test then)
  '(if ~test
     ~then
     nil))


(define (binding-vars bindings)
  (map first (pairs bindings)))

(define (binding-vals bindings)
  (map second (pairs bindings)))


(define-syntax (let bindings . body)
  (list* (list* 'lambda  (binding-vars bindings) body) 
         (binding-vals bindings)))

(define (do . forms)
  (last forms))

(define (wrap-if acc clause)
  '(if ~(first clause)
     ~(second clause)
     ~acc))

(define-syntax (cond . clauses)
  (reduce wrap-if 'nil (reverse (pairs clauses))))

(define-syntax (trace form)
  '(let (result ~form)
      (print "'~form => ~'result")
      result))

(define (require path)
  (map eval (read-many (slurp path))))



(define (tr)
  ^(+ 1 
    (call/cc 
      (lambda (return)
        1
        ^(return 15)
        3))))

(define (dbg-test x)
  (debug))

(define-syntax (define-readermacro start end sym)
  '(set! readtable (cons '((~~start ~~end) ~sym) readtable)))

(define-readermacro "{" "}" pairs*)
(define-readermacro "[" "]" list)