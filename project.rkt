#lang racket
;(foldl + '(1 2 3) 0)  ->  (+ 3(+ 2(+ 1 0)))
(define (my_foldl f init lst)
  (if(null? lst)
     init 
     (my_foldl f (f (car lst) init) (cdr lst))))

;(getLast '(1 2 5))  ->  5
(define (getLast lst)
  (if(null? (cdr lst))
     (car lst)
     (getLast (cdr lst))))

;(withoutLast '(1 2 5))  ->  '(1 2)
(define (withoutLast lst)
  (if(null? (cdr lst))
     '()
     (cons (car lst) (withoutLast (cdr lst)))))

;(foldr + '(1 2 3) 0)  ->  (+ 1(+ 2(+ 3 0)))
(define (foldr1 f init lst)
  (if(null? lst)
     init
     (foldr1 f (f (getLast lst) init) (withoutLast lst))))
;OR
(define (foldr2 f init lst)
  (if(null? lst)
     init
     (f (car lst) (foldr2 f init (cdr lst)))))

;map
;(my_map_fold (lambda (x) (+ x x)) '(1 2 3))  ->  '(2 4 6)
(define (my_map_fold f lst)
  (foldr2 (lambda (el acc)
            (cons (f el) acc))
                               '() lst))
;filter
;(my_filter_fold (lambda (x) (< x 10)) '(1 22 4 2 12 32))  ->  '(1 4 2)
(define (my_filter_fold exp lst)
  (foldr2 (lambda (el acc)
          (if (exp el)
              (cons el acc)
              acc))
                           '() lst))

;reduce
;(my_reduce + '(1 2 3 4))  ->  10
;works without giving the init value
(define (my_reduce op lst)
  (foldr2 (lambda (x r) (op x r)) (car lst) (cdr lst)))

;-----------------------------------
;Write a function, treeFold, which takes a function, f, an initial value, initial, and a tree structure, tree, as parameters.
;It should then produce a single value which is the result of using f to fold the values in the tree (using left, middle, and right subtrees
;in that order). Note that f is a function which takes two parameters. The first parameter is a value from the tree and the second is the partially
;accumulated result.



(define (foldtree f init tree)
  (cond
    ((null? tree) init)
     (#t (apply f (car tree) (my_map_fold  (lambda (x)
                                     (foldtree f init x))
                                                          (cdr tree)
                              )
         )
     )
  )
)
;(apply + '(1 2 3))  ->  6
;(apply + 1 2 '(3))  ->  6
;(apply + '())       ->  0
;(apply sort (list (list '(2) '(1)) <) #:key car)  ->  '((1) (2))

;Insertion Sort
(define (apend l1 l2)
  (if(null? l1)
     l2
     (cons (car l1)(apend (cdr l1) l2))))   

(define (insert el sorted_lst res)
  (cond
    [(null? sorted_lst)  (apend res (list el))]
    [(not (> el (car sorted_lst))) (append res (list el) sorted_lst)]
    [(insert el (cdr sorted_lst) (apend res (list (car sorted_lst))))]))

(define (insert_sort lst)
  (foldr2 (lambda (el acc)
            (insert el acc '()))
                                 '() lst))
;Selection Sort
(define (delete_first_occ e lst)
  (if (null? lst)
      lst   
      (if(eq? e (car lst))
         (cdr lst)
         (cons (car lst) (delete_first_occ e (cdr lst))))))
 

(define (select_min lst)
  (foldr2 (lambda (el acc)
            (min el acc))
                         (car lst) lst))

(define (select_sort lst)
  (cond
    ((null? lst) lst)
    ((null? (cdr lst))lst)
    (#t(let ((m (select_min lst)))  ;m = select_min lst
        (cons m (select_sort (delete_first_occ m lst)))))))

