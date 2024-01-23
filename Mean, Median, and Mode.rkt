#lang racket

; Calculate sum of a list
(define (sumList lis)
  (if (empty? lis) 0 (+ (car lis) (sumList (cdr lis)))))

(sumList '(1 2 3 4 5))

; CAR=First item in list CDR = Last item in list

(define (mean lis) ;Mean
  (/ (sumList lis) (length lis)))

(define (findMax lis)
  (if
   (= (length lis) 1)
   (car lis)  ;Then
   (if        ;Else
    (> (car lis) (findMax (cdr lis)))
    (car lis)     ;Then
    (findMax (cdr lis)))))

;find the median of a list
(define (median lis)
  ; Sort list 
  (define sorted-list (sort lis <))
  
  ;length of list
  (define len (length sorted-list))
  
  ; is length even or odd
  (if (even? len)
      ; If even, return the average of the middle two elements
      (/ (+ (list-ref sorted-list (/ len 2))
            (list-ref sorted-list (- (/ len 2) 1)))
         2)
      ; If odd, return the middle element
      (list-ref sorted-list (- (/ len 2) 1))))

(define (mode lis)
  (define freq-map (make-hash))
  
  ; Count the frequency of each number in the list
  (for-each
   (lambda (num)
     (hash-update! freq-map num add1 0))
   lis)
  
  ; Find the number(s) with the highest frequency
  (define max-freq (apply max (hash-values freq-map)))
  (define modeList
    (hash-map
     freq-map
     (lambda (num freq)
       (if (= freq max-freq)
           num
           #f))
     '()))
  
  ; Remove #f values from the list (if any)
  (define mode-list (filter (lambda (x) x) mode-list))
  
  mode-list)


