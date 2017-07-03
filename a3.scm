;Written by Jonathan M. Loewen STN 301281915 for CMPT-383, Summer 2017, Toby Donaldson, Assignment 3.
;Question 1
(define (my-last-rec lst)
    (if (null? (cdr lst))
        (car lst)
        (my-last-rec(cdr lst))
    )
)

(define (my-last lst)
    (if (null? lst)
        (error "my-last: empty list")  ;This is exactly as in the Assignment, but doesn't work on mit/gnu scheme
        (my-last-rec lst)
    )
)

;Question 2 - Complete
(define (snoc x lst)
    (if (null? lst)
        (list x)
        (cons (car lst) (snoc x (cdr lst)))
    )
)

;Question 3
(define (descendingRange n)
    (if (< n 1)
        '()
        (cons (- n 1) (descendingRange (- n 1))
        )
    )
)

(define (reverseList lst)
    (if (null? lst)
    '()
    (append (reverseList (cdr lst)) (list (car lst)))
    )
)


;Ref: https://stackoverflow.com/questions/26002693/scheme-cant-bind-name-in-null-syntactic-environment-tmp
(define (range n)
    (if (< n 1)
        '()
        (let ()
            (define descList (descendingRange n))
            (reverseList descList)
        )
    )
)

;Question 4, Deep Sum

(define (deep-sum lst)
 (if (null? lst)
  0
  (if (number? (car lst))
  (+ (car lst) (deep-sum (cdr lst)))
   (if (list? (car lst))
    (+ (deep-sum (car lst)) (deep-sum (cdr lst)))
    (+ 0 (deep-sum (cdr lst)))
   )
  )
 )
)

;Question 5 - countPrimes
(define (isPrimeRec n c)
  (if (> c (sqrt n))
    #t
    (if (= (modulo n c) 0)
      #f
      (if (= (modulo (+ c 1) 6) 0)
        (isPrimeRec n (+ c 2))
        (isPrimeRec n (+ c 4))
      )
    )
)
)

;filter out anything thats not 6n +/- 1
(define (isPrime n)
  (if (< n 2)
    #f
    (if (< n 4)
      #t
      (if (or (= (modulo n 6) 5) (= (modulo n 6) 1))
        (isPrimeRec n 5)
        #f
      )
    )
  )
)

(define (countTruths lst)
 (if (= 0 (length lst))
  (+ 0 0)
  (if (equal? (car lst) #t)
    (+ 1 (countTruths (cdr lst)))
    (+ 0 (countTruths (cdr lst)))
  )
 )
)

(define (count-primes n)
 (+ 0 (countTruths (map isPrime (range (+ n 1)))))
)


;Question 6
 
(define (is-bit? b)
  (if (number? b)
    (cond ((= 0 b) #t)
       ((= 1 b) #t)
       (else #f))
    #f
  )
)
 
 
;Question 7
(define (is-bit-seq? lst)
  (if (= 0 (length lst))
      #t
      (if (is-bit? (car lst))
        (is-bit-seq? (cdr lst))
        #f
      )
  )
)
 
;Question 8
 
;Need: A function that returns a single bit sequence for a given value and a desired length.
;return a bit sequence for this value v and length l.
(define (bit-seq-val v l)
  (if (= 0 l)
    '()    
    (if (>= v (expt 2 (- l 1)))
      (cons '1 (bit-seq-val (- v (expt 2 (- l 1))) (- l 1)))
      (cons '0 (bit-seq-val v (- l 1)))
    )
  )
)
 
;Takes in the number to get the bit sequence of, calls bit-seq-val this many times.
(define (cons-bit-seqs v l)
  (if (>= v 0)
      (cons (bit-seq-val v l) (cons-bit-seqs (- v 1) l))
      '()
  )
)
 
(define (all-bit-seqs l)
  (if (= l 0)
  '()
  (cons-bit-seqs (- (expt 2 l) 1)  l)
  )
)