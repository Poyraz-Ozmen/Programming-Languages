(define (andmap1 pred . lox) 
  (let loop ((lst lox)) 
    (cond ((or (null? lst) (null? (car lst))) 
           true) 
          ((null? (cdar lst))
           (apply pred (map car lst))) 
          ((not (apply pred (map car lst))) 
           false) 
          (else (loop (map cdr lst))))))

(define symbol-length
(lambda (inSym)
(if (symbol? inSym)
(string-length (symbol->string inSym))
0
)
)
)



(define (sequence? seq)      
  (andmap1 (lambda (e)        
            (and (symbol? e) 
                 (= 1 (string-length (symbol->string e))))) 
          seq))




(define (same-sequence? seq1 seq2)
  (if  (equal? #t  (sequence? seq1))
      (if(equal? #t (sequence? seq2))
        (if (equal? seq1 seq2)
        #t
        #f )
        (error "PRODUCE ERROR!!!!!!")
      )
      (error "PRODUCE ERROR!!!!!!!")  
     ) 
)

(define (rev seq1)
  (define (iter accum seq1)
    (if (null? seq1)
        accum
        (iter (cons (car seq1) accum)
              (cdr seq1))))
  (iter '() seq1))

(define (reverse-sequence seq1 )
  (if  (equal? #t  (sequence? seq1))
      (rev seq1)
      (error "PRODUCE ERROR!!!!!!!")  
     ) 
)

(define palindromic?
  (lambda (li)
    (equal? li (reverse li))))


(define (palindrome? seq1 )
  (if  (equal? #t  (sequence? seq1))
       (palindromic? seq1)
      (error "PRODUCE ERROR!!!!!!!")  
     ) 
)

(define (member? inSym seq1)
  (if (equal? #t  (sequence? seq1))
    (if (symbol? inSym)
        (not (equal? (member inSym seq1) #f))
        (error "PRODUCE ERROR!!!!!!!")  
      )
    (error "PRODUCE ERROR!!!!!!!")  
    )
 )

(define (delete-1st x lst)
  (cond ((null? lst) '())
        ((equal? (car lst) x) (cdr lst))
        (else (cons (car lst)
                    (delete-1st x (cdr lst))))))


 (define (remove-member inSym seq1)
  (if (equal? #t  (sequence? seq1))
    (if (symbol? inSym)
        (if(equal?  (delete-1st inSym seq1) seq1)
          (error "PRODUCE ERROR!!!!!!!")  
          (delete-1st inSym seq1)
        )
        (error "PRODUCE ERROR!!!!!!!")  
      )
    (error "PRODUCE ERROR!!!!!!!")  
    )
 )



(define (anagram? seq1 seq2)
  (if  (equal? #t  (sequence? seq1))
      (if(equal? #t (sequence? seq2))
        (if (equal? (sort seq1 symbol<?) (sort seq2 symbol<?))
        #t
        #f )
        (error "PRODUCE ERROR!!!!!!")
      )
      (error "PRODUCE ERROR!!!!!!!")  
     ) 
)



(define (anapoli? seq1 seq2)
  (if  (equal? #t  (sequence? seq1))
      (if(equal? #t (sequence? seq2))
        (if (equal? #t (palindrome? seq2))
          (if  (equal? #t (anagram? seq1 seq2))
              #t
              #f
          )
          #f
        )
        (error "PRODUCE ERROR!!!!!!")
      )
      (error "PRODUCE ERROR!!!!!!!")  
     ) 
)
