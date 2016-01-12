; Assignment: Lab 2
; Class: CSC 135
; Name: Sayd Mateen

;-----------------------------------------------Part A--------------------------------------------------------
(define (checkNeg x);Checks if the x value is negative. If true it then changes it to positive and returns it.
  (if(< x 0) (- 0 x) x ))

(define (buildLR left right) ;Takes the two integers and returns the combination of the left two digits and right two digits. 
  (if( < right 10 ) ;This if condition with the one below it checks if there is any of the two integers has less than two digits. It true it then returns -1.
     (if( > right -10) -1))
     (if( < left 10)
        (if( > left -10) -1))
  ;Using two function it grabs the left two and the right two digits and combines them together using additon.
  ;Makes sure to multiply the left number by 100 to give allow the addition of the right two digits. 
  ( +  (leftTwo (checkNeg left)) (modulo (checkNeg right) 100)))


(define (leftTwo x); takes in an integer and returns the left two using arithmitic 
  (if ( < x 100) (* x 100)
      (leftTwo (floor( / x 10)))))

;-----------------------------------------------Part B---------------------------------------------------------

(define (whichSmall x y) ;Gets two integers and returns the smaller one 
  (if(< x y) x
     y))

(define (listMins Left Right) ;Takes two lists and compares the individual digits one by one. It then chooses the smaller of the two and returns a list of the digits that were smaller. 
  (if(null? Left) '()
     (cons (whichSmall (car Left) (car Right)) (listMins (cdr Left) (cdr Right)))))

;-----------------------------------------------Part C---------------------------------------------------------

(define (unwind List)
  (if(null? List) '()
     ;Recursivly calls unwind which checks if the list is even it then proceeds to move the center element to the begining accordingly.
     ;It first grabs the center element and adds it to the front then removes and repeates using recursion until the list is empty. 
     (if(isEven (totalCount List 0)) (cons (listCenter List (- (listCount List 0) 1)) (unwind (removeCenter List (listCenter List (- (listCount List 0) 1))))) ;If even condition
        (cons (listCenter List(listCount List 0)) (unwind (removeCenter List (listCenter List (listCount List 0)))))))) ;If odd condition
  
     
(define (isEven x) ;Returns true if the integer is even. 
  (equal? (modulo x 2) 0))

(define (totalCount L x) ;Returns the total number of elements in the list. 
  (if(null? L) x
     (totalCount (cdr L) (+ x 1))))

(define (listCount L x) ;Returns the elements in the list divided by two, if the list is even it picks the left one. 
  (if(null? L) (floor (/ x 2))
     (listCount (cdr L) (+ x 1))))

(define (listCenter L x) ;Returns the center element indicated by the the x value. 
  (if(< x 1) (car L)
     (listCenter (cdr L) ( - x 1))))
     
(define (removeCenter L a) ;Removes the element indicated by the a parameter a from the list L. 
  (if(null? L) '()
     (if(equal? a (car L)) (cdr L)
        (cons (car L) (removeCenter (cdr L) a))))) 
      
   

;------------------------------------------------Part D-------------------------------------------------------

(define (functionWinner F G List) ;Takes two boolean function and returns the fucntion who gets the most true when comparted to the list. 
  (if( null? List ) 0
     (if( equal? (count F List) (count G List) ) 0 ;Checks if the count came back equal. 
       (if( < (count F List) (count G List) ) 2 1 )))) ;Checks which count has the most number of true. Returns 2 for F and 1 for G. 
          

(define (count X List) ;Count takes the function and applies it to the list while keeping track of the number of trues found.
  (if( null? List) 0
    (+ (if(equal? (X (car List)) #t) 1 0) (count X (cdr List))))) ;Returns the number of trues. 

(define (isNeg x) ;Creates this function for tesing. It returns true if the integer passed in is negative. 
  (< x 0))



;-----------------------------------------------Part E---------------------------------------------------------



(define (getNestedCount L) ;Gets a list of possibly nested integers and returns how many integers are contianed in the list.  
  (if (null? L) 0
      ;If the list check condition is true it takes the nested list as an element and then recursivly calls itself until it finds not a list then calls itself with a differnt if condition.
      (if(list? (car L)) (+(getNestedCount (car L)) (getNestedCount (cdr L)))  
        (+ (getNestedCount (cdr L)) 1)))) ;If its not a list then we just need to cdr and discard the integer and add one to count.


;-----------------------------------------------Part F---------------------------------------------------------


(define (makeCutter n) ;Builds a function based on the specifications indicated during the definiton.
  (define (cutter List) ;Defines the function within the function. After defining it, it then returns the defined function. 
    (getcutter List n))
  cutter
  )
    
(define (getcutter List n) ;This function gets called and returns the list with n integers from the begining of the list taken out. 
  (if(= n 0) '()
       (cons (car List) (getcutter (cdr List) (- n 1)))))

;------------------------------------------------END-------------------------------------------------------------
