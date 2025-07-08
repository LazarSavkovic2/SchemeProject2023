;Made by Lazar Savkovic 300246649
#lang scheme
(define (readXYZ fileIn)
 (let ((sL (map (lambda s (string-split (car s)))
 (cdr (file->lines fileIn)))))
 (map (lambda (L)
 (map (lambda (s)
 (if (eqv? (string->number s) #f)
 s
 (string->number s))) L)) sL)))


; defines the number of iterations that will need to be performed
(define (ransacNumberOfIterations confidence percentage)
  (exact-ceiling (/
   (log 10 (- 1 (expt percentage 3)))
   (log 10 (- 1 confidence))
)))

;(list-ref (readXYZ "Point_Cloud_1_No_Road_Reduced.xyz") (random (length (readXYZ "Point_Cloud_1_No_Road_Reduced.xyz"))))

;this function computes the a b c d values of a plane and returns them in a list
;
(define (plane P1 P2 P3)
  (define a (-(* (- (list-ref P2 1) (list-ref P1 1)) (- (list-ref P3 2) (list-ref P1 2))) (* (- (list-ref P3 1) (list-ref P1 1)) (- (list-ref P2 2) (list-ref P1 2))))) ;creates a variable a 
  (define b(-(* (- (list-ref P2 2) (list-ref P1 2)) (- (list-ref P3 0) (list-ref P1 0))) (* (- (list-ref P3 2) (list-ref P1 2)) (- (list-ref P2 0) (list-ref P1 0))))) ; creates a variable b
  (define c(-(* (- (list-ref P2 0) (list-ref P1 0)) (- (list-ref P3 1) (list-ref P1 1))) (* (- (list-ref P3 0) (list-ref P1 0)) (- (list-ref P2 1) (list-ref P1 1))))) ; creates a variable c
  (define d(* -1 (+ (* a (list-ref P1 0)) (* b (list-ref P1 1)) (* c (list-ref P1 2))))) ; uses defined variables a, b and c to create a variable d
  (cons a (cons b (cons c (cons d '())))); constructs a list returning a b c d
  )
; ‘(a b c d) returns
; 

; checks to see how many points a given plane supports, returns the plane (abcd values) and the number of points it supports in a pair
(define (support plane points eps)
  (define n (supportLoop plane points eps 0 0))
  (cons plane (cons n '()))
  )

; a looping function that checks to see if eps is greater than the distance between a point and
; a plane, if it is smaller the number of supported points increases otherwise it remains the same
; it then returns the number of supported points
(define (supportLoop plane points eps n loopDuration)
  (if(not (equal? loopDuration (length points)))
  (if( < (getDistance plane (list-ref points loopDuration)) eps)
     (supportLoop plane points eps (+ n 1) (+ loopDuration 1))
     (supportLoop plane points eps n (+ loopDuration 1)))
  n))

; gets distance between point and plane returns a number
(define (getDistance plane point)
  (/ (abs(
       + (* (list-ref plane 0) (list-ref point 0)
         (* (list-ref plane 1) (list-ref point 1))
         (* (list-ref plane 2) (list-ref point 2))
         (list-ref plane 3))))
     (sqrt( +
           (expt (list-ref plane 0) 2)
           (expt (list-ref plane 1) 2)
           (expt (list-ref plane 2) 2)))
      ))

; write to file

(define (writeOutput filename list plane eps)
  (define out (open-output-file filename))
  (writeLoop list 0 plane eps out)
 (close-output-port out)
  )

; writes down all the supported points into a file
(define (writeLoop list n plane eps out)
  (if(< n (length list))
     (if(> eps (getDistance (list-ref plane 0) (list-ref list n)))
        (begin
         (display (string-append (number->string (list-ref (list-ref list n) 0))
                                "   " (number->string (list-ref (list-ref list n) 1))
                                 "   " (number->string (list-ref (list-ref list n) 2))  "\n") out)
         (writeLoop list (+ n 1) plane eps out))
        (writeLoop list (+ n 1) plane eps out))
     #t))

;/////////////////////////////////////////////////////////////////////////////////
; returns a dominant plane, uses a blan (() 0) as it will no doubt be replaced
(define (dominantPlane Ps k eps)
 (domPHelper '(() 0) 0 k Ps eps)
  )

; the helper function to create a loop
; A new plane and a new support plane which are then compared
; when the new plane has more supported points it will replace the currentplane
; it returns the plane with the most supported points
(define (domPHelper currentSupportPlane n k Ps eps)
  (define newPlane(plane
                   (list-ref Ps (random (length Ps)))
                   (list-ref Ps (random (length Ps)))
                   (list-ref Ps (random (length Ps)))
                   ))
  (define newSupportPlane(support newPlane Ps eps))
  (if(< n k)
    (if(< (list-ref currentSupportPlane 1) (list-ref newSupportPlane 1))
       (domPHelper newSupportPlane (+ n 1) k Ps eps)
       (domPHelper currentSupportPlane (+ n 1) k Ps eps)
       )
  currentSupportPlane))

; /////////// the last and the main function
; defines the list and most dominant planes
; and then uses them into write input
(define (planeRANSAC filename confidence percentage eps)
  (define list(readXYZ filename))
  (define domPlane(dominantPlane list (ransacNumberOfIterations confidence percentage) eps))
  (writeOutput "Point_Cloud_1_No_Road_Reduced_output.xyz" list domPlane eps) ; manually modify the file
 
  )


(planeRANSAC "Point_Cloud_1_No_Road_Reduced.xyz" 0.99 0.30 0.5)

; all of the below were test cases for the other programs to see if they gave correct values

;(ransacNumberOfIterations 0.99 0.5)
; above the tests is dominant plane.

;(define testPoints (readXYZ "Point_Cloud_1_No_Road_Reduced.xyz"))

;(dominantPlane testPoints (ransacNumberOfIterations 0.99 0.90) 0.5)

;(define a (plane
;  (list-ref (readXYZ "Point_Cloud_1_No_Road_Reduced.xyz") (random (length (readXYZ "Point_Cloud_1_No_Road_Reduced.xyz"))))
;  (list-ref (readXYZ "Point_Cloud_1_No_Road_Reduced.xyz") (random (length (readXYZ "Point_Cloud_1_No_Road_Reduced.xyz"))))
;  (list-ref (readXYZ "Point_Cloud_1_No_Road_Reduced.xyz") (random (length (readXYZ "Point_Cloud_1_No_Road_Reduced.xyz"))))
;      ))
;(domPHelper '(() 0) 0 20 testPoints 0.5)
;(list-ref (support a testPoints 0.5) 1) ;used to test support points
;(supportLoop a testPoints 0.5 0 0) ;this was used to test the support function
; I use an eps of 0.5
 