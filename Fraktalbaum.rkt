;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Fraktalbaum) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp")) #f)))

;A posn is either:
;- a polar
;- a cartesian
;interp. a position in 2-dimensional space

;A polar is a structure: (make-polar magnitude angle)
;interp a point in 2-dimensional space depicted as complexx Number

(define-struct cartesian (x y))
;A cartesian is a structure: (make-cartesian-posn Number Number)
;interp. the x and y coordinate of a point int 2-dimensional space

(define-struct vector(length angle))
;A vector is a structure: (make-vector polar)
;interp. a line in 2-dimesnional space

;A color is a structure: (make-color(red green blue alpha))
;interp. The representation of red, green, blue and alpha value as color.

(define MAIN-SCENE (empty-scene 600 600))
(define START-POINT (make-cartesian 0 0))
(define BRANCH-VECTOR (make-vector 10 (/ pi 3)))




;[X] list-of-X -> X
;returns the last X in list-of-X
(check-expect (last '(1 2 3)) 3)
(check-expect (last (list  1 empty)) '())
(check-error (last '()) "no empty lists allowed")
(check-error (last empty) "no empty lists allowed")
(check-error (last null) "no empty lists allowed")

(define (last list)
 (if (empty? list) (error "no empty lists allowed")
     (cond
       [(null? (rest list)) (first list)]
       [else (last (rest list))])))

;[X] list-of-X -> list-of-X
;returns a list-of-X without its last element
(check-expect (start '(1 2 3))  '(1 2))
(check-error (start '()) "no empty lists allowed")
(check-expect (start (cons 1 (cons 2 empty))) '(1))

(define (start lst)
  (if (empty? lst) (error "no empty lists allowed")
      (cond
        [(null? (rest lst)) '()]
        [else (cons (first lst) (start (rest lst)))])))


;polar -> cartesian
;converts polar to its cartesian depiction
(check-expect (polar->cartesian 1 0) (make-cartesian 1 0))
(check-error (polar->cartesian "1" 0) "length and angle have to be numbers!")
(check-error (polar->cartesian 1 "0") "length and angle have to be numbers!")
(check-range (cartesian-x (polar->cartesian 3 (sin 25)))  2.9 3)
(check-within (polar->cartesian 3 (sin 25))
              (make-cartesian (* (cos 25) 3) (* (sin 25) 3)) 
              0.01)    

(define (polar->cartesian length angle)
  (if (and (number? length) (number? angle))
  (make-cartesian
   (* length (cos angle))
   (* length (sin angle)))
    (error "length and angle have to be numbers!")))

;cartesian, vector, color, scene -> scene
;puts line in color from position into scene
(define (put-branch posn vector color scene)
   (add-line
    scene
    (cartesian-x posn) (cartesian-y posn)
    (+ (cartesian-x posn) (cartesian-x (polar->cartesian (vector-length vector) (vector-angle vector))))
    (+ (cartesian-y posn) (cartesian-y (polar->cartesian (vector-length vector) (vector-angle vector))))
        color)) 


   

;cartesian, color, scene -> scene
;takes a position as cartesian, a color and a scene and returns the scene with a circle at posn in color
(define (put-blossom posn color scene)
  (place-image
   (circle 2 "solid" color)
   (cartesian-x posn) (cartesian-y posn)
   scene))

;cartesian, polar, number, number, list-of-colors -> cartesian, color, scene, -> scene
(define (tree start-posn vector branch-angle growth-relation loc)    
    (cond
      [(empty? loc) MAIN-SCENE]
      [(empty? (rest loc)) (put-blossom start-posn (last loc) MAIN-SCENE)]
      [else 
       (local (
              [define angle (vector-angle vector)]
              [define length (vector-length vector)]
              [define vector-left (make-vector (* length growth-relation) (- angle branch-angle))]
              [define vector-right (make-vector (* length growth-relation) (+ angle branch-angle))]
              [define delta (polar->cartesian length angle)]
              [define end-pos (make-cartesian (+ (cartesian-x start-posn) (cartesian-x delta))
                                              (+ (cartesian-y start-posn) (cartesian-y delta)))]
              [define left (put-branch start-posn vector (first loc) (tree end-pos vector-left branch-angle growth-relation (rest loc)))]
              [define right (put-branch start-posn vector (first loc) (tree end-pos vector-right branch-angle growth-relation (rest loc)))])
            
               (put-branch start-posn vector (first loc) (put-branch end-pos vector-left (first loc) (put-branch end-pos vector-right (first loc) left))))]))
           

(define (tree1 sp v ba gr loc scene)
  (cond
    [(empty? loc) scene]
    [(empty? (rest loc)) (put-blossom sp (last loc) scene)]
    [else
     (local (
             [define angle (vector-angle v)]
             [define length (vector-length v)]
             [define vl (make-vector (* length gr) (- angle ba))]
             [define vr (make-vector (* length gr) (+ angle ba))]
             [define delta (polar->cartesian length angle)]
             [define ep (make-cartesian (+ (cartesian-x sp) (cartesian-x delta))
                                              (+ (cartesian-y sp) (cartesian-y delta)))])
       (put-branch sp v (first loc) (put-branch ep vl (first loc) (put-branch ep vr (first loc) (tree1 ep vl ba gr (rest loc) (tree1 ep vr ba gr (rest loc) scene))))))])) 
      
 

     


;(put-blossom (polar->cartesian (vector-polar BRANCH-VECTOR)) "blue" (put-branch START-POINT BRANCH-VECTOR "red"  MAIN-SCENE))

(define POSN (make-cartesian 300 400))
(define v (make-vector 120 (/ (- pi) 2)))
;(put-branch (make-cartesian 300 400) (make-vector 120 (/ (- pi) 2)) "red" (empty-scene 600 600))
(tree POSN v (/ (* 2 pi) 5) 0.66 '("blue" "blue" "blue" "blue" "blue" "blue" "blue" "blue" "pink"))
(tree1 POSN v (/ (* 2 pi) 5) 0.66 '("blue" "blue" "blue" "blue" "blue" "blue" "pink") (empty-scene 600 600))














  
