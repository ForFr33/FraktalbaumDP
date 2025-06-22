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

(define-struct vector(polar))
;A vector is a structure: (make-vector polar)
;interp. a line in 2-dimesnional space

;A color is a structure: (make-color(red green blue alpha))
;interp. The representation of red, green, blue and alpha value as color.

(define MAIN-SCENE (empty-scene 400 400))
(define START-POINT (make-cartesian 0 0))
(define BRANCH-VECTOR (make-vector (make-polar 10 (/ pi 3))))




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
(check-expect (polar->cartesian (make-polar 1 0)) (make-cartesian 1 0))
(check-error (polar->cartesian "polar") "polar has to be a number!")
(check-range (cartesian-x (polar->cartesian (make-polar 3 (sin 25))))  2.9 3)
(check-within (polar->cartesian (make-polar 3 (sin 25)))
              (make-cartesian (* (cos 25) 3) (* (sin 25) 3)) 
              0.01)    
(define (polar->cartesian polar) 
  (if (number? polar)
  (make-cartesian
   (* (magnitude polar) (cos (angle polar)))
   (* (magnitude polar) (sin (angle polar))))
  (error "polar has to be a number!")))
   

;cartesian, vector, color, scene -> scene
;puts line in color from position into scene
(define (put-branch posn vector color scene)
   (add-line
    scene
    (cartesian-x posn) (cartesian-y posn)
    (cartesian-x (polar->cartesian (vector-polar vector)))
    (cartesian-y (polar->cartesian (vector-polar vector)))
        color))


   

;cartesian, color, scene -> scene
;takes a position as cartesian, a color and a scene and returns the scene with a circle at posn in color
(define (put-blossom posn color scene)
  (place-image
   (circle 1 "solid" color)
   (cartesian-x posn) (cartesian-y posn)
   scene))

;cartesian, polar, number, number, list-of-colors -> cartesian, color, scene, -> scene
(define (tree start-posn vector branch-angle growth-relation list-of-colors)
  (if (null? (rest list-of-colors))
      (put-blossom start-posn (last list-of-colors)scene)
      ;TODO
    (put-branch start-posn vector (first list-of-colors) (tree start-posn vector branch-angle growth-relation (rest list-of-colors)))))
    ;(local []()) ;Angepasste konstanten für rekursive Aufrufe
        ;(tree start-posn vector branch-angle growth-relation (rest list-of-colors))
        ;(tree  start-posn vector branch-angle growth-relation (rest list-of-colors)))
     


;(put-blossom (polar->cartesian (vector-polar BRANCH-VECTOR)) "blue" (put-branch START-POINT BRANCH-VECTOR "red"  MAIN-SCENE))


(tree START-POINT BRANCH-VECTOR (/ pi 2) 2 '("blue" "red" "green" "orange"))















  
