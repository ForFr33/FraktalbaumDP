;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Fraktalbaum) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp")) #f)))

;A posn is either:
;- a polar
;- a cartesian
;interp. a position in 2-dimensional space

(define-struct cartesian (x y))
;A cartesian is a structure: (make-cartesian Number Number)
;interp. the x and y coordinate of a point int 2-dimensional space

(define-struct vector(length angle))
;A vector is a structure: (make-vector ComplexNumber Number)
;interp. a line in 2-dimesnional space

;Anmerkung:
;ich hätte eigentlich auch gerne ein polar struct definiert, allerdings gibt es in racket/base bereits ein solches und ich war mit nicht sicher,
;ob wir dieses benutzen dürfen und da der name "polar" bereits dafür vergeben ist habe ich es so gemacht, dass ein polar einfach als winkel zusammen mit
;der Länge einer Geraden im zwei-dimensionalen Raum dargestellt wird, ich hoffe, dass das kein Problem ist

;A color is a structure: (make-color(Number Number Number))
;interp. The representation of red, green, blue and alpha value as color.


(define POSN (make-cartesian 300 400))
;interp. the default start position of the fractal tree
(define v (make-vector 120 (/ (- pi) 2)))
;interp. the default branch vector of the fractal tree
(define MAIN-SCENE (empty-scene 600 600))
;interp. the canvas the fractal tree is drawn onto
(define BRANCH-VECTOR (make-vector 10 (/ pi 3)))
(define DEFAULT_GROWTH 0.66)
(define DEFAULT_BRANCH_ANGLE (/ (* 2 pi) 5))
(define DEFAULT_COLOR_LIST (list (make-color 0 0 0) (make-color  255 0 0) (make-color 0 255 0) (make-color 0 0 255) (make-color 0 255 0))) 

(define-struct WorldState (branch-angle growth-relation list-of-colors scene))
;A WorldState is a structure: (make-WorldState ComplexNumber Number list-of-colors scene)
;interp. the current state of the Program

(define DEFAULT_WORLD_STATE (make-WorldState (/ (* 2 pi) 5) 0.5 DEFAULT_COLOR_LIST MAIN-SCENE))
;interp. the default WorldState bigbang is being called with

;vector, cartesian -> cartesian
;calculates the endpoint c of v
(define (calc-endpoint v c)
  (make-cartesian
   (+ (cartesian-x c)
      (cartesian-x (polar->cartesian (vector-length v) (vector-angle v))))
   (+ (cartesian-y c)
      (cartesian-y (polar->cartesian (vector-length v) (vector-angle v))))))

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
   (circle 5 "solid" color)
   (cartesian-x posn) (cartesian-y posn)
   scene))

           
;cartesian, vector, number, number, list-of-colors, scene -> scene
;draws branch from sp in direction of v in the first color in loc if loc still
(define (tree1 sp v ba gr loc scene)
  (cond
    [(empty? loc) scene]
    [(empty? (rest loc)) (put-blossom (calc-endpoint v sp) (last loc) scene)]
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
     

;WorldState -> WorldState
;takes ws and renders it
(define (render ws)
 (tree1 POSN v (WorldState-branch-angle ws) (WorldState-growth-relation ws) (WorldState-list-of-colors ws) (WorldState-scene ws))) 

;WorldStat, key -> number
(check-within (WorldState-branch-angle (change DEFAULT_WORLD_STATE "up"))  (+ DEFAULT_BRANCH_ANGLE 0.1) 0.001) 
;(check-expect (changge "down") ...)
;(check-expect (change "left") ...)
;(check-expect (change "right") ...)
(define (change ws key)
  (cond
    ([key=? key "up"] (make-WorldState (+ (WorldState-branch-angle ws) 0.1) (WorldState-growth-relation ws) (WorldState-list-of-colors ws) (WorldState-scene ws)))
    ([key=? key "down"] (make-WorldState (- (WorldState-branch-angle ws) 0.1) (WorldState-growth-relation ws) (WorldState-list-of-colors ws) (WorldState-scene ws)))
    ([key=? key "left"] (make-WorldState (WorldState-branch-angle ws) (+ (WorldState-growth-relation ws) 0.01) (WorldState-list-of-colors ws) (WorldState-scene ws)))
    ([key=? key "right"] (make-WorldState (WorldState-branch-angle ws) (- (WorldState-growth-relation ws) 0.01) (WorldState-list-of-colors ws) (WorldState-scene ws)))
    ([key=? key "plus"] ...)
    ([key=? key "minus"] ...)))
   

(big-bang DEFAULT_WORLD_STATE
  (on-key change)
  (to-draw render)) 





  
