;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Fraktalbaum) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp")) #f)))
;A posn is either:
; - A cartesian
; - A polar

(define-struct cartesian (x y))
;A cartesian is a structure: (make-cartesian Number Number)
;interp. the x and y coordinate of a point int 2-dimensional space

;A polar is a structure (make-polar Number Number)
;interp. the polar depiction of a point in 2-dimensional space

;Anmerkung:
;ich hätte eigentlich auch gerne ein polar struct definiert, allerdings gibt es in racket/base bereits ein solches und ich war mit nicht sicher,
;ob wir dieses benutzen dürfen und da der name "polar" bereits dafür vergeben ist habe ich es so gemacht, dass ein polar einfach als winkel zusammen mit
;der Länge einer Geraden im zwei-dimensionalen Raum dargestellt wird, ich hoffe, dass das kein Problem ist

(define-struct vector(length angle))
;A vector is a structure: (make-vector Number Number)
;interp. a line in 2-dimensional space

(define-struct WorldState (start-posn branch-vector branch-angle growth-relation list-of-colors scene))
;A WorldState is a structure: (make-WorldState Number Number Number list-of-colors scene)
;interp. the current state of the Program represented as image

; A KeyEvent is one of these strings:
; - "up"
; - "down"
; - "left"
; - "right"
; - "+"
; - "-"
; - "c"
; - "w"
; - "a"
; - "s"
; - "d"
; - "l"
; - "v"

;A color is a structure: (make-color(Number Number Number))
;interp. The representation of a color as its red green and blue value

;constants:
(define START_POSN (make-cartesian 300 400))
(define DEFAULT_VECTOR (make-vector 120 (/ (- pi) 2)))
(define MAIN_SCENE (empty-scene 600 600))
(define DEFAULT_GROWTH_RELATION 0.66)
(define DEFAULT_BRANCH_ANGLE (/ (* 2 pi) 5))
(define DEFAULT_COLOR_LIST (list (make-color 0 0 0) (make-color  255 0 0) (make-color 0 255 0) (make-color 0 0 255) (make-color 0 255 0)))
(define DEFAULT_WORLD_STATE (make-WorldState START_POSN DEFAULT_VECTOR DEFAULT_BRANCH_ANGLE DEFAULT_GROWTH_RELATION DEFAULT_COLOR_LIST MAIN_SCENE))


;vector, cartesian -> cartesian
;calculates the endpoint of v, where c is the starting point of v
(check-within (calc-endpoint DEFAULT_VECTOR (make-cartesian 0 0)) (make-cartesian (+ 0 (cartesian-x (polar->cartesian (vector-length DEFAULT_VECTOR) (vector-angle DEFAULT_VECTOR))))
                                                                                  (+ 0 (cartesian-y (polar->cartesian (vector-length DEFAULT_VECTOR) (vector-angle DEFAULT_VECTOR))))) 0.01)
                                                                                                 
(define (calc-endpoint v c)
  (make-cartesian
   (+ (cartesian-x c)
      (cartesian-x (polar->cartesian (vector-length v) (vector-angle v))))
   (+ (cartesian-y c)
      (cartesian-y (polar->cartesian (vector-length v) (vector-angle v)))))) 

;[X] list-of-X -> X
;returns the last X in list-of-X
(check-expect (last '(1 2 3)) 3)
(check-expect (last (list 1 2 3 4)) 4)
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
;converts a posn in polar depiction to its cartesian depiction
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
   (cartesian-x (calc-endpoint vector posn))
   (cartesian-y (calc-endpoint vector posn))
   color))
(define TEST_SCENE_BRANCH (scene+line (empty-scene 100 100) 10 10 20 20 "red"))
;interp. scene to test if put-branch behaves as expected
"Test for put-branch, test is successfull if there is a green line and nothing else visible"
(put-branch (make-cartesian 10 10) (make-vector (sqrt (+ 400 400)) (atan 1)) "green" TEST_SCENE_BRANCH)

;cartesian, color, scene -> scene
;takes a position as cartesian, a color and a scene and returns the scene with a circle at posn in color
(define (put-blossom posn color scene)
  (place-image
   (circle 5 "solid" color)
   (cartesian-x posn) (cartesian-y posn) 
   scene))

(define TEST_SCENE_BLOSSOM (place-image (circle 5 "solid" "red") 10 10 (empty-scene 100 100)))
;interp. scene to test if put-blossom behaves as expected
"Test for put-blossom, test is successfull if there is a green circle in the top left corner of an otherwise empty scene and no red is visible"
(put-blossom (make-cartesian 10 10) "green" TEST_SCENE_BLOSSOM)

;cartesian, vector, number, number, list-of-colors, scene -> scene
;draws branch from sp in direction of v in the first color in loc if loc still has color, else if there are no colors in loc left draws blossom in last color in loc
(define (tree sp v ba gr loc scene)
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
             [define ep (calc-endpoint (make-vector length angle) sp)])
       (put-branch sp v (first loc) (put-branch ep vl (first loc) (put-branch ep vr (first loc) (tree ep vl ba gr (rest loc) (tree ep vr ba gr (rest loc) scene))))))]))  
     
;WorldState -> Image
;takes ws and renders it onto a scene
(define (render ws)
  (tree (WorldState-start-posn ws) (WorldState-branch-vector ws) (WorldState-branch-angle ws) (WorldState-growth-relation ws) (WorldState-list-of-colors ws) (WorldState-scene ws))) 

;WorldState, KeyEvent -> WorldState
;changes properties of current world state based of given KeyEvents
(check-within (change (change (change DEFAULT_WORLD_STATE "up") "left") "+") (make-WorldState
                                                                              (WorldState-start-posn DEFAULT_WORLD_STATE)
                                                                              (WorldState-branch-vector DEFAULT_WORLD_STATE)
                                                                              (+ (WorldState-branch-angle DEFAULT_WORLD_STATE) 0.1)
                                                                              (+ (WorldState-growth-relation DEFAULT_WORLD_STATE) 0.01)
                                                                              (append (list (last (WorldState-list-of-colors DEFAULT_WORLD_STATE))) (start (WorldState-list-of-colors DEFAULT_WORLD_STATE)))
                                                                              (WorldState-scene DEFAULT_WORLD_STATE))
                                                                              0.001)
(check-within (change (change (change DEFAULT_WORLD_STATE "down") "right") "-") (make-WorldState
                                                                                 (WorldState-start-posn DEFAULT_WORLD_STATE)
                                                                                 (WorldState-branch-vector DEFAULT_WORLD_STATE)                           
                                                                                 (- (WorldState-branch-angle DEFAULT_WORLD_STATE) 0.1)
                                                                                 (- (WorldState-growth-relation DEFAULT_WORLD_STATE) 0.01)
                                                                                 (append (rest (WorldState-list-of-colors DEFAULT_WORLD_STATE)) (list (first (WorldState-list-of-colors DEFAULT_WORLD_STATE))))
                                                                                 (WorldState-scene DEFAULT_WORLD_STATE))
                                                                                 0.001)
(define (change ws key)
  (cond
    [(key=? key "up") (make-WorldState (WorldState-start-posn ws) (WorldState-branch-vector ws)  (+ (WorldState-branch-angle ws) 0.1) (WorldState-growth-relation ws) (WorldState-list-of-colors ws) (WorldState-scene ws))]
    [(key=? key "down") (make-WorldState (WorldState-start-posn ws) (WorldState-branch-vector ws)  (- (WorldState-branch-angle ws) 0.1) (WorldState-growth-relation ws) (WorldState-list-of-colors ws) (WorldState-scene ws))]
    [(key=? key "left") (make-WorldState (WorldState-start-posn ws) (WorldState-branch-vector ws) (WorldState-branch-angle ws) (+ (WorldState-growth-relation ws) 0.01) (WorldState-list-of-colors ws) (WorldState-scene ws))]
    [(key=? key "right") (make-WorldState (WorldState-start-posn ws)(WorldState-branch-vector ws) (WorldState-branch-angle ws) (- (WorldState-growth-relation ws) 0.01) (WorldState-list-of-colors ws) (WorldState-scene ws))]
    [(key=? key "+") (make-WorldState (WorldState-start-posn ws) (WorldState-branch-vector ws) (WorldState-branch-angle ws) (WorldState-growth-relation ws)  (append (list (last (WorldState-list-of-colors ws))) (start (WorldState-list-of-colors ws))) (WorldState-scene ws))]
    [(key=? key "-") (make-WorldState (WorldState-start-posn ws) (WorldState-branch-vector ws)  (WorldState-branch-angle ws) (WorldState-growth-relation ws)  (append (rest (WorldState-list-of-colors ws)) (list (first (WorldState-list-of-colors ws)))) (WorldState-scene ws))]
    [(key=? key " ") (make-WorldState (WorldState-start-posn ws) (WorldState-branch-vector ws)  (WorldState-branch-angle ws) (WorldState-growth-relation ws) (map (lambda (c) (make-color (color-green c) (color-blue c) (color-red c))) (WorldState-list-of-colors ws)) (WorldState-scene ws))]
    [(key=? key "c") (make-WorldState (WorldState-start-posn ws) (WorldState-branch-vector ws) (WorldState-branch-angle ws) (WorldState-growth-relation ws) (append (list (make-color (random 256) (random 256) (random 256))) (WorldState-list-of-colors ws)) (WorldState-scene ws))]
    [(key=? key "d") (make-WorldState (make-cartesian (+ (cartesian-x (WorldState-start-posn ws)) 20) (cartesian-y (WorldState-start-posn ws))) (WorldState-branch-vector ws)  (WorldState-branch-angle ws) (WorldState-growth-relation ws)  (WorldState-list-of-colors ws) (WorldState-scene ws))]
    [(key=? key "s") (make-WorldState (make-cartesian (cartesian-x (WorldState-start-posn ws)) (+ (cartesian-y (WorldState-start-posn ws)) 20)) (WorldState-branch-vector ws)  (WorldState-branch-angle ws) (WorldState-growth-relation ws)  (WorldState-list-of-colors ws) (WorldState-scene ws))]
    [(key=? key "a") (make-WorldState (make-cartesian (- (cartesian-x (WorldState-start-posn ws)) 20) (cartesian-y (WorldState-start-posn ws))) (WorldState-branch-vector ws)  (WorldState-branch-angle ws) (WorldState-growth-relation ws)  (WorldState-list-of-colors ws) (WorldState-scene ws))]
    [(key=? key "w") (make-WorldState (make-cartesian (cartesian-x (WorldState-start-posn ws)) (- (cartesian-y (WorldState-start-posn ws)) 20)) (WorldState-branch-vector ws)  (WorldState-branch-angle ws) (WorldState-growth-relation ws)  (WorldState-list-of-colors ws) (WorldState-scene ws))]
    [(key=? key "l") (make-WorldState (WorldState-start-posn ws) (WorldState-branch-vector ws) (WorldState-branch-angle ws) (WorldState-growth-relation ws) (append (WorldState-list-of-colors ws) (list (make-color (random 256) (random 256) (random 256)))) (WorldState-scene ws))]
    [(key=? key "v") (make-WorldState (WorldState-start-posn ws) (make-vector (+ (vector-length (WorldState-branch-vector ws)) 10) (vector-angle (WorldState-branch-vector ws)))  (WorldState-branch-angle ws) (WorldState-growth-relation ws) (WorldState-list-of-colors ws) (WorldState-scene ws))]
    [(key=? key "r") DEFAULT_WORLD_STATE]
    [else (error (string-append "change is not defined for the key " key))]))    
     

(render (make-WorldState
 (make-cartesian 300 600)
 (make-vector 140 #i-1.5707963267948966)
  #i0.8566370614359171
  0.66
 (list
  (make-color 170 116 23)
  (make-color 180 80 190)
  (make-color 210 78 153)
  (make-color 210 21 153)
  (make-color 100 53 167)
  (make-color 71 92 110)
  (make-color 72 89 214)
  (make-color 117 88 1)
  (make-color 0 0 0)
  (make-color 255 0 0)
  (make-color 0 255 0)
  (make-color 0 0 255)
  (make-color 0 255 0)
  (make-color 245 184 169))
 MAIN_SCENE))

     
;installs all event handlers and initializes WorldState with default values
(big-bang DEFAULT_WORLD_STATE
  (on-key change)
  (to-draw render)) 






  
