;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Fraktalbaum) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp")) #f)))
(define-struct plar (magnitude angle))

(define-struct cartesian (x y))
;A cartesian is a structure: (make-cartesian Number Number)
;interp. the x and y coordinate of a point int 2-dimensional space

(define-struct vector(plar))
;A vector is a structure: (make-vector polar)
;interp. a line in 2 Dimensional represented as polar depiction of a complex number

;A color is a structure: (make-color(red green blue alpha))
;interp. The representation of red, green, blue and alpha value as color.

(define MAIN-SCENE (empty-scene 400 400))
(define START-POINT (make-cartesian 0 0))
(define BRANCH-VECTOR (make-vector (make-plar 10 (/ pi 3))))
                




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
;interp. takes the polar depiction of a point in 2-dimensional space
;and converts it two its cartesian depiction
(check-expect (polar->cartesian 0 1) (make-cartesian 1 0))
(check-error (polar->cartesian 0 "1") "error: angle and length have to be numbers!")
(check-range (cartesian-x (polar->cartesian (sin 25) 3))  2.9 3)
(check-within (polar->cartesian (sin 25) 3)
              (make-cartesian (* (cos 25) 3) (* (sin 25) 3)) 
              0.01)
    ; NEUE TESTS!!!!
(define (polar->cartesian plr)
  (if (and (real? (plar-magnitude plr)) (real? (plar-angle plr))) 
  (make-cartesian
   (* (plar-magnitude plr) (cos (plar-angle plr)))
   (* (plar-magnitude plr) (sin (plar-angle plr))))
     (error "error: input has to be a polar!")))

;cartesian, vector, color, scene -> scene
;puts line in color from position into scene
(define (put-branch posn vector color scene)
  (place-image
   (add-line
    (rectangle 40 40 "solid" "white") ;ÄNDERN
    (cartesian-x posn) (cartesian-y posn)
    (cartesian-x (polar->cartesian (vector-plar vector)))
    (cartesian-y (polar->cartesian (vector-plar vector)))
    "black")
    200 200
    scene))

   

;cartesian, color, scene -> scene
;takes a position as cartesian, a color and a scene and returns the scene with a circle at posn in color
(define (put-blossom posn color scene)
  (empty-scene 100 100))

;cartesian, polar, branch-angle, growth-relation, list-of-colord -> cartesian, color, scene, -> scene
(define (tree start-posn vector branch-angle growth-relation list-of-colors)
  (if (null? rest list-of-colors)
      (put-blossom start-posn (last list-of-colors) (empty-scene 100 100))
      ;TODO
    (put-branch start-posn vector (first list-of-colors))))
    ;(local []()) ;Angepasste konstanten für rekursive Aufrufe
        ;(tree start-posn vector branch-angle growth-relation (rest list-of-colors))
        ;(tree  start-posn vector branch-angle growth-relation (rest list-of-colors)))
     

(put-branch START-POINT BRANCH-VECTOR "blue"  MAIN-SCENE)




















  
