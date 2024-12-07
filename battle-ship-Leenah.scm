; battle-ship.scm

(import image)
(import canvas)
(import reactive)
(import lab)
(problem "This file lacks documentation on some functions, and there's a lot of commneted code (under experimentation.)")



(define white-peg
  (overlay
    (circle 10 "outline" "black")
    (circle 10 "solid" "white")))

(define red-peg
  (overlay
    (circle 10 "outline" "black")
    (circle 10 "solid" "red")))


;from Maddy
;;; (square-piece size) -> drawing?
;;;    size : integer? size > 0
;;; Returns a solid, grey square of size 'size' with a black circle 
;;; of radius 'size/5' overlayed on top of it
(define square-piece
  (lambda (size)
    (overlay/align "middle" "center"
                    (circle (/ size 5) "solid" "black") 
                    (square size "outline" "black")
                    (square size "solid" "gray"))))



;;; (grid-vec vec i)-> void?
;;;    vec : vector?
;;;    i : integer? represents the index of the vector
;;; Recursively goes through the elements in 'vec'
;;; and sets each elements to be 'square-piece'
(define grid-vec
  (lambda (vec i griddy)
    (if (>= i (vector-length vec))
      void
      (begin
      (if (equal? (vector-ref griddy i) 1)
        (vector-set! vec i (overlay red-peg (square-piece 50)))
        (vector-set! vec i (square-piece 50)))
      (grid-vec vec (+ i 1) griddy)
      vec))))

;;; (grid) -> vector? of-image?
;;; Returns the drif of the game as a vector

(define grid0 (make-vector 100 0))
(define grid1 (make-vector 100 0))


(define grid
  (grid-vec grid0 0))


;;; Try overlying the ships

(define make-ship
  (lambda (size)
    (let
      ([ship-base (solid-rectangle size (/ size 3) "green")]
       [ship-edge (solid-triangle (/ size 3) "green")])
        (beside (rotate -90 ship-edge) 
                ship-base 
                (rotate 90 ship-edge)))))


;;; use 'ship' for testing
(define ship
  (make-ship 150))
; (vector-set! grid 5 ship)

; Idk how to overlay a ship on top of the vector. 
; How can I make the ship perfectly fit horizontally and perfectly on the vector grid?
 ; ship



;;; incomplete
; (define vector->canvas
;   (lambda (vec i row)
;   (let
;     ([canv (make-canvas 100 100)]))))

; (define make-row
;   (lambda (vec i width height x y)
;     (let ([canv (make-canvas width height)])
;       (if (>= x width)
;           void
;           (begin
;            (canvas-drawing! canv x y (vector-ref vec i)) 
;            (make-row vec (+ i 1) width height (+ x 50) y)
;             canv)))))
; (make-row grid 0 500 500 0 0)

;;; flame animation code
(define flame-parts
  (lambda (width height color)
    (path width height (list (pair (* width 0.5) (* height 0.1))  ; Top point
      (pair (* width 0.3) (* height 0.4))
      (pair (* width 0.2) (* height 0.6))
      (pair (* width 0.25) (* height 0.8))
      (pair (* width 0.4) (* height 0.9))
      (pair (* width 0.5) (* height 0.95))
      (pair (* width 0.6) (* height 0.9))
      (pair (* width 0.75) (* height 0.8))
      (pair (* width 0.8) (* height 0.6))
      (pair (* width 0.7) (* height 0.4)))
      "solid" color)))

(define flame-part2
  (lambda (width height)
    (overlay/align "middle" "bottom"
      (flame-parts (/ width 1.5) (/ height 1.5) "yellow")
      (flame-parts width height "orange"))))

(define flame
  (lambda (width height)
    (overlay/offset (* width -0.335) (* height -0.64)
      (flame-parts (/ width 3) (/ height 3) "white")
      (flame-part2 width height))))

(define flame-canvas (make-canvas 150 150)) ; Smaller canvas size

(define flame-animation
  (lambda (time)
    (let* ([base-size 80] ; Adjusted base size for the smaller canvas
           [scale (+ 0.8 (* 0.2 (sin (/ time 250))))] ; Scale between 0.8 and 1.0
           [shake-x (truncate (* 3 (sin (/ time 50))))] ; Convert to integer
           [shake-y (truncate (* 2 (cos (/ time 70))))] ; Convert to integer
           [flame-width (* base-size scale)]
           [flame-height (* base-size scale)]
           [flame (flame flame-width flame-height)])
      (begin
        ;; Clear canvas
        (canvas-rectangle! flame-canvas 0 0 150 150 "solid" "white")
        ;; Draw the animated flame at the center of the canvas
        (canvas-drawing! flame-canvas 
          (+ 75 shake-x) (+ 75 shake-y) flame) ; Center flame at (75, 75)
        #t)))) ; Return #t to allow animation to continue

;; Start the animation
(ignore (animate-with flame-animation))

;The code below works
(part "A reactive grid of pegs")
;;; The code below is using the 'reactive library' for more information check the
;;; 'Interactivity, Events, and Reactivity' reading.



;initial model struct
(struct state (peg x y griddy-p1 griddy-p2 turns))


(define grid-maker
  (lambda (st canvass i k j vec)
  (if (equal? k 500)
    null
    (begin (canvas-drawing! canvass i k (vector-ref vec j))
      (if (< i 450)
        (grid-maker st canvass (+ i 50) k (+ j 1) vec)
        (grid-maker st canvass 0 (+ k 50) (+ j 1) vec))))))

(define view
  (lambda (st canvass)
    (match st
      [(state peg x y griddy-p1 griddy-p2 turns)
        (begin
          (ignore
            (begin 
          ;;; This is purposefully low-level for testing
            (if (negative? turns)
            (grid-maker st canvass 0 0 0 (grid-vec (make-vector 100 0) 0 griddy-p1)); (canvas-circle! canvass x y 20 "solid" "blue");
            (grid-maker st canvass 0 0 0 (grid-vec (make-vector 100 0) 0 griddy-p2)); (canvas-circle! canvass x y 20 "solid" "red") ;
              ))))])))
      
;the update function  
(define update
  (lambda (msg st)
    (match st
      [(state peg x y griddy-p1 griddy-p2 turns)
        (match msg
          [(event-mouse-click btn cx cy)
            (if (negative? turns)
            (let* ([a (vector-set! griddy-p1 (truncate (+ (* (/ cy 50) 10) (/ cx 50))) 1)])
              (state peg x y griddy-p1 griddy-p2 (* turns -1)))
            (let* ([a (vector-set! griddy-p2 (truncate (+ (* (/ cy 50) 10) (/ cx 50))) 1)])
              (state peg x y griddy-p1 griddy-p2 (* turns -1)))
              )])])))

                      
             

(display
  (reactive-canvas
    500 500
    ;initial model
      (state (circle 10 "solid" "black") 15 15 grid0 grid1 1)
    ;view function
      view
    ;update fucntion
      update; eventualy this will need to update the grid function.
    ;subscriptions
    (on-mouse-click)))

(problem "Click anywhere on the canvas to place the peg. 
  \n Tip: You have to click exactly in the middle of the 
          black circle to place the red/white peg accurately.
  \n Tip: The fist click makes a red peg, click a second time to place a white peg on the grid")

