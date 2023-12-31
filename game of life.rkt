;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |game of life|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct cell (x y state))
; A cell is a (make-cell integer integer boolean)

; move-seed : nat nat [ListOf cells] -> [ListOf cells]
; move every cell in the list by x y
(define (move-seed x y loc)
  (map (lambda (c) (make-cell (+ x (cell-x c)) (+ y (cell-y c)) (cell-state c))) loc))

(define (living x y) (make-cell x y #t))

; SEED DEFINITIONS
(define GLIDER-SEED (list (make-cell 1 1 #t)
                          (make-cell -1 0 #t)
                          (make-cell 1 0 #t)
                          (make-cell 0 -1 #t)
                          (make-cell 1 -1 #t)))
(define BLINKER-SEED (list (make-cell -1 0 #t)
                           (make-cell 0 0 #t)
                           (make-cell 1 0 #t)))
(define GLIDER+BLINKER (append (move-seed 5 0 BLINKER-SEED) GLIDER-SEED))
(define SQUARE (list (make-cell 0 0 #t)
                     (make-cell 1 0 #t)
                     (make-cell 0 -1 #t)
                     (make-cell 1 -1 #t)))
(define EATER-1 (list (living 0 0) (living 1 0)
                      (living 0 -1) (living 2 -1)
                      (living 2 -2)
                      (living 2 -3) (living 3 -3)))
(define GLIDER-GUN
  (append
   (move-seed 1 -4 SQUARE)
   (move-seed 35 -2 SQUARE)
   (list (living 25 0)
         (living 23 -1) (living 25 -1)
         (living 13 -2) (living 14 -2) (living 21 -2) (living 22 -2)
         (living 12 -3) (living 16 -3) (living 21 -3) (living 22 -3)
         (living 11 -4) (living 17 -4) (living 21 -4) (living 22 -4)
         (living 11 -5) (living 15 -5) (living 17 -5) (living 18 -5) (living 23 -5) (living 25 -5)
         (living 11 -6) (living 17 -6) (living 25 -6)
         (living 12 -7) (living 16 -7)
         (living 13 -8) (living 14 -8))))

(define GUN+EATER (append GLIDER-GUN (move-seed 39 -24 EATER-1)))

; adjacent?: cell cell -> boolean
; Determines whether the 2 cells are adjacent
(check-expect (adjacent? (make-cell 0 0 #f) (make-cell 0 0 #f)) #f)
(check-expect (adjacent? (make-cell 0 0 #f) (make-cell 1 0 #f)) #t)
(check-expect (adjacent? (make-cell 0 0 #f) (make-cell 1 1 #f)) #t)
(check-expect (adjacent? (make-cell 0 0 #f) (make-cell 0 2 #f)) #f)
(check-expect (adjacent? (make-cell 2 2 #t) (make-cell 0 0 #f)) #f)

(define (adjacent? cell1 cell2)
  (and (not (and (= (cell-x cell1) (cell-x cell2)) (= (cell-y cell1) (cell-y cell2))))
       (or (and (>= (cell-x cell1) (cell-x cell2))
                (< (cell-x cell1) (+ (cell-x cell2) 2)))
           (and (>= (cell-x cell2) (cell-x cell1))
                (< (cell-x cell2) (+ (cell-x cell1) 2))))
       (or (and (>= (cell-y cell1) (cell-y cell2))
                (< (cell-y cell1) (+ (cell-y cell2) 2)))
           (and (>= (cell-y cell2) (cell-y cell1))
                (< (cell-y cell2) (+ (cell-y cell1) 2))))))

; adjacent-cells: cell [listOf cells] -> [listOf cells]
; produces a list of the living cells adjacent to the input cell
(check-expect (adjacent-cells (make-cell -1 1 #f) GLIDER-SEED)
              (list (make-cell -1 0 #true)))

(define (adjacent-cells cell loc)
  (filter (lambda (c) (and (adjacent? cell c) (cell-state c))) loc))

; update-cell: cell [ListOf cells] -> cell
; takes a cell and the list of all cells and applies the rules from conway's game of life to that cell
(check-expect (update-cell (make-cell 0 0 #f) GLIDER-SEED)
              (make-cell 0 0 #f))
(check-expect (update-cell (make-cell 0 0 #t) BLINKER-SEED) (make-cell 0 0 #t))

(define (update-cell cell loc)
  (if (or (and (cell-state cell)
               (<= (length (adjacent-cells cell loc)) 3)
               (>= (length (adjacent-cells cell loc)) 2))
          (= (length (adjacent-cells cell loc)) 3))
      (make-cell (cell-x cell) (cell-y cell) #t)
      (make-cell (cell-x cell) (cell-y cell) #f)))

; bounds : max/min cell-x/cell-y [ListOf cells] -> Int
; abstraction for finding coordinates at the edge of the list
(define (bounds func dimen loc)
  (foldl (lambda (c a) (func (dimen c) a)) 0 loc))

(check-expect (xmax GLIDER-SEED) 1)
(define (xmax loc) (bounds max cell-x loc))
(check-expect (xmin GLIDER-SEED) -1)
(define (xmin loc) (bounds min cell-x loc))
(define (ymax loc) (bounds max cell-y loc))
(define (ymin loc) (bounds min cell-y loc))

; change-active-area : [ListOf cells] -> [ListOf cells]
; makes a list of every living cell and dead cell adjacent to a living cell
(define (change-active-area loc)
  (local [(define living-cells (filter cell-state loc))
          (define (cells-around c) (list (make-cell (sub1 (cell-x c)) (add1 (cell-y c)) #f)
                                         (make-cell (cell-x c) (add1 (cell-y c)) #f)
                                         (make-cell (add1 (cell-x c)) (add1 (cell-y c)) #f)
                                         (make-cell (sub1 (cell-x c)) (cell-y c) #f)
                                         (make-cell (add1 (cell-x c)) (cell-y c) #f)
                                         (make-cell (sub1 (cell-x c)) (sub1 (cell-y c)) #f)
                                         (make-cell (cell-x c) (sub1 (cell-y c)) #f)
                                         (make-cell (add1 (cell-x c)) (sub1 (cell-y c)) #f)))]
    (foldl (lambda (c a) (cond [(member? c a) a]
                               [(member? (make-cell (cell-x c) (cell-y c) #t) a) a]
                               [else (cons c a)]))
           living-cells (foldl append '() (map cells-around living-cells)))))
     
  

(define BG-SIZE 1000)
; draw-board : [ListOf cells] -> image
;
(define (draw-board loc)
  (foldr (lambda (c s) (place-image (square 10 "solid" (if (cell-state c) "black" "white"))
                                    (+ (/ BG-SIZE 10) (* 10 (cell-x c)))
                                    (+ (/ BG-SIZE 10) (* -10 (cell-y c)))
                                    s))
         (square BG-SIZE "solid" "white")
         loc))

; update-board : [ListOf cells] -> [ListOf cells]
; 
(define (update-board loc)
  (map (lambda (c) (update-cell c loc)) (change-active-area loc)))

(define (nth-state n loc)
  (cond [(= n 0) loc]
        [else (nth-state (- n 1) (update-board loc))]))


(big-bang GUN+EATER
  [to-draw draw-board]
  [on-tick update-board 0.25]
  [stop-when (lambda (loc) (> (xmax loc) 500))])

