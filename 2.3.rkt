#lang sicp
(define (make-point x y) (cons x y))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment s)
  (let (
        (start-x (x-point (start-segment s)))
        (end-x   (x-point (end-segment s)))
        (start-y (y-point (start-segment s)))
        (end-y   (y-point (end-segment s)))
       )
    (make-point (average start-x end-x) (average start-y end-y))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point (midpoint-segment (make-segment (make-point 2 2) (make-point 4 6))))

; two perpendicular segments. first one horizontal, second one vertical
; only works for rectangles aligned with an axises
(define (make-rectangle s1 s2) (cons s1 s2))

(define (rect-horizontal rect)
  (car rect))

(define (rect-vertical rect)
  (cdr rect))

(define (length rect)
  (abs (- (x-point (start-segment (rect-horizontal rect))) (x-point (end-segment (rect-horizontal rect))))))
                   
(define (height rect)
  (abs (- (y-point (start-segment (rect-vertical rect))) (y-point (end-segment (rect-vertical rect))))))

(define (perimeter-rect rect)
  (* 2 (+ (length rect) (height rect))))

(define (area-rect rect)
  (* (length rect) (height rect)))

(define r1 (make-rectangle (make-segment (make-point 0 0) (make-point 10 0)) (make-segment (make-point 0 0) (make-point 0 5))))
(newline)
(area-rect r1)
(perimeter-rect r1)

; two points. bottom left, top right
(define (make-rectangle-alt p1 p2) (cons p1 p2))
(define (rect-bottomleft rect) (car rect))
(define (rect-topright rect) (cdr rect))

(define (length-alt rect)
  (abs (- (x-point (rect-bottomleft rect)) (x-point (rect-topright rect)))))

(define (height-alt rect)
  (abs (- (y-point (rect-bottomleft rect)) (y-point (rect-topright rect)))))

(define (perimeter-rect-alt rect)
  (* 2 (+ (length-alt rect) (height-alt rect))))

(define (area-rect-alt rect)
  (* (length-alt rect) (height-alt rect)))

(define r2 (make-rectangle-alt (make-point 0 0) (make-point 10 5)))
(newline)
(area-rect-alt r2)
(perimeter-rect-alt r2)