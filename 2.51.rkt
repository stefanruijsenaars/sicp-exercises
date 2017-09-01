#lang racket/gui
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))


(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (let ((start-coord-map ((frame-coord-map frame) (start-segment segment)))
             (end-coord-map ((frame-coord-map frame) (end-segment segment))))
       (line
        (make-posn (xcor-vect start-coord-map) (ycor-vect start-coord-map))
        (make-posn (xcor-vect end-coord-map) (ycor-vect end-coord-map)))))
     segment-list)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define outline-frame-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0)
                  (make-vect 0 1))
    (make-segment (make-vect 0 1)
                  (make-vect 1 1))
    (make-segment (make-vect 1 1)
                  (make-vect 1 0))
    (make-segment (make-vect 1 0)
                  (make-vect 0 0)))))

(define x-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0)
                  (make-vect 1 1))
    (make-segment (make-vect 0 1)
                  (make-vect 1 0)))))

(define diamond-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0.5)
                  (make-vect 0.5 0))
    (make-segment (make-vect 0.5 0)
                  (make-vect 1 0.5))
    (make-segment (make-vect 1 0.5)
                  (make-vect 0.5 1))
    (make-segment (make-vect 0.5 1)
                  (make-vect 0 0.5)))))

(define wave-painter
  (segments->painter
   (list
    (make-segment (make-vect 0.5 0.4) ;;; leg triangle
                  (make-vect 0.6 0))
    (make-segment (make-vect 0.5 0.4)
                  (make-vect 0.4 0))
    (make-segment (make-vect 0.3 0)
                  (make-vect 0.35 0.4))
    (make-segment (make-vect 0.35 0.4)
                  (make-vect 0.3 0.7))
    (make-segment (make-vect 0.3 0.7)
                  (make-vect 0.2 0.6))
    (make-segment (make-vect 0.2 0.6)
                  (make-vect 0 0.8))
    (make-segment (make-vect 0 0.9)
                  (make-vect 0.2 0.7))
    (make-segment (make-vect 0.2 0.7)
                  (make-vect 0.3 0.75))
    (make-segment (make-vect 0.3 0.75)
                  (make-vect 0.4 0.75))
    (make-segment (make-vect 0.4 0.75)
                  (make-vect 0.35 0.9))
    (make-segment (make-vect 0.35 0.9)
                  (make-vect 0.4 1))
    (make-segment (make-vect 0.5 1)
                  (make-vect 0.55 0.9))
    (make-segment (make-vect 0.55 0.9)
                  (make-vect 0.5 0.75))
    (make-segment (make-vect 0.5 0.75)
                  (make-vect 0.6 0.75))
    (make-segment (make-vect 0.6 0.75)
                  (make-vect 1 0.45))
    (make-segment (make-vect 1 0.3)
                  (make-vect 0.6 0.5))
    (make-segment (make-vect 0.6 0.5)
                  (make-vect 0.7 0)))))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate-counterclockwise-180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate-counterclockwise-270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define unit-frame (make-frame (make-vect 0 500) (make-vect 500 0) (make-vect 0 -500)))


(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below-alt painter1 painter2)
  (rotate-counterclockwise-270 (beside (rotate-counterclockwise-270 (rotate-counterclockwise-270 (rotate-counterclockwise-270 painter1))) (rotate-counterclockwise-270 (rotate-counterclockwise-270 (rotate-counterclockwise-270 painter2))))))

((below-alt wave-painter wave-painter) unit-frame)