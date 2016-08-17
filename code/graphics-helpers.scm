(define (graphics-init x y)
  (let ((g (make-graphics-device (car (enumerate-graphics-types)))))
    (graphics-set-coordinate-limits g 0 (+ 1 y) x 0)
    g))

(define (draw-wall g x y)
  (graphics-draw-line g x y (+ x 1) (+ y 1))
  (graphics-draw-line g x (+ y 1) (+ x 1) y)
  (graphics-draw-line g x y (+ x 1) (+ y 1))

  (graphics-draw-line g x y (+ x 1) y)
  (graphics-draw-line g x y x (+ y 1))
  (graphics-draw-line g (+ x 1) (+ y 1) (+ x 1) y)
  (graphics-draw-line g (+ x 1) (+ y 1) x (+ y 1))
  )

(define (draw-point g x y)
  (graphics-draw-line g (+ x .65) (+ y .5) (+ x .5) (+ y .65))
  (graphics-draw-line g (+ x .65) (+ y .5) (+ x .5) (+ y .35))  
  (graphics-draw-line g (+ x .35) (+ y .5) (+ x .5) (+ y .65))
  (graphics-draw-line g (+ x .35) (+ y .5) (+ x .5) (+ y .35))  
)

(define (draw-right g x y)
  (graphics-draw-line g (+ x .05) (+ y .05) (+ x .95) (+ y .5))
  (graphics-draw-line g (+ x .05) (+ y .95) (+ x .95) (+ y .5))  
  (graphics-draw-line g (+ x .05) (+ y .05) (+ x .05) (+ y .95))
)

(define (draw-left g x y)
  (graphics-draw-line g (+ x .95) (+ y .05) (+ x .05) (+ y .5))
  (graphics-draw-line g (+ x .95) (+ y .95) (+ x .05) (+ y .5))  
  (graphics-draw-line g (+ x .95) (+ y .05) (+ x .95) (+ y .95))
)

(define (draw-down g x y)
  (graphics-draw-line g (+ x .05) (+ y .05) (+ x .5) (+ y .95))
  (graphics-draw-line g (+ x .95) (+ y .05) (+ x .5) (+ y .95))  
  (graphics-draw-line g (+ x .05) (+ y .05) (+ x .95) (+ y .05))
)

(define (draw-up g x y)
  (graphics-draw-line g (+ x .05) (+ y .95) (+ x .5)  (+ y .05))
  (graphics-draw-line g (+ x .95) (+ y .95) (+ x .5)  (+ y .05))  
  (graphics-draw-line g (+ x .05) (+ y .95) (+ x .95) (+ y .95))
)

(define (draw-right2 g x y)
  (graphics-draw-line g (+ x .05) (+ y .35) (+ x .95) (+ y .5))
  (graphics-draw-line g (+ x .05) (+ y .65) (+ x .95) (+ y .5))  

  (graphics-draw-line g (+ x .25) (+ y .25) (+ x .25) (+ y .75))
  (graphics-draw-line g (+ x .05) (+ y .05) (+ x .05) (+ y .95))
)

(define (draw-left2 g x y)
  (graphics-draw-line g (+ x .95) (+ y .35) (+ x .05) (+ y .5))
  (graphics-draw-line g (+ x .95) (+ y .65) (+ x .05) (+ y .5))  

  (graphics-draw-line g (+ x .75) (+ y .25) (+ x .75) (+ y .75))
  (graphics-draw-line g (+ x .95) (+ y .05) (+ x .95) (+ y .95))
)

(define (draw-down2 g x y)
  (graphics-draw-line g (+ x .35) (+ y .05) (+ x .5) (+ y .95))
  (graphics-draw-line g (+ x .65) (+ y .05) (+ x .5) (+ y .95))  

  (graphics-draw-line g (+ x .25) (+ y .25) (+ x .75) (+ y .25))
  (graphics-draw-line g (+ x .05) (+ y .05) (+ x .95) (+ y .05))
)

(define (draw-up2 g x y)
  (graphics-draw-line g (+ x .35) (+ y .95) (+ x .5)  (+ y .05))
  (graphics-draw-line g (+ x .65) (+ y .95) (+ x .5)  (+ y .05))  

  (graphics-draw-line g (+ x .25) (+ y .75) (+ x .75) (+ y .75))
  (graphics-draw-line g (+ x .05) (+ y .95) (+ x .95) (+ y .95))
)



(define (dont-draw g x y)
  #t
)

(define graphics-map-data
  (list (cons #\X draw-wall)
	(cons #\. draw-point)
	(cons #\< draw-left)
	(cons #\> draw-right)
	(cons #\^ draw-up)
	(cons #\v draw-down)
	(cons #\l draw-left2)
	(cons #\r draw-right2)
	(cons #\u draw-up2)
	(cons #\d draw-down2)
	(cons #\space dont-draw)
	)
  )

(define ((draw-object g t-map) x y) 
  (let* ((proc (cdr (assq (string-ref (list-ref t-map y) x) 
			  graphics-map-data))))
    (proc g x y)
    )
)

(define (iter-coord helper x y xmax)
  (helper x y)
  (cond ((and (= 0 x) (= 0 y)) '())
	((= x 0) (iter-coord helper xmax (- y 1) xmax))
	(else (iter-coord helper (- x 1) y xmax))
	)
)

(define (draw-map g data)
  (let ((t-map (cdr (assq 'text-map data)))
	(x (car (cdr (assq 'map-size data))))
	(y (cadr (cdr (assq 'map-size data))))
	)
    (graphics-clear g)
    (iter-coord (draw-object g t-map) (- x 1) (- y 1) (- x 1))
    (graphics-draw-text g 2 (+ y 0.95) 
			(fold-right string-append 
				    "" 
				    (map 
				     (lambda (x) 
				       (string-append x " ")
				       ) 
				     (map number->string 
					  (cdr (assq 'score data))))))
    )
)
