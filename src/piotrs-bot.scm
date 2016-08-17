;;; A sample test robot -- with graphics
(define (random-cmd)
  (let ((r (random 1.0)))
    (cond ((< r 0.5) 'forward)
	  ((< r 0.7) 'right)
	  ((< r 0.9) 'left)
	  ((< r 2.0) 'shoot)
	  )
    )
  )

(define coord 
  '((left (-1 0))
    (right (1 0))
    (up (-1 0))
    (down (1 0))))

;; This is the function where the bulk of the code goes -- it takes 
;; the state of the system (parsed-data) and returns 
(define (test-bot-update-graphical-pre parsed-data)

  (list (random-cmd) (random-cmd))
)

(define (test-bot-update-graphical-pre g parsed-data)
  (if (not (eqv? g #f))
      (draw-map g (cdr (assq 'text-map parsed-data)) 
		(car (cdr (assq 'map-size parsed-data)))
		(cadr (cdr (assq 'map-size parsed-data)))
		)
      '()
      )
  (test-bot-update-graphical-pre parsed-data)
)

(define (run-test-bot-graphical g delay mouser-sys max-turns data)
  (if (> 0 delay) (sleep-current-thread delay))
  (if (equal? max-turns 0)
      #t
      (run-test-bot-graphical g delay
			      mouser-sys 
			      (- max-turns 1) 
			      (update-system mouser-sys 
					    (test-bot-update-graphical g data))
			      )
      )
  )

;; To run this robot, try (test-bot #t 500 50)
(define (test-bot-graphical render? delay max-turns)
  (let* ((mouser-sys (mouser-init))
	(initial-data (get-input mouser-sys))
	(g (if render? (graphics-init (cadr (assq 'map-size initial-data))
				      (caddr (assq 'map-size initial-data))
		)
	       #f))
	)
    (run-test-bot-graphical g
			    delay
			    mouser-sys
			    max-turns
			    initial-data)
;    (deinit mouser-sys)
    )
)
