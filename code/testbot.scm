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

;; This is the function where the bulk of the code goes -- it takes 
;; the state of the system (parsed-data) and returns 
(define (test-bot-update parsed-data)
  (list (random-cmd) (random-cmd))
)

(define (run-test-bot mouser-sys max-turns data)
  (if (equal? max-turns 0)
      #t
      (run-test-bot mouser-sys 
		    (- max-turns 1) 
		    (update-system mouser-sys 
				   (test-bot-update data))
		    )
      )
  )

;; To run this robot, try (test-bot #t 500 50)
(define (test-bot max-turns)
  (let* ((mouser-sys (mouser-init))
	(initial-data (get-input mouser-sys))
	)
    (run-test-bot mouser-sys
		  max-turns
		  initial-data)
;    (deinit mouser-sys)
    )
)
