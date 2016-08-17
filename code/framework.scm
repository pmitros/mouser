(load "graphics-helpers.scm")

(define (mouser-init) (open-tcp-stream-socket "localhost" 12345))

(define deinit close-port)

(define (get-map-helper port rv)
  (let ((str (read-line port)))
;    (write-line (string-append "[" str"]" ))
    (if (equal? str "DONE")
	rv
	(get-map-helper port (append rv (list str)))
	)
    )
  )

(define (get-input-base port) 
  (get-map-helper port '())
)

(define (command-symbol->letter sym)
  (cond ((equal? sym 'forward) #\F)
	((equal? sym 'right) #\R)
       ((equal? sym 'shoot) #\S)
	((equal? sym 'left) #\L))
)

(define (write-command port cmd)
  (let* ((cmd-letters (map command-symbol->letter cmd))
	 (cmd-string (list->string cmd-letters)))
    (write-string cmd-string port)
    (newline port)
    (flush-output port)
    )
)

; "Now is the time 7 8.1" => (Now is the time 7 8.1)
(define (string-parse str)
  (with-input-from-string str
    (lambda ()
      (let lp ((x (read)))
	(if (eof-object? x)
	    ()
	    (cons x (lp (read)))))))
  )

; Checks if something is a map fragment
; "Hello" 7.5 (3.14 2.71) ==> #f
; "Xkjlfdk" "Xaklj2opk" ==> #t
(define (map-fragment? str)
  (if (and (string? str)
	   (equal? (string-ref str 0) #\X))
      #t
      #f
      )
  )

(define (score? list)
  (if (and (list? list)
	   (equal? (car list) 'Score))
      #t
      #f
      )
  )

(define (dir-number->symbol num)
  (cond ((equal? num 0) 'up)
	((equal? num 1) 'right)
	((equal? num 2) 'down)
	((equal? num 3) 'left))
)

(define (dir-symbol->number sym)
  (cond ((equal? sym 'up) 0)
	((equal? sym 'right) 1)
	((equal? sym 'down) 2)
	((equal? sym 'left) 3))
)

(define (map-char->symbol char)
  (cond ((equal? char #\X) 'wall)
	((equal? char #\space) 'empty)
	((equal? char #\>) 'robot)
	((equal? char #\<) 'robot)
	((equal? char #\^) 'robot)
	((equal? char #\v) 'robot)
	((equal? char #\u) 'robot)
	((equal? char #\r) 'robot)
	((equal? char #\d) 'robot)
	((equal? char #\l) 'robot)
	((equal? char #\.) 'dot)
	(else 'unknown))
)

(define (dead-number->bool dead)
  (= dead 1)
)

(define (map-parse map)
  map
)

; Take a map ("XXX" "X.X" "XXX")
; returns a lambda s.t. (x 1 1) returns "." and (x 0 0) returns "X"
(define (lambda-wrap-map basic-map)
  (lambda (x y)
    (map-char->symbol (string-ref (list-ref basic-map y) x))))



(define (robot-parse robot)
  (list 
	(cons 'num (list-ref robot 0))
	(cons 'x (list-ref robot 1))
	(cons 'y (list-ref robot 2))
	(cons 'd (dir-number->symbol (list-ref robot 3)))
	(cons 'alive (= (list-ref robot 4) 0)))
)

(define (robots-parse robots)
  (map robot-parse robots)
  )


(define (score-parse score)
  (cdr score)
)

(define (robot-wrap robot lambda-map)
  (lambda (x y)
    (if (and (= x (cdr (assq 'x robot)))
	     (= y (cdr (assq 'y robot)))
	     )
	(list 'robot robot)
	(lambda-map x y)
	)))

;; Converts text into a Scheme structure -- still needs work
(define (parse-server-struct base-input)
  (let* (
	 (input (delete-matching-items base-input 
				       (lambda (t) (substring? "Commands" t))))
	 (unparsed-map (keep-matching-items input map-fragment?))
	 (parsed-map (lambda-wrap-map unparsed-map))
	 (remainder (delete-matching-items input map-fragment?))
	 (parsed-remainder (map string-parse remainder))
	 (scores (car (keep-matching-items parsed-remainder score?)))
	 (parsed-scores (score-parse scores))
	 (robots (delete-matching-items parsed-remainder score?))
	 (parsed-robots (robots-parse robots))
	 (lambda-map (fold-right robot-wrap parsed-map parsed-robots))
	 (map-size (list (string-length (car unparsed-map)) 
			 (length unparsed-map)))
	 ) 
    (list (cons 'map-size map-size)
          (cons 'text-map unparsed-map)
	  (cons 'lambda-map lambda-map)
	  (cons 'score parsed-scores)
	  (cons 'robots parsed-robots)
	  )
	 )
    )

(define (get-input port)
  (parse-server-struct (get-input-base port))
)

;; Write a command, and pass back the feedback from the system
(define (update-system port cmd)
  (write-command port cmd)
  (get-input port)
)

(load "testbot-graphical.scm")
(load "testbot.scm")
(load "contest-draw.scm")
