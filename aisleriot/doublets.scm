; Aisleriot - doublets.scm
; Copyright (C) 1998 Rosanna Yuen <rwsy@mit.edu>

(define FLIP-COUNTER 0)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK)
  (add-normal-slot '())

  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)

  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (add-blank-slot)
  (add-normal-slot '())
  (add-carriage-return-slot)

  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (deal-cards-face-up 0 '(2 3 4 5 6 7 8 9))

  (check-kings '(2 3 4 5 6 7 8 9))

  (add-to-score! 1)

  (set! FLIP-COUNTER 0)
  (list 6 3)
)

(define (check-kings slot-list)
  (if (= (get-value (get-top-card (car slot-list))) king)
      (begin
	(let ((new-deck (get-cards 0)))
	      (set-cards! 0 (reverse (cons (make-card (get-value (get-top-card (car slot-list)))
						      (get-suit (get-top-card (car slot-list))))
					   (reverse new-deck)))))
	(remove-card (car slot-list))
	(deal-cards-face-up 0 (cons (car slot-list) '()))
	(check-kings slot-list))
      (if (> (list-length slot-list) 1)
	  (check-kings (cdr slot-list)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (not (= slot-id 0))
       (not (= slot-id 8))
       (not (= (get-value (car card-list)) king))))

(define (button-released start-slot card-list end-slot)
  (if (= end-slot 8)
      (if (or (= (get-value (car card-list))
		 (* 2 (get-value (get-top-card end-slot))))
	      (= (get-value (car card-list))
		 (- (* 2 (get-value (get-top-card end-slot))) 13)))
	  (begin
	    (add-to-score! 1)
	    (move-n-cards! start-slot end-slot card-list)
	    (cond ((not (empty-slot? 1))
		   (deal-cards 1 (cons start-slot '())))
		  ((not (empty-slot? 0))
		   (deal-cards-face-up 0 (cons start-slot '())))
		  (#t #t)))
	  #f)
      #f))

(define (button-clicked slot-id)
  (if (= slot-id 0)
      (cond ((and (empty-slot? 0)
		  (not (empty-slot? 1)))
	     (begin
	       (set! FLIP-COUNTER (+ 1 FLIP-COUNTER))
	       (flip-cards-back 2)))
	    ((not (empty-slot? 0))
	     (add-card! 1 (flip-card (remove-card 0))))
	    (#t #f))
      #f))

(define (button-double-clicked slot)
  (if (and (not (empty-slot? slot))
	   (not (= slot 0))
	   (not (= slot 8))
	   (not (= (get-value (get-top-card slot)) king))
	   (= (get-value (get-top-card slot))
	      (modulo (* 2 (get-value (get-top-card 8))) 13)))
      (begin
	(add-to-score! 1)
	(deal-cards slot '(8))
	(cond ((not (empty-slot? 1))
	       (deal-cards 1 (cons slot '())))
	      ((and (not (empty-slot? 0))
		    (not (= slot 1)))
	       (deal-cards-face-up 0 (cons slot '())))
	      (#t #t)))
      #f))

(define (check-move slot)
  (if (and (not (empty-slot? slot))
	   (= (get-value (get-top-card slot))
	      (modulo (* 2 (get-value (get-top-card 8))) 13)))
      #t
      (if (< slot 9)
	  (check-move (+ 1 slot))
	  #f)))

(define (game-over borp)
  (or (< FLIP-COUNTER 2)
      (not (empty-slot? 0))
      (check-move 1)))

(define (game-won borp)
  (= 48 (list-length (get-cards 8))))

(define (get-hint borp)
  (let ((wanted (modulo (* 2 (get-value (get-top-card 8))) 
			13)))
    (list 4 (get-value-name wanted))))

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint)
