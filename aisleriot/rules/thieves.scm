; Aisleriot - Thieves
; Copyright (C) 1999 Robert Brady <rwb197@ecs.soton.ac.uk>

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-joker-deck)
  
  (shuffle-deck)
  (add-partially-extended-slot '() down 5)
  (add-partially-extended-slot '() down 5)
  (add-partially-extended-slot '() down 5)
  (add-partially-extended-slot '() down 5)
  (add-partially-extended-slot '() down 5)
  (add-partially-extended-slot '() down 5)
  (add-partially-extended-slot '() down 5)
  (add-carriage-return-slot)
  (add-carriage-return-slot)
  (add-partially-extended-slot DECK right 10 )
  (add-normal-slot '())
  (deal-cards-face-up 7 '(0 1 2 3 4 5 6 0 1 2 3 4 5 6 0 1 2 3 4 5 6 0 1 2 3 4 5 6 0 1 2 3 4 5 6 8))
  (list 7 3)
)

(define (values-match? c1 c2)
  (or (eq? (get-value c1) joker)
      (eq? (get-value c2) joker)
      (eq? (+ 1 (get-value c1)) (get-value c2))
      (eq? (get-value c1) (+ 1 (get-value c2))))
)

(define (score-for card)
  (if (eq? card ace) 8
  (if (eq? card 2) 6
  (if (eq? card 3) 6
  (if (eq? card 4) 4
  (if (eq? card 5) 4
  (if (eq? card 6) 2
  (if (eq? card 7) 2
  (if (eq? card 8) 2
  (if (eq? card 9) 4
  (if (eq? card 10) 4
  (if (eq? card jack) 6
  (if (eq? card queen) 6
  (if (eq? card king) 8
  0))))))))))))))

(define (can-move-from where)
  (and (not (empty-slot? where))
       (not (empty-slot? 8))
       (values-match? (get-top-card where) (get-top-card 8))))

(define (move-possible)
  (or (can-move-from 0)
      (can-move-from 1)
      (can-move-from 2)
      (can-move-from 3)
      (can-move-from 4)
      (can-move-from 5)
      (can-move-from 6)))

(define (button-pressed slot-id card-list) #f)
(define (button-released start-slot card-list end-slot) #f)
(define (button-clicked slot-id) 
  (if (eq? slot-id 7)
      (if (empty-slot? slot-id)
	  #f
	  (add-card! 8 (flip-card (remove-card 7)))
      )
      (if (< slot-id 7)
	  (if (values-match? (get-top-card slot-id) (get-top-card 8))
              (begin
		(add-to-score! (score-for (get-value (get-top-card slot-id))))
		(add-card! 8 (remove-card slot-id))
              )
          )
      )
  )
)
(define (button-double-clicked slot) #f)
(define (game-won) (and (empty-slot? 0) (empty-slot? 1) (empty-slot? 2) 
                        (empty-slot? 3) (empty-slot? 4) (empty-slot? 5) 
                       (empty-slot? 6)))

(define (game-over) 
  (if (game-won) 
    #f
    (if (empty-slot? 7) 
	(if (move-possible)
	    #t 
	    #f)
	#t)
    )
)

(define (get-hint) (list 0 "Try clicking somewhere"))
(define (get-options) #f)
(define (apply-options options) #f)
(define (timeout) #f)

(set-lambda new-game button-pressed button-released button-clicked 
  button-double-clicked game-over game-won get-hint get-options apply-options
  timeout)
