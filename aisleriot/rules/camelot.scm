; Aisleriot - camelot.scm
; Copyright (C) 1998 Rosanna Yuen <rwsy@mit.edu>

(define add-stage #t)

(define (new-game)
  (initialize-playing-area)
  (make-standard-deck)
  (shuffle-deck)
 
  (add-normal-slot '())           ; Slot 0
  (add-normal-slot '())           ; Slot 1
  (add-normal-slot '())           ; Slot 2
  (add-normal-slot '())           ; Slot 3
  (add-carriage-return-slot)
  (add-normal-slot '())           ; Slot 4
  (add-normal-slot '())           ; Slot 5
  (add-normal-slot '())           ; Slot 6
  (add-normal-slot '())           ; Slot 7
  (add-carriage-return-slot)
  (add-normal-slot '())           ; Slot 8
  (add-normal-slot '())           ; Slot 9
  (add-normal-slot '())           ; Slot 10
  (add-normal-slot '())           ; Slot 11
  (add-carriage-return-slot)
  (add-normal-slot '())           ; Slot 12
  (add-normal-slot '())           ; Slot 13
  (add-normal-slot '())           ; Slot 14
  (add-normal-slot '())           ; Slot 15
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-normal-slot DECK)          ; Slot 16
  (add-normal-slot '())           ; Slot 17
)

(define (button-pressed slot-id card-list)
  (if (or (empty-slot? slot-id)
	  (and (= (get-value (car (get-cards slot-id))) king)
	       (or (= slot-id 0)
		   (= slot-id 3)
		   (= slot-id 12)
		   (= slot-id 15)))
	  (and (= (get-value (car (get-cards slot-id))) queen)
	       (or (= slot-id 1)
		   (= slot-id 2)
		   (= slot-id 13)
		   (= slot-id 14)))
	  (and (= (get-value (car (get-cards slot-id))) jack)
	       (or (= slot-id 4)
		   (= slot-id 7)
		   (= slot-id 8)
		   (= slot-id 11)))
	  (= slot-id 16))
      #f
      #t))

(define (button-released start-slot card-list end-slot)
  (if (= start-slot end-slot)
      (if (and (= (get-value (car card-list)) 10)
	       (not add-stage))
	  #t
	  #f)
      (if (empty-slot? end-slot)
	  (cond ((= (get-value (car card-list)) king)
		 (if (or (= end-slot 0)
			 (= end-slot 3)
			 (= end-slot 12)
			 (= end-slot 15))
		     (move-n-cards! start-slot end-slot card-list)
		     #f))
		((=  (get-value (car card-list)) queen)
		 (if (or (= end-slot 1)
			 (= end-slot 2)
			 (= end-slot 13)
			 (= end-slot 14))
		     (move-n-cards! start-slot end-slot card-list)
		     #f))
		((=  (get-value (car card-list)) jack)
		 (if (or (= end-slot 4)
			 (= end-slot 7)
			 (= end-slot 8)
			 (= end-slot 11))
		     (move-n-cards! start-slot end-slot card-list)
		     #f))
		((= start-slot 17)
		 (move-n-cards! start-slot end-slot card-list))
		(#t #f))
	  (if (and (= (+ (get-value (car card-list))
			 (get-value (car (get-cards end-slot))))
		      10)
		   (not add-stage))
	      (begin
		(remove-card end-slot))
	      #f))))

(define (button-clicked slot-id)  
  (if (= slot-id 16)
      (if (or (empty-slot? 16)
	      (not (empty-slot? 17)))
	  #f
	  (let ((top-card (remove-card 16)))
            (if (eq? top-card '())
                #f
                (add-card! 17 (flip-card top-card)))
	       (if (not (empty-slot? 16))(set! add-stage #t))))
      #f))

(define (button-double-clicked slot)
  #f)     

(define (game-won ugh)
  (if (and (empty-slot? 5)
	   (empty-slot? 6)
	   (empty-slot? 9)
	   (empty-slot? 10)
	   (empty-slot? 16)
	   (empty-slot? 17))
      #t
      #f))


(define (game-over ugh)
  (if (and add-stage
	   (not (empty-slot? 0))
	   (not (empty-slot? 1))
	   (not (empty-slot? 2))
	   (not (empty-slot? 3))
	   (not (empty-slot? 4))
	   (not (empty-slot? 5))
	   (not (empty-slot? 6))
	   (not (empty-slot? 7))
	   (not (empty-slot? 8))
	   (not (empty-slot? 9))
	   (not (empty-slot? 10))
	   (not (empty-slot? 11))
	   (not (empty-slot? 12))
	   (not (empty-slot? 13))
	   (not (empty-slot? 14))
	   (not (empty-slot? 15)))
      (begin 
	(set! add-stage #f)
	#t)
      #t)
  )

(define (get-hint ugh)
  #f)                                       

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint)  