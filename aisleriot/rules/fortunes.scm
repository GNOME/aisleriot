; Aisleriot - camelot.scm
; Copyright (C) 1998 Rosanna Yuen <rwsy@mit.edu>

(define (new-game)
  (initialize-playing-area)
  (set-ace-high)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK)

  (add-blank-slot)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (list 6 4)
)

(define (button-pressed slot-id card-list)
  (and (> slot-id 0)
       (or (empty-slot? 1)
	   (empty-slot? 2)
	   (empty-slot? 3)
	   (empty-slot? 4))))

(define (button-released start-slot card-list end-slot)
  (if (empty-slot? end-slot)
      (move-n-cards! start-slot end-slot card-list)
      #f))


(define (button-clicked slot-id)
  (if (empty-slot? slot-id)
      #f
      (if (= slot-id 0)
	  (begin 
	    (deal-cards 0 '(1 2 3 4))
	    (flip-top-card 1)
	    (flip-top-card 2)
	    (flip-top-card 3)
	    (flip-top-card 4))
	  (if (= slot-id 1)
	      (begin
		(if (and (not (empty-slot? 2))
			 (= (get-suit (get-top-card 1))
			    (get-suit (get-top-card 2)))
			 (< (get-value (get-top-card 1))
			    (get-value (get-top-card 2))))
		    (begin 
		      (remove-card slot-id) 
		      (add-to-score! 1))
		    (begin
		      (if (and (not (empty-slot? 3))
			       (= (get-suit (get-top-card 1))
				  (get-suit (get-top-card 3)))
			       (< (get-value (get-top-card 1))
				  (get-value (get-top-card 3))))
			  (begin 
			    (remove-card slot-id) 
			    (add-to-score! 1))
			  (begin
			    (if (and (not (empty-slot? 4))
				     (= (get-suit (get-top-card 1))
					(get-suit (get-top-card 4)))
				     (< (get-value (get-top-card 1))
					(get-value (get-top-card 4))))
				(begin 
				  (remove-card slot-id) 
				  (add-to-score! 1))))))))
	      (begin
		(if (= slot-id 2)
		    (begin
		      (if (and (not (empty-slot? 1))
			       (= (get-suit (get-top-card 2))
				  (get-suit (get-top-card 1)))
			       (< (get-value (get-top-card 2))
				  (get-value (get-top-card 1))))
			  (begin 
			    (remove-card slot-id) 
			    (add-to-score! 1))
			  (begin
			    (if (and (not (empty-slot? 3))
				     (= (get-suit (get-top-card 2))
					(get-suit (get-top-card 3)))
				     (< (get-value (get-top-card 2))
					(get-value (get-top-card 3))))
				(begin 
				  (remove-card slot-id) 
				  (add-to-score! 1))
				(begin
				  (if (and (not (empty-slot? 4))
					   (= (get-suit (get-top-card 2))
					      (get-suit (get-top-card 4)))
					   (< (get-value (get-top-card 2))
					      (get-value (get-top-card 4))))
				      (begin 
					(remove-card slot-id) 
					(add-to-score! 1))))))))
		    (begin 
		      (if (= slot-id 3)
			  (begin
			    (if (and (not (empty-slot? 1))
				     (= (get-suit (get-top-card 3))
					(get-suit (get-top-card 1)))
				     (< (get-value (get-top-card 3))
					(get-value (get-top-card 1))))
				(begin 
				  (remove-card slot-id) 
				  (add-to-score! 1))
				(begin
				  (if (and (not (empty-slot? 2))
					   (= (get-suit (get-top-card 3))
					      (get-suit (get-top-card 2)))
					   (< (get-value (get-top-card 3))
					      (get-value (get-top-card 2))))
				      (begin 
					(remove-card slot-id) 
					(add-to-score! 1))
				      (begin
					(if (and (not (empty-slot? 4))
						 (= (get-suit (get-top-card 3))
						    (get-suit (get-top-card 4)))
						 (< (get-value (get-top-card 3))
						    (get-value (get-top-card 4))))
					    (begin 
					      (remove-card slot-id) 
					      (add-to-score! 1))))))))
			  (begin 
			    (if (= slot-id 4)
				(begin
				  (if (and (not (empty-slot? 1))
					   (= (get-suit (get-top-card 4))
					      (get-suit (get-top-card 1)))
					   (< (get-value (get-top-card 4))
					      (get-value (get-top-card 1))))
				      (begin 
					(remove-card slot-id) 
					(add-to-score! 1))
				      (begin
					(if (and (not (empty-slot? 2))
						 (= (get-suit (get-top-card 4))
						    (get-suit (get-top-card 2)))
						 (< (get-value (get-top-card 4))
						    (get-value (get-top-card 2))))
					    (begin 
					      (remove-card slot-id) 
					      (add-to-score! 1))
					    (begin
					      (if (and (not (empty-slot? 3))
						       (= (get-suit (get-top-card 4))
							  (get-suit (get-top-card 3)))
						       (< (get-value (get-top-card 4))
							  (get-value (get-top-card 3))))
						  (begin 
						    (remove-card slot-id) 
						    (add-to-score! 1))))))))))))))))))
  
(define (button-double-clicked slot)
  #f)     
	  
(define (game-won borp)
  (and (empty-slot? 0)
       (= 1 (list-length (get-cards 1)))
       (= 1 (list-length (get-cards 2)))
       (= 1 (list-length (get-cards 3)))
       (= 1 (list-length (get-cards 4)))))
     
(define (game-over borp)
  (not (and (empty-slot? 0)
	    (and (not (empty-slot? 1))
		     (not (empty-slot? 2))
		     (not (empty-slot? 3))
		     (not (empty-slot? 4))
		     (not (= (get-suit (get-top-card 1))
			     (get-suit (get-top-card 2))))
		     (not (= (get-suit (get-top-card 1))
			     (get-suit (get-top-card 3))))		
		     (not (= (get-suit (get-top-card 1))
			     (get-suit (get-top-card 4))))
		     (not (= (get-suit (get-top-card 2))
			     (get-suit (get-top-card 3))))
		     (not (= (get-suit (get-top-card 2))
			     (get-suit (get-top-card 4))))
		     (not (= (get-suit (get-top-card 3))
			     (get-suit (get-top-card 4))))))))

(define (get-hint borp)
  #f)


(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint)
