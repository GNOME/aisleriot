; AisleRiot - accordion.scm
; Copyright (C) 2008 Ed Sirett <ed@makewrite.demon.co.uk>
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (aisleriot interface) (aisleriot api))

(define row1 '(0 1 2 3 4 5 6 7 8 ))
(define row2 '(9 10 11 12 13 14 15 16 17 ))
(define row3 '(18 19 20 21 22 23 24 25 26 ))
(define row4 '(27 28 29 30 31 32 33 34 35 ))
(define row5 '(36 37 38 39 40 41 42 43 44 ))
(define row6 '(45 46 47 48 49 50 51 ))


(define (add-full-line)
  (add-normal-slot '() )
  (add-normal-slot '() )
  (add-normal-slot '() )
  (add-normal-slot '() )
  (add-normal-slot '() )
  (add-normal-slot '() )
  (add-normal-slot '() )
  (add-normal-slot '() )
  (add-normal-slot '() )
  (add-carriage-return-slot)
)

(define (new-game)

  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)


  (add-full-line)
  (add-full-line)
  (add-full-line)
  (add-full-line)
  (add-full-line)
  (add-normal-slot '() )
  (add-normal-slot '() )
  (add-normal-slot '() )
  (add-normal-slot '() )
  (add-normal-slot '() )
  (add-normal-slot '() )
  (add-normal-slot '() )

  (deal-cards-face-up-from-deck DECK (append row1 row2 row3 row4 row5 row6))
  (give-status-message)
  (list 9 6)
)


(define (recalc-score last-slot) 
  (if 
    (not (empty-slot? last-slot)) 
    0 
    (+ 1 (recalc-score (- last-slot 1)))
  )
)

(define (give-status-message)
  (set-score! (recalc-score 51))
)





(define (button-clicked slot-id) 
  #f
)

(define  (sidle-up first-slot)
    (if (and (< first-slot 51)
             (not (empty-slot? (+ first-slot 1)))
             (empty-slot? first-slot)
        )
        (and  
            (move-n-cards! (+ first-slot 1) first-slot (list (get-top-card (+ first-slot 1))))
            (remove-card (+ first-slot 1))
            (sidle-up (+ first-slot 1))
        )
        #t
    )
)


(define (do-action end-slot start-slot card-list)
      (and
          (remove-card end-slot) 
          (move-n-cards! start-slot end-slot card-list)
          (if (not (empty-slot? start-slot)) (remove-card start-slot) #t)
          (sidle-up start-slot)
          (give-status-message)
      )
)

(define (button-released start-slot card-list end-slot)
   (if ( droppable? start-slot card-list end-slot)
      (do-action end-slot start-slot card-list) 
      #f  
  )
)

(define (matches-in-rank slot1 card) 
   (and (>= slot1 0) 
        (= (get-value (get-top-card slot1)) 
           (get-value card)
        )
   )
)

(define (matches-in-suit slot1 card) 
   (and (>= slot1 0) 
        (= (get-suit (get-top-card slot1)) 
           (get-suit card)
        )
   )
)

(define (button-pressed slot-id card-list) 
   (if (not (empty-slot? slot-id))
         (> slot-id 0)
         #f
   )
)




(define (playable? from-slot card)
   (or (playable-1? from-slot card) 
       (playable-3? from-slot card)
   )
)

(define (playable-3? from card)
    (and (>= from 3) 
	     (or (matches-in-suit  (- from 3) card ) 
	         (matches-in-rank  (- from 3) card )
	     )
    )
)

(define (playable-1? from card)
    ( and (>= from 1)
	   (or (matches-in-suit  (- from 1) card  ) 
	       (matches-in-rank  (- from 1) card  )
	   )
    )
)

(define (button-double-clicked slot-id) 
    (cond ((empty-slot? slot-id) #f)
          ((playable-3? slot-id (get-top-card slot-id))
             (do-action (- slot-id 3) slot-id (list (get-top-card slot-id)))
          )          
          ((playable-1? slot-id (get-top-card slot-id))
             (do-action (- slot-id 1) slot-id (list (get-top-card slot-id)))
          )
	  (else #f)          
    )
)


 
(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)
  )
)



(define (game-won)
  (and (empty-slot? 1) 
       (not (empty-slot? 0))
  )
)

(define (make-hint possible-move)
    (if (car possible-move)
           (hint-move (car possible-move) 1 (cadr possible-move)) 
           #f
    )
)


(define (find-playable-move start-slot)
	(cond ( (empty-slot? start-slot) 
              (list #f)
          )
          ( (playable-3? start-slot (get-top-card start-slot)) 
              (list start-slot (- start-slot 3))
          )
          ( (playable-1? start-slot (get-top-card start-slot)) 
              (list start-slot (- start-slot 1))
          )
          ( else 
              (find-playable-move (+ start-slot 1))
          )  
    )    
)


(define (get-hint)
       (make-hint (find-playable-move 1 ))
)

(define (droppable?  start-slot card-list  end-slot) 
  ( and 
       (not (empty-slot? end-slot))    
         (or (= (+ end-slot 1) start-slot)
             (= (+ end-slot 3) start-slot)
         )
         (or (matches-in-rank end-slot (car card-list))
             (matches-in-suit end-slot (car card-list))
         )
  )
)

(define (get-options) #f )

(define (apply-options options) #f)

(define (timeout) #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked 
            button-double-clicked game-continuable game-won get-hint 
            get-options apply-options timeout droppable?
)
