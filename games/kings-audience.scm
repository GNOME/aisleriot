; AisleRiot - king's_audience.scm
; Copyright (C) 2005 Zach Keene
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

(use-modules (aisleriot interface) (aisleriot api) (ice-9 format))

(define stock 11)
(define waste 12)
(define reserves '(0 1 2 3 4 9 10 13 14 19 20 21 22 23 24 25))
(define royal-discards '(5 6 7 8))
(define foundations '(15 16 17 18))

(def-save-var open-royal 5)
(def-save-var open-foundation 15)

(define (new-game)
  (initialize-playing-area)
  (make-standard-deck)
  (shuffle-deck)

  (add-blank-slot)
  (add-normal-slot '())         ; slot 0
  (add-normal-slot '())         ; slot 1
  (add-normal-slot '())         ; slot 2
  (add-normal-slot '())         ; slot 3
  (add-carriage-return-slot)

  (add-raised-slot '())         ; slot 4
  (add-extended-slot '() right) ; slot 5 (discard)
  (add-extended-slot '() right) ; slot 6 (discard)
  (add-extended-slot '() right) ; slot 7 (discard)
  (add-extended-slot '() right) ; slot 8 (discard)
  (add-raised-slot '())         ; slot 9
  (add-carriage-return-slot)

  (add-raised-slot '())         ; slot 10
  (add-blank-slot)
  (add-normal-slot DECK)        ; slot 11 (stock)
  (add-normal-slot '())         ; slot 12 (waste) 
  (add-blank-slot)
  (add-raised-slot '())         ; slot 13
  (add-carriage-return-slot)

  (add-raised-slot '())         ; slot 14
  (add-normal-slot '())         ; slot 15 (foundation)
  (add-normal-slot '())         ; slot 16 (foundation)
  (add-normal-slot '())         ; slot 17 (foundation)
  (add-normal-slot '())         ; slot 18 (foundation)
  (add-raised-slot '())         ; slot 19
  (add-carriage-return-slot)

  (add-raised-slot '())         ; slot 20
  (add-normal-slot '())         ; slot 21
  (add-normal-slot '())         ; slot 22
  (add-normal-slot '())         ; slot 23
  (add-normal-slot '())         ; slot 24
  (add-raised-slot '())         ; slot 25

  (set! open-royal 5)
  (set! open-foundation 15)

  (deal-cards-face-up stock reserves)
  (give-status)

  (list 6 5)
)

(define (add-raised-slot list)
  (set! VERTPOS (- VERTPOS 0.5))
  (add-normal-slot list)
  (set! VERTPOS (+ VERTPOS 0.5))
)

(define (give-status)
  (set-statusbar-message (format #f
                                 (G_"Stock remaining: ~a")
                                 (number->string (length (get-cards stock)))
                         )
  )
)


(define (button-pressed slot-id card-list)
  (member slot-id (append (list waste) reserves))
)

(define (droppable? start-slot card-list end-slot)
  (and (not (null? (car card-list)))
       (not (= start-slot end-slot))
       (not (= end-slot stock))
       (or (pair? (car card-list) (get-top-card end-slot) king queen)
           (pair? (car card-list) (get-top-card end-slot) ace jack)
           (and (not (empty-slot? end-slot))
                (member end-slot foundations)
                (= (get-suit (car card-list)) 
                   (get-suit (get-top-card end-slot)))
                (= (+ (get-value (car card-list)) 1) 
                   (get-value (get-top-card end-slot))
                )
           )
       )
  )
)

(define (pair? card1 card2 rank1 rank2)
  (and (not (null? card1))
       (not (null? card2))
       (= (get-suit card1) (get-suit card2))
       (or (and (= rank1 (get-value card1)) (= rank2 (get-value card2)))
           (and (= rank1 (get-value card2)) (= rank2 (get-value card1)))
       )
  )
)

(define (button-released start-slot card-list end-slot)
  (if (droppable? start-slot card-list end-slot)
      (if (member end-slot foundations)
          (begin
            (move-n-cards! start-slot end-slot card-list)
            (add-to-score! 1)
            (fill-gaps reserves)
          )
          (if (or (= ace (get-value (car card-list))) 
                  (= jack (get-value (car card-list)))
              )
              (move-pair start-slot card-list end-slot open-foundation)
              (move-pair start-slot card-list end-slot open-royal)
          )
      )
      #f
  )
)

(define (move-pair start-slot card-list end-slot destination)
  (remove-card end-slot)
  (if (member destination foundations)
      (begin
        (add-card! destination (make-visible 
                                 (make-card jack (get-suit (car card-list))))
                               )
        (set! open-foundation (+ open-foundation 1))
      )
      (begin
        (add-card! destination (make-visible
                                 (make-card king (get-suit (car card-list))))
                               )
        (add-card! destination (make-visible
                                 (make-card queen (get-suit (car card-list))))
                               )
        (set! open-royal (+ open-royal 1))
      )
  )
  (add-to-score! 2)
  (fill-gaps reserves)
)  

(define (fill-gaps slot-list)
  (if (or (and (empty-slot? waste) (empty-slot? stock)) (null? slot-list))
    #t
    (begin
      (if (empty-slot? (car slot-list))
          (if (empty-slot? waste)
              (deal-cards-face-up stock (list (car slot-list)))
              (deal-cards-face-up waste (list (car slot-list)))
          )          
      )
      (fill-gaps (cdr slot-list))
    )
  )
)   


(define (button-clicked slot-id)
  (if (= slot-id stock)
      (flip-stock stock waste 0)
      #f
  )
)

(define (button-double-clicked slot-id)
  (if (member slot-id (append (list waste) reserves))
    (let ((move (check-moves-helper slot-id 
                   (append (list waste) reserves foundations)
                )
         ))
      (if move
        (begin
          (button-released slot-id (list (remove-card slot-id)) (cadr move))
          (fill-gaps reserves)
        )
        #f
      )
    )
    #f
  )
)

(define (game-continuable)
  (give-status)
  (and (get-hint)
       (not (game-won))
  )
)

(define (game-won)
  (= 52 (get-score))
)

(define (get-hint)
  (define move (or (check-moves (append (list waste) reserves) foundations)
                   (check-moves (append (list waste) reserves) reserves))
  )
  (if move
      (hint-move (car move) 1 (cadr move))
      (and (not (empty-slot? stock)) (list 0 (G_"Deal a new card")))
  )
)

(define (check-moves from-list to-list)
  (if (not (null? from-list))
    (begin
      (or (check-moves-helper (car from-list) to-list)
          (check-moves (cdr from-list) (delete (car from-list) to-list))
      )
    )
    #f
  )
)

(define (check-moves-helper item to-list)
  (if (not (null? to-list))
    (begin
      (if (droppable? item (list (get-top-card item)) (car to-list))
        (list item (car to-list))
        (check-moves-helper item (cdr to-list))
      )
    )
    #f
  )
)

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout droppable?)
