;; AisleRiot - wall.scm
;; Copyright (C) 2015 Otto Wallenius <owalleni@gmail.com>, Markus Tuhkanen
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(use-modules (aisleriot interface) (aisleriot api))

(define throne 0)
(define high-guard-post '(1 2 3))
(define low-guard-post '(4 5 6 7))
(define first-low-guard-slot-id 4)
(define first-wall-slot-id 8)
(define last-wall-slot-id 52)
(define wall '(8 9 10 11 12 13 14 15 16 17 18
                 19 20 21 22 23 24 25
                 26 27 28 29 30 31 32 33
                 34 35 36 37 38 39 40
                 41 42 43 44 45 46 47 48
                 49 50 51 52))
(define first-target-slot-id 0)
(define stock 53)
(define waste '(54 55 56))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-double-deck)
  
  ; remove kings of spades from DECK
  (set! DECK (filter 
              (lambda (card) (not (and
                                   (= (get-value card) king)
                                   (= (get-suit card) spade))))
              DECK))
  (shuffle-deck)
  
  ; throne
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-partially-extended-slot '() right 2)
  
  ; high-guard-post
  (add-carriage-return-slot)
  (add-blank-slot)
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-partially-extended-slot '() right 2)
  (add-blank-slot)
  (add-partially-extended-slot '() right 2)
  (add-blank-slot)
  (add-partially-extended-slot '() right 2)
  
  ; low-guard-post
  (add-carriage-return-slot)
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-partially-extended-slot '() right 3)
  (add-blank-slot)
  (add-partially-extended-slot '() right 3)
  (add-blank-slot)
  (add-partially-extended-slot '() right 3)
  (add-blank-slot)
  (add-partially-extended-slot '() right 3)
  (add-blank-slot)
  
  ; wall
  (add-carriage-return-slot)
  (add-empty-normal-slots 8)
  
  (add-carriage-return-slot)
  (set! VERTPOS (- VERTPOS 0.5))
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-empty-normal-slots 7)
  
  (add-carriage-return-slot)
  (set! VERTPOS (- VERTPOS 0.5))
  (add-empty-normal-slots 8)
  
  (add-carriage-return-slot)
  (set! VERTPOS (- VERTPOS 0.5))
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-empty-normal-slots 7)
  
  (add-carriage-return-slot)
  (set! VERTPOS (- VERTPOS 0.5))
  (add-empty-normal-slots 8)
  
  (add-carriage-return-slot)
  (set! VERTPOS (- VERTPOS 0.5))
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-empty-normal-slots 7)
  
  ; deck
  (add-carriage-return-slot)
  (set! VERTPOS (+ VERTPOS 0.5))
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot DECK)
  
  ; waste
  (add-partially-extended-slot '() right 3)
  (add-partially-extended-slot '() right 3)
  (add-partially-extended-slot '() right 3)
  
  ; deal cards
  (deal-cards-face-up stock (append high-guard-post high-guard-post))
  (deal-cards stock (append low-guard-post low-guard-post low-guard-post))
  (deal-cards-face-up stock wall)
  (do-deal-next-cards)
  (add-card! throne (make-visible (make-card king spade)))
  (add-card! throne (make-visible (make-card king spade)))
  
  (give-status-message)
  
  (list 8 8)
  )

(define (add-empty-normal-slots n)
  (if (> n 0)
      (begin (add-normal-slot '())
             (add-empty-normal-slots (- n 1)))))

(define (give-status-message)
  (set-statusbar-message (get-attacks-left-string)))

(define (get-attacks-left-string)
  (string-append (G_"Deals left: ")
                 (number->string (/ (length (get-cards stock)) 3))))

(define (in-wall? slot-id)
  (and (>= slot-id first-wall-slot-id)
       (<= slot-id last-wall-slot-id)))

(define (in-left-wall-edge? slot-id)
  (and (in-wall? slot-id)
       (= (modulo (- slot-id first-wall-slot-id) 15) 0)))

(define (in-right-wall-edge? slot-id)
  (and (in-wall? slot-id)
       (= (modulo (- slot-id first-wall-slot-id) 15) 7)))

(define (in-low-guard-post? slot-id)
  (memv slot-id low-guard-post))

(define (in-high-guard-post? slot-id)
  (memv slot-id high-guard-post))

(define (in-waste? slot-id)
  (memv slot-id waste))

(define (in-throne? slot-id)
  (= slot-id throne))

(define (is-target-slot? slot-id)
  (or (in-throne? slot-id)
      (in-high-guard-post? slot-id)
      (in-low-guard-post? slot-id)
      (in-wall? slot-id)))

; If the player attacks a "no-retreat-slot", the attack
; cards are discarded even if the attack fails.
(define (no-retreat? slot-id)
  (and (exposed? slot-id)
       (in-low-guard-post? slot-id)
       (not (empty-slot? slot-id))
       (not (is-visible? (get-top-card slot-id)))))

; Returns #t iff there's a low guard post pile that is both hidden and exposed.
(define (hidden-exposed-low-guard?)
  (or-map (lambda (slot-id)
            (and
             (exposed? slot-id)
             (not (empty-slot? slot-id))
             (not (is-visible? (get-top-card slot-id)))))
          low-guard-post))

(define (droppable? start-slot-id card-list end-slot-id)
  (and (not (= start-slot-id end-slot-id))
       (in-waste? start-slot-id)
       (or
        (in-waste? end-slot-id)
        (and
         (is-target-slot? end-slot-id)
         (or 
          (attackers-would-win? card-list end-slot-id)
          (and (exposed? end-slot-id)
               (no-retreat? end-slot-id)))))))

; Makes visible cards that by the rules should be made
; visible after removing cards in slot slot-id.
(define (make-visible-if-possible slot-id)
  (if (in-low-guard-post? slot-id)
      (if (and
           (not (empty-slot? slot-id))
           (not (is-visible? (get-top-card slot-id))))
          (make-cards-visible slot-id))
      #f))

; Makes all cards visible in slot slot-id.
(define (make-cards-visible slot-id)
  (set-cards! slot-id (map make-visible (get-cards slot-id))))

; Returns #t if attack succeeded, #f otherwise. If attack succeeds
; the cards in target-slot-id are removed.
(define (attack attackers target-slot-id source-slot-id)
  (if (attackers-would-win? attackers target-slot-id)
      (let* ([remove-fixed
              (lambda () (remove-n-cards target-slot-id (length (get-cards target-slot-id))))])
        (add-to-score! (get-hp target-slot-id))
        ; if pile face down, show the cards quickly before discarding them
        (if (and
             (in-low-guard-post? target-slot-id)
             (not (is-visible? (get-top-card target-slot-id))))
            (begin
              (make-visible-if-possible target-slot-id)
              (delayed-call remove-fixed))
            (remove-fixed))
        #t)
      (begin
        (if (no-retreat? target-slot-id)
            (make-visible-if-possible target-slot-id))
        #f)))

; Returns the lowest sum of card values minus one that can remove the cards in slot slot-id.
(define (get-hp slot-id)
  (if (empty-slot? slot-id)
      #f
      (let* ([lpid (get-left-parent-slot-id slot-id)]
             [rpid (get-right-parent-slot-id slot-id)]
             [lrpid (if lpid (get-right-parent-slot-id lpid) #f)]
             [rlpid (if rpid (get-left-parent-slot-id rpid) #f)])
        (cond
          [(in-left-wall-edge? slot-id)
           (card-value-sum
            (append (get-cards slot-id)
                    (if rpid (get-cards rpid) '())
                    (if rlpid (get-cards rlpid) '())))]
          [(in-right-wall-edge? slot-id)
           (card-value-sum
            (append (get-cards slot-id)
                    (if lpid (get-cards lpid) '())
                    (if lrpid (get-cards lrpid) '())))]
          [(in-wall? slot-id)
           (card-value-sum
            (append (get-cards slot-id)
                    (if lpid (get-cards lpid) '())
                    (if rpid (get-cards rpid) '())))]
          [(or (is-target-slot? slot-id))
           (card-value-sum (get-cards slot-id))]
          [else #f]))))

; Returns #t if attackers would win if they attacked target-slot-id, #f otherwise.
(define (attackers-would-win? attackers target-slot-id)
  (and (exposed? target-slot-id)
       (not (null? attackers))
       (not (empty-slot? target-slot-id))
       (< (get-hp target-slot-id) (card-value-sum attackers))))

(define (card-value-sum card-list)
  (if (null? card-list)
      0
      (+ (get-value (car card-list)) (card-value-sum (cdr card-list)))))

(define (get-left-child-slot-id slot-id)
  (if (or (< slot-id first-wall-slot-id)
          (> slot-id (- last-wall-slot-id 7))
          (= (modulo (- slot-id first-wall-slot-id) 15) 0))
      #f
      (+ slot-id 7)))

(define (get-right-child-slot-id slot-id)
  (if (or (< slot-id first-wall-slot-id)
          (> slot-id (- last-wall-slot-id 7))
          (= (modulo (- slot-id first-wall-slot-id) 15) 7))
      #f
      (+ slot-id 8)))

(define (get-left-parent-slot-id slot-id)
  (if (or (< slot-id (+ first-wall-slot-id 8))
          (> slot-id last-wall-slot-id)
          (= (modulo (- slot-id first-wall-slot-id) 15) 0))
      #f
      (- slot-id 8)))

(define (get-right-parent-slot-id slot-id)
  (if (or (< slot-id (+ first-wall-slot-id 8))
          (> slot-id last-wall-slot-id)
          (= (modulo (- slot-id first-wall-slot-id) 15) 7))
      #f
      (- slot-id 7)))

(define (get-left-sibling-slot-id slot-id)
  (if (or (not (in-wall? slot-id))
          (memv (modulo (- slot-id first-wall-slot-id) 15) '(0 8)))
      #f
      (- slot-id 1)))

(define (get-right-sibling-slot-id slot-id)
  (if (or (not (in-wall? slot-id))
          (memv (modulo (- slot-id first-wall-slot-id) 15) '(7 14)))
      #f
      (+ slot-id 1)))

; If a card is exposed, it means that one can attack it.
(define (exposed? slot-id)
  (cond [(in-wall? slot-id)
         (let ([lcid (get-left-child-slot-id slot-id)]
               [rcid (get-right-child-slot-id slot-id)]
               [lsid (get-left-sibling-slot-id slot-id)]
               [rsid (get-right-sibling-slot-id slot-id)])
           (or (and (not lcid) (not rcid)) 
               (and lcid (empty-slot? lcid))
               (and rcid (empty-slot? rcid))
               (and lsid (empty-slot? lsid))
               (and rsid (empty-slot? rsid))))]
        [(in-throne? slot-id) (and-map empty-slot? high-guard-post)]
        [(in-low-guard-post? slot-id)
         (or
          (empty-slot? (+ (* (- slot-id first-low-guard-slot-id) 2) first-wall-slot-id))
          (empty-slot? (+ (* (- slot-id first-low-guard-slot-id) 2) first-wall-slot-id 1))
          (and
           (memv slot-id (cdr low-guard-post))
           (empty-slot? (- slot-id 1)))
          (and (memv slot-id (list-head low-guard-post 3))
               (empty-slot? (+ slot-id 1))))]
        [(in-high-guard-post? slot-id)
         (or (empty-slot? (+ slot-id 3))
             (empty-slot? (+ slot-id 4))
             (empty-slot? (cadr high-guard-post))
             (and (= slot-id (cadr high-guard-post))
                  (or (empty-slot? (car high-guard-post))
                      (empty-slot? (caddr high-guard-post)))))]
        [else #f]))

; Returns the slot id of the first face-up slot that you can attack and win
; with the cards in waste, or #f if there is no such slot.
(define (find-visible-winnable-slot)
  (find-visible-winnable-slot-rec first-target-slot-id))

(define (find-visible-winnable-slot-rec slot-id)
  (if (not (is-target-slot? slot-id))
      #f
      (if (and
           (attackers-would-win? (get-cards-from-slots waste) slot-id)
           (is-visible? (get-top-card slot-id)))
          slot-id
          (find-visible-winnable-slot-rec (+ slot-id 1)))))

(define (do-deal-next-cards)
  (if (not (empty-slot? stock))
      (begin
        (remove-n-cards (car waste) (length (get-cards (car waste))))
        (remove-n-cards (cadr waste) (length (get-cards (cadr waste))))
        (remove-n-cards (caddr waste) (length (get-cards (caddr waste))))
        (deal-cards-face-up stock waste)
        #t)
      #f))

(define (button-clicked slot-id)
  (cond 
    [(= slot-id stock)
     (do-deal-next-cards)]
    [(is-target-slot? slot-id)
     (let ([was-no-retreat? (no-retreat? slot-id)])
       (if (or
            (attack (get-cards-from-slots waste) slot-id waste)
            was-no-retreat?)
           (begin
             (empty-slots! waste)
             (do-deal-next-cards)
             #t)
           #f))]
    [else #f]))

(define (button-double-clicked slot-id)
  (if (is-target-slot? slot-id)
      (begin
        (attack (get-cards-from-slots waste) slot-id waste)
        (empty-slots! waste)
        (do-deal-next-cards)
        #t)
      #f))

(define (button-pressed slot-id card-list)
  (in-waste? slot-id))

(define (button-released start-slot card-list end-slot)
  (cond [(is-target-slot? end-slot)
         ; no-retreat? must be evaluated before attack for correct result.
         (let ([was-no-retreat? (no-retreat? end-slot)])
           (let* ([attack-succeeded? (attack card-list end-slot #f)])
             (if (and-map empty-slot? waste)
                 (do-deal-next-cards))
             (or attack-succeeded?
                 was-no-retreat?)))]
        [(memv end-slot waste)
         (move-n-cards! start-slot end-slot card-list)]
        [else #f]))

(define (empty-slots! slot-list)
  (if (not (null? slot-list))
      (begin
        (remove-n-cards (car slot-list) (length (get-cards (car slot-list))))
        (empty-slots! (cdr slot-list)))))

(define (get-cards-from-slots slot-list)
  (if (null? slot-list)
      '()
      (append
       (get-cards (car slot-list))
       (get-cards-from-slots (cdr slot-list)))))

(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (or (not (empty-slot? stock))
           (find-visible-winnable-slot)
           (and (not (and-map empty-slot? waste))
                (hidden-exposed-low-guard?)))))

(define (game-won)
  (empty-slot? throne))

(define (dealable?)
  (if (not (empty-slot? stock))
      (list 0 (G_"Deal cards."))
      #f))

(define (get-hint)
  (let ([winnable-slot (find-visible-winnable-slot)])
    ; if there's a hidden, exposed low guard post pile
    ; and attacking it is the best choice you have
    (if (and (hidden-exposed-low-guard?)
             (or (not winnable-slot)
                 (in-wall? winnable-slot)))
        (list 0 (G_"Attack a face-down pile."))
        (if winnable-slot
            (hint-remove-top-card winnable-slot)
            (dealable?)))))

(define (get-options) #f)
(define (apply-options options) #f)
(define (timeout) #f)

(set-features droppable-feature dealable-feature)

(set-lambda new-game button-pressed button-released button-clicked
            button-double-clicked game-continuable game-won get-hint get-options
            apply-options timeout droppable? dealable?)
