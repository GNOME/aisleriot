; AisleRiot - eliminator.scm
; Copyright (C) 2010 Sapphire Becker (logicplace.com)
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

(define foundation '(0 1 2 3 4 5))
(define found-amt 6)
(define tableau '(6 7 8 9))

; Suggestion by Vincent Povirk
(define (any lst)
	(let ((lcdr (cdr lst)) (lcar (car lst)))
		(if (or (null? lcdr) lcar)
			lcar
			(any lcdr)
		)
	)
)
(define (all lst)
	(let ((lcdr (cdr lst)) (lcar (car lst)))
		(if (or (null? lcdr) (not lcar))
			lcar
			(all lcdr)
		)
	)
)

(define (new-game)
	(initialize-playing-area)
	(set-ace-low)
	(make-standard-deck)
	(shuffle-deck)
	
	; Add foundation slots
	(add-normal-slot '())
	(add-normal-slot '())
	(add-normal-slot '())
	(add-normal-slot '())
	(if (>= found-amt 5) (add-normal-slot '()))
	(if (= found-amt 6) (add-normal-slot '()))
	(set! foundation (iota found-amt))
	
	(add-carriage-return-slot)
	(if (= found-amt 6) (add-blank-slot))
	
	; Add tableau
	(add-extended-slot '() down)
	(add-extended-slot '() down)
	(add-extended-slot '() down)
	(add-extended-slot '() down)
	(set! tableau (map (lambda(n) (+ n found-amt)) (iota 4)))
	
	; Deal cards (13x)
	(deal-cards-face-up-from-deck DECK (apply append (make-list 13 tableau)))
	
	; Remove unrequested foundation
	;(let ((down (make-card joker spade)))
	;	(if (< found-amt 6) (add-card! 0 down))
	;	(if (< found-amt 5) (add-card! 5 down))
	;)
	
	(list found-amt 4)
)

(define (button-pressed slot-id card-list)
	(and (member slot-id tableau) (= (length card-list) 1))
)
(define (button-released start-slot card-list end-slot)
	(if (droppable? start-slot card-list end-slot)
		(begin
			(if (not (empty-slot? end-slot)) (add-to-score! 1))
			(add-card! end-slot (car card-list))
		)
		#f
	)
)
(define (button-double-clicked tid)
	(let ((card (get-top-card tid)) (mv #f))
		(for-each (lambda(fid)
			(if mv #t
				(if (and
					(not (empty-slot? tid))
					(not (empty-slot? fid))
					(droppable? tid (list card) fid)
				)(begin
					(deal-cards tid (list fid))
					(add-to-score! 1)
					(set! mv #t)
				) #f )
			)
		)foundation)
		mv
		;(if mv
		;	#t 
		;	(begin ; Don't spam!
		;		(add-to-score! -1)
		;		#f
		;	)
		;)
	)
)

(define (find-possible-move)
	(let ((fnd #f))
		(let ((tmp (any (apply append (map (lambda(tid)
			(map (lambda(fid)
				(and (not (empty-slot? tid)) (eq? fnd #f)
					(if (droppable? tid (list (get-top-card tid)) fid)
						(begin (if (not (empty-slot? fid)) (set! fnd (list tid fid))) #t)
						#f
					)
				)
			)foundation)
		)tableau)))))
			(if fnd fnd tmp)
		)
	)
)

(define (game-continuable)
	(and
		; Has the game been won?
		(not (game-won))
		(or
			; If there's still an empty slot, you can play
			(any (map empty-slot? foundation))
			; Otherwise check all cards
			(list? (find-possible-move))
		)
	)
)

(define (game-won) ; If the tableau is empty you win
	(all (map empty-slot? tableau))
)

(define (droppable? start-slot card-list end-slot)
	(if (member end-slot foundation)
		(let ((top-card (get-top-card end-slot)))
			(or (empty-slot? end-slot)
				(and (is-visible? top-card)
					(let ((top (get-value top-card)) (card (get-value (car card-list))))
						(or 
							(= card (+ top 1)) ; Card is higher
							(= card (- top 1)) ; Card is lower
							(or (equal? (list card top) '(13 1)) ; Card is king over ace
								(equal? (list card top) '(1 13)) ; Card is ace over king
							)
						)
					)
				)
			)
		)
		#f
	)
)

; Options
(define (get-options)
	(list 'begin-exclusive 
		(list (G_"Six Foundations")  (= found-amt 6))
		(list (G_"Five Foundations") (= found-amt 5))
		(list (G_"Four Foundations") (= found-amt 4))
	'end-exclusive)
)
(define (apply-options options)
	(set! found-amt (cond
		((cadr (list-ref options 1)) 6)
		((cadr (list-ref options 2)) 5)
		((cadr (list-ref options 3)) 4)
		(#t found-amt)
	))
)

; Hint
(define (get-hint)
	(let ((x (find-possible-move)))
		(if (list? x)
			(hint-move (car x) 1 (cadr x))
			(if x
				(list 0 (G_"Play a card to foundation."))
				(list 0 (G_"No moves."))
			)
		)
	)
)

; Ignore
(define (do-deal-next-cards) #f)
(define (timeout) #f)
(define (button-clicked slot-id) #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout droppable?)
