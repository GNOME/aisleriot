; AisleRiot - scorpion.scm
; Copyright (C) 1999 Rosanna Yuen <rwsy@mit.edu>
;
; This game is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2, or (at your option)
; any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
; USA

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK)

  (add-blank-slot)

  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (deal-cards 0 '(1 2 3 4))
  (deal-cards-face-up 0 '(5 6 7))
  (deal-cards 0 '(1 2 3 4))
  (deal-cards-face-up 0 '(5 6 7))
  (deal-cards 0 '(1 2 3 4))
  (deal-cards-face-up 0 '(5 6 7))
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7))
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7))
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7))
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7))

  (list 9 4))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (is-visible? (car (reverse card-list)))))

(define (button-released start-slot card-list end-slot)
  (and (or (and (empty-slot? end-slot)
		(= (get-value (car (reverse card-list))) king))
	   (and (not (empty-slot? end-slot))
		(eq? (get-suit (get-top-card end-slot))
		     (get-suit (car (reverse card-list))))
		(= (get-value (get-top-card end-slot))
		   (+ 1 (get-value (car (reverse card-list)))))))
       (move-n-cards! start-slot end-slot card-list)
       (or (empty-slot? start-slot)
	   (is-visible? (get-top-card start-slot))
	   (make-visible-top-card start-slot))))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (not (empty-slot? 0))
       (deal-cards-face-up 0 '(1 2 3))))


(define (button-double-clicked slot-id)
  #f)

(define (game-continuable)
  #t)

(define (game-won)
  #f)

(define (get-hint)
  #f)

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout)
