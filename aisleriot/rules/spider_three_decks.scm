; AisleRiot - spider_three_decks.scm
; Copyright (C) 2005 Daniel Werner <dw@dur.ch>
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

(load "spider.scm")

(define tableau '(13 14 15 16 17 18 19 20 21 22 23 24))
(define foundation '(1 2 3 4 5 6 7 8 9 10 11 12))
(define initial-deal '(13 14 15 16 17 18 19 20 21 22 23 24 13 14 15 16 17 18 19 20 21 22 23 24 13 14 15 16 17 18 19 20 21 22 23 24 13 14 15 16 17 18 19 20 21 22 23 24 13 14 15 16 17 18))
(define winning-score 144)
(define stock 0)

(define allow-empty-slots #t)

(define (make-standard-triple-deck)
  (if (= ace 14)
     (set! DECK (append (make-standard-deck-list-ace-high 2 club) (make-standard-deck-list-ace-high 2 club) (make-standard-deck-list-ace-high 2 club)))
     (set! DECK (append (make-standard-deck-list-ace-low ace club) (make-standard-deck-list-ace-low ace club) (make-standard-deck-list-ace-low ace club))))
  )

(define (new-game)
  (initialize-playing-area)
  (make-standard-triple-deck)
  (shuffle-deck)

  ;set up the board
  (add-normal-slot DECK)
  (add-normal-slot '())
  (set! HORIZPOS (- HORIZPOS 0.09))
  (add-normal-slot '())
  (set! HORIZPOS (- HORIZPOS 0.09))
  (add-normal-slot '())
  (set! HORIZPOS (- HORIZPOS 0.09))
  (add-normal-slot '())
  (set! HORIZPOS (- HORIZPOS 0.09))
  (add-normal-slot '())
  (set! HORIZPOS (- HORIZPOS 0.09))
  (add-normal-slot '())
  (set! HORIZPOS (- HORIZPOS 0.09))
  (add-normal-slot '())
  (set! HORIZPOS (- HORIZPOS 0.09))
  (add-normal-slot '())
  (set! HORIZPOS (- HORIZPOS 0.09))
  (add-normal-slot '())
  (set! HORIZPOS (- HORIZPOS 0.09))
  (add-normal-slot '())
  (set! HORIZPOS (- HORIZPOS 0.09))
  (add-normal-slot '())
  (set! HORIZPOS (- HORIZPOS 0.09))
  (add-normal-slot '())
  (add-carriage-return-slot)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (deal-initial-setup)

  (give-status-message)

  (list 12 6))

(define (get-options) #f)

(define (apply-options options) #f)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable?)
