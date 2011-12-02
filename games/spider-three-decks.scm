; AisleRiot - spider_three_decks.scm
; Copyright (C) 2005 Daniel Werner <dw@dur.ch>
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

(primitive-load-path "spider")

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

(set-lambda! 'new-game new-game)
(set-lambda! 'get-options get-options)
(set-lambda! 'apply-options apply-options)
