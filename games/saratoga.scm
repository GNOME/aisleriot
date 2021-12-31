; AisleRiot - saratoga.scm
; Copyright (C) Alan Horkan, 2005.  
; [NB ask Gnome Foudnation about Copyright Assignment.  
; If I die I want my code to be as free as possible but maybe not Public Domain]
; Friends passed. ...

; saratoga is a face up variation of Klondike
; 3 card deal, unlimited redeals
 
; AisleRiot - klondike.scm
; Copyright (C) 1998, 2003 Jonathan Blandford <jrb@mit.edu>
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

(primitive-load-path "klondike")

(define deal-one #t)
(define deal-three #f)
(define no-redeal #f)

(define max-redeal -1)

(define deal-three #f)

; The set up:

(define tableau '(6 7 8 9 10 11 12))
(define foundation '(2 3 4 5))
(define stock 0)
(define waste 1)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)

  (make-standard-deck)
  (shuffle-deck)
  
  (add-normal-slot DECK 'stock)

  (if deal-three
      (add-partially-extended-slot '() right 3 'waste)
      (add-normal-slot '() 'waste))

  (add-blank-slot)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)

  (deal-cards-face-up stock '(6 7 8 9 10 11 12 7 8 9 10 11 12 8 9 10 11 12 9 10 11 12 10 11 12 11 12 12))
  
  (give-status-message)

  (list 7 3)
)

(define (get-options)
  (list (list (G_"Three card deals") deal-three)))

(define (apply-options options)
  (set! deal-three (cadar options)))

(set-lambda! 'new-game new-game)
(set-lambda! 'get-options get-options)
(set-lambda! 'apply-options apply-options)
