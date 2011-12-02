; AisleRiot - gold_mine.scm
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

(define deal-one #f)
(define deal-three #t)
(define kings-only #f)

(define max-redeal 0)

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

  (give-status-message)

  (list 7 3.1)
)

(define (get-options)
  '())

(define (apply-options options) #f)

(set-lambda! 'new-game new-game)
(set-lambda! 'get-options get-options)
(set-lambda! 'apply-options apply-options)
