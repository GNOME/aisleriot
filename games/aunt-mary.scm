;; AisleRiot - aunt_mary.scm  -*-scheme-*-
;; Copyright (C) Alan Horkan, 2005.
;; Aunt Mary is a very difficult Klondike variation.  
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (aisleriot interface) (aisleriot api))

(primitive-load-path "klondike") 

(define deal-one #t)
(define deal-three #f)
(define no-redeal #f)

(define max-redeal 2)

(define tableau '(6 7 8 9 10 11))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)

  (make-standard-deck)
  (shuffle-deck)

  ;; Stock 
  (add-normal-slot DECK 'stock)
  ;; Waste
  (if deal-three
      (add-partially-extended-slot '() right 3 'waste)
      (add-normal-slot '() 'waste))

  ;; Foundation (4)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)
  ;; Tableau (6)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)

  ;; Pretty Good Solitaire (goodsol) deals Aunt Mary like this
  (deal-cards stock 
    '(  7 8 9 10 11 
          8 9 10 11  
            9 10 11 
              10 11 
                 11))
  (deal-cards-face-up stock 
    '(6 
      6 7 
      6 7 8 
      6 7 8 9 
      6 7 8 9 10 
      6 7 8 9 10 11))

  (give-status-message)

  (list 6 3)
)

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(set-lambda! 'new-game new-game)
(set-lambda! 'get-options get-options)
(set-lambda! 'apply-options apply-options)
