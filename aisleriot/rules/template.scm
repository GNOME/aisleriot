; AisleRiot -
; Copyright (C)
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
)

(define (button-pressed slot-id card-list)
  #f)

;;; Not essential, see set-features below.
(define (droppable)
  #f)

(define (button-released start-slot card-list end-slot)
  ;; This will often start with somthing like:
  ;;  (if (droppable? start-slot card-list end-slot ...
  #f)

(define (button-clicked slot-id)
  #f)

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

;; Define the optional features the game uses. Valid options are:
;;  droppable-feature: An predicate, droppable?, is defined that will 
;;                     return whether the card can be dropped here. 
;;                     This is used by the drawing code to highlight
;;                     droppable locations.
(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout droppable?)
