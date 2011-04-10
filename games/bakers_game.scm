; AisleRiot - bakers_game.scm
; Copyright (C) 2008 Vincent Povirk <madewokherd@gmail.com>
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

(load "freecell.scm")

(define (field-join? lower upper)
  (and (eq? (get-suit lower) (get-suit upper))
       (eq? (+ (get-value lower) 1) (get-value upper))))

(define (max-auto-red)
  13
)

(define (max-auto-black)
  13
)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable?)
