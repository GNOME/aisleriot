; Aisleriot - helper.scm
; Copyright (C) 1998 Jonathan Blandford <jrb@mit.edu>
;
; This game is free software; you can redistribute it and/or
; modify it under the terms of the GNU Library General Public
; License as published by the Free Software Foundation; either
; version 2 of the License, or (at your option) any later version.
;
; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Library General Public License for more details.
;
; You should have received a copy of the GNU Library General Public
; License along with this library; if not, write to the Free
; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(define list-length
  (lambda (obj)
	 (call-with-current-continuation
	  (lambda (return)
		 (letrec ((r (lambda (obj) (cond ((null? obj) 0)
													((pair? obj) (+ (r (cdr obj)) 1))
													(else (return #f))))))
			(r obj))))))


(define (check-same-suit-list card-list)
  (if (< (list-length card-list) 2)
      #t
      (if (= (get-suit (car card-list)) (get-suit (cadr card-list)))
	  (check-same-suit-list (cdr card-list))
	  #f)))

(define (check-straight-descending-list card-list)
  (if (< (list-length card-list) 2)
      #t
      (if (= (get-value (car card-list)) (- (get-value (cadr card-list)) 1))
	  (check-straight-descending-list (cdr card-list))
	  #f)))

