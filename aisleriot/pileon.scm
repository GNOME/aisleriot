; Aisleriot - pileon.scm
; Copyright (C) 1998 Nick Lamb <njl195@zepler.org.uk>

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (add-carriage-return-slot)

  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (add-carriage-return-slot)

  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (add-carriage-return-slot)

  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (add-carriage-return-slot)

  (deal-cards-face-up-from-deck DECK
   '(0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 11 11 11 11 12 12 12 12))

  (list 7 4))

(define (check-same-value-list card-list)
  (if (< (length card-list) 2)
      #t
      (if (= (get-value (car card-list)) (get-value (cadr card-list)))
          (check-same-value-list (cdr card-list))
          #f)))

(define (freeze-slot slot-id)
   (flip-top-card slot-id)
   (add-to-score! 4))

(define (button-pressed slot-id card-list)
   (and (check-same-value-list card-list)
        (is-visible? (car card-list))))

(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (if (and (= (length (get-cards end-slot)) 4)
           (check-same-value-list (get-cards end-slot)))
                (freeze-slot end-slot)) #t)

(define (button-released start-slot card-list end-slot)
  (and (or (empty-slot? end-slot)
           (eq? (get-value (car (get-cards end-slot)))
                (get-value (car card-list))))
       (< (+ (length (get-cards end-slot)) (length card-list)) 5)
       (complete-transaction start-slot card-list end-slot)))

(define (button-clicked slot-id) #f)

(define (button-double-clicked slot) #f)

(define (game-over) (not (game-won)))

(define (done-or-empty slot-id)
  (or (empty-slot? slot-id)
      (not (is-visible? (car (get-cards slot-id))))))

(define (game-won) 
  (and (done-or-empty 0)
       (done-or-empty 1)
       (done-or-empty 2)
       (done-or-empty 3)
       (done-or-empty 4)
       (done-or-empty 5)
       (done-or-empty 6)
       (done-or-empty 7)
       (done-or-empty 8)
       (done-or-empty 9)
       (done-or-empty 10)
       (done-or-empty 11)
       (done-or-empty 12)
       (done-or-empty 13)
       (done-or-empty 14))
)

(define (get-hint)  #f)

(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout)
