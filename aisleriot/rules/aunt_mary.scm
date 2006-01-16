;; AisleRiot - aunt_mary.scm  -*-scheme-*-
;; Copyright (C) Alan Horkan, 2005.
;; Aunt Mary is a very difficult Klondike variation.  
;; 
;; Goodsol gives odds as < 1%.  It is possible to win in theory.  
;; In practice I've only ever won when the deck wasn't shuffled

(load "klondike.scm") 

(define deal-three #f)

(define tableau '(6 7 8 9 10 11))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)

  (make-standard-deck)
  (shuffle-deck)

  ;; Stock 
  (add-normal-slot DECK)
  (if deal-three
      (add-partially-extended-slot '() right 3)
      (add-normal-slot '()))

  ;; Foundation (4)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)
  ;; Tableau (6)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

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

  (list 7 3) 
)

;; One attempt.  No redeals
(define (get-redeals-string)
  (format (_"Redeals left: ~a") (number->string (- 0 FLIP-COUNTER))))

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable?)
