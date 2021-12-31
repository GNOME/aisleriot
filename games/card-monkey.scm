#! /usr/bin/guile -s
!#
; Usage: guile -s card-monkey.scm [game.scm] [number of moves] [timeout in seconds] [-v] [-d]
; example: card-monkey.scm klondike.scm 100 60
; -v - Display the game state after each move.
; -d - Deterministic mode - do not seed the PRNG.

(define-module (aisleriot interface))

(add-to-load-path (dirname (current-filename)))

(debug-enable 'backtrace)
(define _verbose #f)

(define status-log '())

(define (log-status str)
    (set! status-log (cons str status-log)))

(define status-info "unknown")

(define-public (set-status-info! status)
    (set! status-info status))

(define (assert x str)
    (if x
        #t
        (begin (display status-log) (newline)
               (display status-info) (newline)
               (display str) (newline)
               (display "current state:") (newline) (_dump-state)
               (_revert-game-state) (display "previous state:") (newline) (_dump-state)
               (error str))))


(define _droppable-feature 1)
(define _scores-disabled 2)
(define _dealable-feature 4)

(define _features 0)

(define-public (set-feature-word! x)
    (set! _features x)
    (assert (integer? x) "non-integer passed to set-feature-word!"))

(define-public (get-feature-word)
    _features)

(define-public (aisleriot-random n)
    (random n))


(define _new-game 'undefined)
(define _button-pressed 'undefined)
(define _button-released 'undefined)
(define _button-clicked 'undefined)
(define _button-double-clicked 'undefined)
(define _game-continuable 'undefined)
(define _game-won 'undefined)
(define _get-hint 'undefined)
(define _get-options 'undefined)
(define _apply-options 'undefined)
(define _timeout 'undefined)
(define (_droppable? start-slot card-list end-slot) #f)
(define (_dealable?) _dealable-sensitivity)
(define _do-deal-next-cards 'undefined)

(define-public (set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-continuable game-won get-hint get-options apply-options timeout . extra-args)
    (set! _new-game new-game)
    (set! _button-pressed button-pressed)
    (set! _button-released button-released)
    (set! _button-clicked button-clicked)
    (set! _button-double-clicked button-double-clicked)
    (set! _game-continuable game-continuable)
    (set! _game-won game-won)
    (set! _get-hint get-hint)
    (set! _get-options get-options)
    (set! _apply-options apply-options)
    (set! _timeout timeout)
    (if (= _droppable-feature (logand _features _droppable-feature))
        (begin
            (set! _droppable? (car extra-args))
            (set! extra-args (cdr extra-args))))
    (if (= _dealable-feature (logand _features _dealable-feature))
        (begin
            (set! _dealable? (car extra-args))
            (set! _do-deal-next-cards (cadr extra-args))
            (set! extra-args (cddr extra-args))))
    (assert (null? extra-args) "too many arguments to set-lambda"))


(define-public (set-lambda! symbol value)
    (case symbol
        ((new-game) (set! _new-game value))
        ((button-pressed) (set! _button-pressed value))
        ((button-released) (set! _button-released value))
        ((button-clicked) (set! _button-clicked value))
        ((button-double-clicked) (set! _button-double-clicked value))
        ((game-over) (set! _game-continuable value))
        ((winning-game) (set! _game-won value))
        ((hint) (set! _get-hint value))
        ((get-options) (set! _get-options value))
        ((apply-options) (set! _apply-options value))
        ((timeout) (set! _timeout value))
        ((droppable) (set! _droppable? value))
        ((dealable) (set! _dealable? value))
        (else (assert #f "unknown symbol name passed to set-lambda!"))))


(define _slots 'undefined)

(define-public (reset-surface)
    (set! _slots (make-vector 0))
    (set! _score 0))

(define (_get-placement-expansion-level x)
    (let ((placement (caddr x)))
         (case (car placement)
               ((normal) 2)
               ((expanded-right expanded) 0)
               ((partially-expanded-right partially-expanded) (+ 1 (caddr placement))))))

(define-public (add-slot x)
    ;(assert (not game-started))
    (set! _slots (list->vector (append (vector->list _slots)
                                       (list (list (_get-placement-expansion-level x) (car x) (cadr x) 0)))))
    '())

(define-public (get-slot x)
    (cdr (vector-ref _slots x)))

(define (_get-expansion x)
    (car (vector-ref _slots x)))

(define-public (set-cards-c! id cards)
    (set-car! (cdr (get-slot id)) cards)
    #t)

(define-public (set-slot-y-expansion! slot expansion)
    '())


(define-public (G_ x)
    (assert (string? x) "G_ called on a non-string")
    x)


(define-public (set-statusbar-message x)
    (assert (string? x) "set-statusbar-message called on a non-string"))


(define _score 0)

(define-public (update-score x)
    (set! _score (string->number x))
    x)


(define-public (delayed-call x)
    ; FIXME
    (x)
    #t)


(define (_is-valid-hint? hint)
    (case (car hint)
        ((0) (string? (cadr hint)))
        ((1) (and (string? (cadr hint))
                (string? (caddr hint))))
        ((2) (string? (cadr hint)))
        ((4) (string? (cadr hint)))
        (else #f)))


(define _dealable-sensitivity #f)
(define-public (dealable-set-sensitive x)
    (assert (boolean? x) "dealable-set-sensitive called with a non-boolean")
    (set! _dealable-sensitivity x))


(define-public (click-to-move?) #t)


(define (_set-exclusive-list options n)
    (if (eq? (car options) 'end-exclusive)
        (cons
            'end-exclusive
            (_randomize-option-list (cdr options)))
        (cons
            (list (caar options) (= 0 n))
            (_set-exclusive-list (cdr options) (- n 1)))))

(define (_count-exclusive-list options n)
    (if (eq? (car options) 'end-exclusive)
        n
        (_count-exclusive-list (cdr options) (+ n 1))))

(define (_randomize-option-list options)
    (if (or (null? options) (not options))
        options
        (if (eq? 'begin-exclusive (car options))
            (cons
                'begin-exclusive
                (_set-exclusive-list (cdr options) (random (_count-exclusive-list (cdr options) 0))))
            (cons
                (list (caar options) (= 1 (random 2)))
                (_randomize-option-list (cdr options))))))

(define (_randomize-options)
    (_apply-options (_randomize-option-list (_get-options))))


(define _state 'undefined)
(define _previous-state 'undefined)
(define (_get-game-state)
    (record-move -1 '())
    (let ((state MOVE))
         (discard-move)
         state))
(define (_store-game-state)
    (set! _state (_get-game-state)))
(define (_changed-game-state?)
    (not (equal? _state (_get-game-state))))
(define (_revert-game-state)
    (eval-move _state)
    (_store-game-state))




(define _score-increasing-moves '())
(define _old-score 0)

(define (_do-drag-and-drop start-slot num-cards end-slot)
    (let ((cards (_get-first-n-cards (get-cards start-slot) num-cards)))
            (remove-n-cards start-slot num-cards)
            (button-released start-slot cards end-slot)))

(define skip-drop-fail-checks #f)

(define skip-obscure-drop-check #f)

(define (_list-drops-from-slot start-slot num-cards end-slot acc)
    (set-status-info! (list "dropping" num-cards "cards from slot" start-slot "to slot" end-slot))
    (if (= end-slot SLOTS)
        acc
        (let ((cards (_get-first-n-cards (get-cards start-slot) num-cards)))
             (remove-n-cards start-slot num-cards)
             (if (_droppable? start-slot cards end-slot)
                 (begin
                     (assert (or skip-obscure-drop-check (empty-slot? end-slot) (is-visible? (get-top-card end-slot))) "dropping onto a slot containing an invisible card")
                     (assert (_button-released start-slot cards end-slot) "droppable? returned true but button-released returned false")
                     (assert (_changed-game-state?) "droppable? and button-released returned true but game state didn't change")
                     (and (> _score _old-score)
                          (set! _score-increasing-moves (cons (list _do-drag-and-drop start-slot num-cards end-slot) _score-increasing-moves)))
                     (_revert-game-state)
                     (assert (is-visible? (list-ref cards (- num-cards 1))) "dragging an invisible card")
                     (_list-drops-from-slot start-slot num-cards (+ 1 end-slot)
                         (cons (list _do-drag-and-drop start-slot num-cards end-slot) acc)))
                 (or (and skip-drop-fail-checks (add-cards! start-slot cards) (_list-drops-from-slot start-slot num-cards (+ 1 end-slot) acc))
                     (begin
                         (assert (not (_button-released start-slot cards end-slot)) "droppable? returned false but button-released returned true")
                             (add-cards! start-slot cards)
                             (assert (not (_changed-game-state?)) "droppable? and button-released returned false but changed the game state")
                             (_revert-game-state)
                             (_list-drops-from-slot start-slot num-cards (+ 1 end-slot) acc)))))))

(define (_get-first-n-cards cards n)
    (if (= n 0)
        '()
        (cons (car cards)
              (_get-first-n-cards (cdr cards) (- n 1)))))

(define (_list-drags-from-slot slot-id num-cards acc)
    (set-status-info! (list "dragging" num-cards "cards from slot" slot-id))
    (if (and (not (= num-cards (_get-expansion slot-id)))
             (<= num-cards (length (get-cards slot-id))))
        (if (_button-pressed slot-id (_get-first-n-cards (get-cards slot-id) num-cards))
            (_list-drags-from-slot slot-id (+ 1 num-cards)
                (_list-drops-from-slot slot-id num-cards 0 acc))
            acc)
        acc))

(define (_list-drags slot-id acc)
    (set-status-info! (list "dragging from slot" slot-id))
    (if (= slot-id SLOTS)
        acc
        (_list-drags (+ 1 slot-id)
            (_list-drags-from-slot slot-id 1 acc))))

(define (_list-deal)
    (set-status-info! "dealing!")
    (if (and (= (logand _features dealable-feature) dealable-feature)
             (_dealable?))
        (begin
            (_do-deal-next-cards)
            (assert (_changed-game-state?) "do-deal-next-cards didn't change game state")
            (and (> _score _old-score)
                 (set! _score-increasing-moves (cons (list _do-deal-next-cards) _score-increasing-moves)))
            (_revert-game-state)
            (list (list _do-deal-next-cards)))
        '()))

(define (_list-clicks slot-id acc-list)
    (set-status-info! (list "single clicking slot" slot-id))
    (if (= slot-id SLOTS)
        acc-list
        (if (_button-clicked slot-id)
            (begin
                (assert (_changed-game-state?) "button-clicked returned true but didn't change game state")
                (and (> _score _old-score)
                     (set! _score-increasing-moves (cons (list _button-clicked slot-id) _score-increasing-moves)))
                (_revert-game-state)
                (_list-clicks (+ slot-id 1) (cons (list _button-clicked slot-id) acc-list)))
            (begin
                (assert (not (_changed-game-state?)) "button-clicked returned false but changed game state")
                (if (_button-double-clicked slot-id)
                    (begin
                        (assert (_changed-game-state?) "button-double-clicked returned true but didn't change game state")
                        (and (> _score _old-score)
                             (set! _score-increasing-moves (cons (list _button-double-clicked slot-id) _score-increasing-moves)))
                        (_revert-game-state)
                        (_list-clicks (+ slot-id 1) (cons (list _button-double-clicked slot-id) acc-list)))
                    (begin
                        (assert (not (_changed-game-state?)) "button-double-clicked returned false but changed game state")
                        (_list-clicks (+ slot-id 1) acc-list)))))))

(define (_list-possible-moves)
    (_store-game-state)
    (set! _old-score _score)
    (set! _score-increasing-moves '())
    (_list-drags 0
     (_list-clicks 0 
      (_list-deal))))

(define (_get-rank-str rank)
    (cond ((= rank ace) "A")
          ((< rank 11) rank)
          ((= rank jack) "J")
          ((= rank queen) "Q")
          ((= rank king) "K")))

(define (_get-suit-str suit)
    (cond ((= suit club) "C")
          ((= suit diamond) "D")
          ((= suit heart) "H")
          ((= suit spade) "S")))

(define (_dump-cards cards)
    (if (null? cards)
        #f
        (begin
            (display " ")
            (if (is-visible? (car cards))
                (begin
                    (display (_get-rank-str (get-value (car cards))))
                    (display (_get-suit-str (get-suit (car cards)))))
                (display "##"))
            (_dump-cards (cdr cards)))))

(define (_get-drawn-cards cards n acc)
    (if (or (null? cards) (= n 1))
        acc
        (_get-drawn-cards (cdr cards) (- n 1) (cons (car cards) acc))))

(define (_dump-slot-state slot-id)
    (if (= slot-id SLOTS)
        #t
        (begin
            (display "slot ") (display slot-id) (display ":")
            (_dump-cards (_get-drawn-cards (get-cards slot-id) (_get-expansion slot-id) '()))
            (newline)
            (_dump-slot-state (+ slot-id 1)))))

(define (_dump-state)
    (begin
        (display "score: ") (display _score) (newline)
        (_dump-slot-state 0)))

(define (_do-one-of functions)
    (let ((f (list-ref functions (random (length functions)))))
         (set-status-info! f)
         (and _verbose
              (display f) (newline))
         (assert (apply (car f) (cdr f)) "move was valid when checking but isn't valid anymore; is undo/redo missing some state?")
         (and _verbose (_dump-state))))

(define (_do-a-move possible-moves)
    (if (or (null? _score-increasing-moves)
            (= (random 3) 0))
        (begin
            ;(assert (not (null? possible-moves)) "no more moves, but game is continuable")
            (if (null? possible-moves)
                (begin
                    (_start-game))
                (_do-one-of possible-moves)))
        (_do-one-of _score-increasing-moves)))

(define (_start-game)
    (and _verbose
         (display "starting new game\n"))
    (_randomize-options)
    (_new-game)
    (start-game)
    (and _verbose (_dump-state)))

(define (test-move)
    (cond ((not (_game-continuable))
           ;(assert (or (_game-won) (null? (_list-possible-moves))) "game lost but moves remain")
           (assert (or (= _scores-disabled (logand _features _scores-disabled))
                       (not (_game-won))
                       (not (= _score 0)))
                   "game won with a score of 0")
           (_start-game))
          ((not (assert (_is-valid-hint? (_get-hint)) "game has no valid hint but is continuable")) #f)
          (#t
           (_do-a-move (_list-possible-moves))
           (and (= 0 (random 10))
                ; check for infinite loop type things
                (let ((current-state (_get-game-state)))
                     (if (equal? _previous-state current-state)
                         (_start-game)
                         (set! _previous-state current-state)))))))

(define (test-n-moves n t g)
    (if (or (= n 0)
            (and (> (current-time) t)
                 (begin
                     (display g)
                     (display " timed out with ")
                     (display n)
                     (display " moves remaining\n")
                     #t)))
        #t
        (begin 
            (test-move)
            (test-n-moves (- n 1) t g))))

(define (test-file args)
    (if (string=? (cadr args) "rules/template.scm")
        (quit))
    (if (string=? (cadr args) "rules/clock.scm")
        (set! skip-obscure-drop-check #t))
    (log-status (list "testing" args "\n"))
    (if (member "-v" args)
        (set! _verbose #t))
    (if (not (member "-d" args))
        (let ((time (gettimeofday)))
          (set! *random-state*
                (seed->random-state (+ (car time)
                                       (cdr time))))))
    (load (cadr args))
    (_start-game)
    ;(set! skip-drop-fail-checks (> SLOTS 40))
    (test-n-moves (string->number (caddr args)) (+ (current-time) (string->number (cadddr args))) (cadr args))
    (quit))

(load "api.scm")

(test-file (command-line))

