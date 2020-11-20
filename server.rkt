;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname server) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)

; An BattleShipUniverse (BSU) is one of:
; - Lobby
; - BattleShipGame
; and represents a lobby accepting player signups or an active
; game of battleship

(define-struct lobby [players capacity])
; A Lobby is a (make-lobby [List-of Player] Number)

(define-struct bsgame [active-players losers moves])
; A BattleShipGame (BSGame) is a
; (make-bsgame [List-of Player] [List-of Player] [List-of Move])

(define-struct player [name pieces world])
; A Player is a (make-player String [List-of Piece] IWorld)

; A Piece is a [List-of Posn]

(define-struct move [target pos])
; A Move is a (make-move String Posn)


; FLOW

; SIGNUP FLOW
; client ~~> server
; server sends SignupResponse -> client
; client sends PlacementMessage -> server
; ...
; game is about to begin and server sends clients list of all player names

; GAMEPLAY FLOW
; server sends TurnNotification -> client
; client sends AttackMessage -> server
; server sends EndOfTurnResponse -> ALL clients
; ...


; A SignupBundle is a (make-bundle Lobby SignupMail '())

; A SignupMail is a (make-mail IWorld SignupResponse)
; representing confirmation of signing up for a game with the
; name that is assigned to the signed up player

; A SignupResponse is a String
; denoting the username of the player

; A TurnNotificationBundle is a (make-bundle BSGame (list TurnNotificationMail) '())
; where it tells a single player that it is their turn to make a move

; A TurnNotificationMail is a (make-mail IWorld TurnNotification)
; A TurnNotification is "go"
(define TURN-NOTIFICATION "go")

; A EndOfTurnBundle is a (make-bundle BSGame (append [List-of EndOfTurnMail] (list TurnNotificationMail)) [List-of IWorld])
; where all the mail is to be sent to all active players and
; contains all of the turns that have been made in the game so far
; and the list of IWorlds are the losers to disconnect from. There is also a single mail to the next in line to take their turn.

; A EndOfTurnMail is a (make-mail IWorld EndOfTurnResponse)

; A EndOfTurnResponse is a (list [List-of String] [List-of String] [List-of AttackMessage])
; where for (list _active_ _lost_ _moves_)
; - _active_ are the names of the surviving players with the first in the list being the player
;   whose turn it is
; - _lost_ are the names of the players who have lost already
; - _moves_ are all of the attack moves that have happened in the game, in order with the most
;   recent first.

; A PlayerMessage is one of:
; - (list "place" PlacementMessage)
; - (list "attack" AttackMessage)
; and represents the possible messages a player can send to the server

; A PlacementMessage is a [List-of [List-of PosnMessage]]
; represents the desired layout of a player's pieces in a game
; where each element in the list is a distinct piece.

; An AttackMessage is a (list String PosnMessage)
; and represents the name of the player you are attacking and which
; position on the board your attack is intended to land.

; A PosnMessage is a (list Nat Nat)


;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          
;    ;;;;;;;;                                               ;;;;                           
;    ;;;;;;;;                                                 ;;                           
;    ;;                                                       ;;                           
;    ;;                                                       ;;                           
;    ;;        ;;;   ;;;    ;;;;;    ;; ;; ;;;  ;; ;;;;       ;;         ;;;;      ;;;;;;  
;    ;;;;;;;;   ;;; ;;;    ;;;;;;;;  ;;;;;;;;;; ;;;;;;;;      ;;       ;;   ;;   ;;;;;;;;; 
;    ;;;;;;;;    ;; ;;     ;     ;;  ;;  ;;  ;; ;;;   ;;;     ;;       ;;    ;;  ;;      ; 
;    ;;           ;;;       ;;;;;;;  ;;  ;;  ;; ;;     ;;     ;;      ;;      ;  ;;;;;     
;    ;;           ;;;      ;;;;;;;;  ;;  ;;  ;; ;;     ;;     ;;      ;;;;;;;;;   ;;;;;;;  
;    ;;           ;;;     ;;;    ;;  ;;  ;;  ;; ;;     ;;     ;;      ;;             ;;;;; 
;    ;;          ;;;;;    ;;     ;;  ;;  ;;  ;; ;;     ;;     ;;      ;;                ;; 
;    ;;          ;; ;;    ;;    ;;;  ;;  ;;  ;; ;;;   ;;;     ;;       ;         ;     ;;; 
;    ;;;;;;;;;  ;;   ;;    ;;;;;;;;  ;;  ;;  ;; ;;;;;;;;       ;;      ;;     ;  ;;;;;;;;; 
;    ;;;;;;;;; ;;;   ;;;    ;;;  ;;  ;;  ;;  ;; ;; ;;;;         ;;;;     ;;;;;;   ;;;;;;   
;                                               ;;                                         
;                                               ;;                                         
;                                               ;;                                         
;                                               ;;                                         
;                                                                                          

(define player1-name "iworld1")
(define player2-name "iworld2")

(define piece1-place-message '((0 0) (1 0) (2 0)))
(define piece2-place-message '((5 5)))
(define piece3-place-message '((0 1) (1 1) (2 1)))

(define piece1 (list (make-posn 0 0) (make-posn 1 0) (make-posn 2 0)))
(define piece2 (list (make-posn 5 5)))
(define piece3 (list (make-posn 0 1) (make-posn 1 1) (make-posn 2 1)))

(define player1-placement (list piece1-place-message piece2-place-message))
(define player2-placement (list piece3-place-message))
(define player1-place-message (list "place" player1-placement))
(define player2-place-message (list "place" player2-placement))

(define player1-pieces (list piece1 piece2))
(define player2-pieces (list piece3))

(define player1-unplaced (make-player player1-name '() iworld1))
(define player2-unplaced (make-player player2-name '() iworld2))
(define player1 (make-player player1-name player1-pieces iworld1))
(define player2 (make-player player2-name player2-pieces iworld2))

(define empty-lobby (make-lobby '() 2))
(define one-player-lobby-unplaced (make-lobby (list player1-unplaced) 2))
(define two-player-lobby-unplaced (make-lobby (list player1-unplaced player2-unplaced) 2))

(define two-player-lobby-one-placed (make-lobby (list player1 player2-unplaced) 2))
(define two-player-lobby-both-placed (make-lobby (list player1 player2) 2))

(define player1-signup-mail (make-mail iworld1 player1-name))
(define player2-signup-mail (make-mail iworld2 player2-name))

(define player1-signup-bundle (make-bundle one-player-lobby-unplaced
                                           (list player1-signup-mail)
                                           '()))
(define player2-signup-bundle (make-bundle two-player-lobby-unplaced
                                           (list player2-signup-mail)
                                           '()))

(define bsu-start (make-bsgame (list player1 player2) empty empty))

(define attack-player2-message (list "attack" (list player2-name (list 0 0))))
(define attack-player2-move (make-move player2-name (make-posn 0 0)))
(define post-player2-attack (make-bsgame (list player1 player2)
                                         empty
                                         (list attack-player2-move)))
(define post-player2-turn (make-bsgame (list player2 player1)
                                       empty
                                       (list attack-player2-move)))
(define post-player2-attack-state
  (list (list player2-name player1-name)
        empty
        (list (second attack-player2-message))))
(define post-player2-turn-bundle
  (make-bundle post-player2-turn
               (list (make-mail iworld2 post-player2-attack-state)
                     (make-mail iworld1 post-player2-attack-state)
                     (make-mail iworld2 TURN-NOTIFICATION))
               empty))

;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          
;   ;;     ;;                 ;;                                                           
;   ;;     ;;                 ;;                                                           
;   ;;     ;;                                                                              
;   ;;     ;;                                                                              
;   ;;     ;;  ;;  ;;;      ;;;;     ;;     ;;     ;;;;      ;;  ;;     ;;;;;;      ;;;;   
;   ;;     ;;  ;;;;;;;;       ;;      ;;   ;;    ;;   ;;     ;; ;;;;  ;;;;;;;;;   ;;   ;;  
;   ;;     ;;  ;;;   ;;;      ;;      ;;   ;;    ;;    ;;    ;;;   ;  ;;      ;   ;;    ;; 
;   ;;     ;;  ;;     ;;      ;;      ;;   ;;   ;;      ;    ;;       ;;;;;      ;;      ; 
;   ;;     ;;  ;;     ;;      ;;       ;; ;;    ;;;;;;;;;    ;;        ;;;;;;;   ;;;;;;;;; 
;   ;;     ;;  ;;     ;;      ;;       ;; ;;    ;;           ;;           ;;;;;  ;;        
;   ;;     ;;  ;;     ;;      ;;       ;; ;;    ;;           ;;              ;;  ;;        
;   ;;;   ;;;  ;;     ;;      ;;        ;;;      ;           ;;       ;     ;;;   ;        
;    ;;;;;;;   ;;     ;;      ;;        ;;;      ;;     ;    ;;       ;;;;;;;;;   ;;     ; 
;     ;;;;;    ;;     ;;   ;;;;;;;;     ;;;        ;;;;;;    ;;        ;;;;;;       ;;;;;; 
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          


; run-server : Nat -> 
(define (run-server player-capacity)
  (universe (make-lobby '() player-capacity)
            [on-new handle-connection]
            [on-msg handle-message]
            [port 9000]))


;                                                                    
;                                                                    
;                                                                    
;                                                                    
;                                                                    
;                                                                    
;                                                                    
;                                                                    
;     ;;;;;    ;;  ;;;               ;;  ;;;       ;;;;   ;;       ;;
;    ;;;;;;;   ;;;;;;;;              ;;;;;;;;    ;;   ;;  ;;       ;;
;   ;;;   ;;;  ;;;   ;;;             ;;;   ;;;   ;;    ;;  ;;     ;; 
;   ;;     ;;  ;;     ;;  ;;;;;;;;;; ;;     ;;  ;;      ;  ;;  ;  ;; 
;   ;;     ;;  ;;     ;;  ;;;;;;;;;; ;;     ;;  ;;;;;;;;;  ;; ;;; ;; 
;   ;;     ;;  ;;     ;;             ;;     ;;  ;;         ;; ; ; ;; 
;   ;;     ;;  ;;     ;;             ;;     ;;  ;;          ;;; ;;;  
;   ;;;   ;;;  ;;     ;;             ;;     ;;   ;          ;;; ;;;  
;    ;;;;;;;   ;;     ;;             ;;     ;;   ;;     ;   ;;   ;;  
;     ;;;;;    ;;     ;;             ;;     ;;     ;;;;;;   ;;   ;;  
;                                                                    
;                                                                    
;                                                                    
;                                                                    
;                                                                    


; handle-connection : BSU IWorld -> [U BSU SignupBundle]
; register a player if in lobby mode otherwise reject the connection
(check-expect (handle-connection empty-lobby iworld1)
              player1-signup-bundle)
(check-expect (handle-connection one-player-lobby-unplaced iworld2)
              player2-signup-bundle)
(check-expect (handle-connection bsu-start iworld3) bsu-start)
(define (handle-connection bsu new-world)
  (cond [(lobby? bsu) (register-player-and-maybe-start bsu new-world)]
        [(bsgame? bsu) bsu]))

; register-player-and-maybe-start : Lobby IWorld -> SignupBundle
; register a player and produce the confirmation for that player
(check-expect (register-player-and-maybe-start empty-lobby iworld1) player1-signup-bundle)
(check-expect (register-player-and-maybe-start one-player-lobby-unplaced iworld2)
              player2-signup-bundle)
(define (register-player-and-maybe-start lobby world)
  (make-bundle (add-new-player lobby (iworld-name world) world)
               (list (make-mail world (iworld-name world)))
               '()))

; next-name : Lobby -> String
; produce the next name to be assigned to a joining player
(check-expect (next-name empty-lobby) "player1")
(check-expect (next-name one-player-lobby-unplaced) "player2")
(check-expect (next-name two-player-lobby-unplaced) "player3")
(define (next-name lobby)
  (string-append "player" (number->string (add1 (lobby-num-players lobby)))))

; add-new-player : Lobby String IWorld -> Lobby
(check-expect (add-new-player empty-lobby player1-name iworld1) one-player-lobby-unplaced)
(check-expect (add-new-player one-player-lobby-unplaced player2-name iworld2)
              two-player-lobby-unplaced)
(define (add-new-player lobby name world)
  (make-lobby (snoc (make-player name '() world) (lobby-players lobby))
              (lobby-capacity lobby)))

; lobby-num-players : Lobby -> Number
; the number of players in the lobby
(define (lobby-num-players lobby) (length (lobby-players lobby)))

; snoc : X [List-of X] -> [List-of X]
; append to end of list
(check-expect (snoc 1 '()) '(1))
(check-expect (snoc 2 '(1)) '(1 2))
(define (snoc x l) (append l (list x)))


;                                                                                                                
;                                                                                                                
;                                                                                                                
;                                                                                                                
;                                                                                                                
;                                                                                                                
;                                                                                                                
;                                                                                                                
;     ;;;;;    ;;  ;;;               ;; ;; ;;;     ;;;;      ;;;;;;     ;;;;;;     ;;;;;      ;;;; ;;     ;;;;   
;    ;;;;;;;   ;;;;;;;;              ;;;;;;;;;;  ;;   ;;   ;;;;;;;;;  ;;;;;;;;;   ;;;;;;;;   ;;;;;;;;   ;;   ;;  
;   ;;;   ;;;  ;;;   ;;;             ;;  ;;  ;;  ;;    ;;  ;;      ;  ;;      ;   ;     ;;  ;;;   ;;;   ;;    ;; 
;   ;;     ;;  ;;     ;;  ;;;;;;;;;; ;;  ;;  ;; ;;      ;  ;;;;;      ;;;;;        ;;;;;;;  ;;     ;;  ;;      ; 
;   ;;     ;;  ;;     ;;  ;;;;;;;;;; ;;  ;;  ;; ;;;;;;;;;   ;;;;;;;    ;;;;;;;    ;;;;;;;;  ;;     ;;  ;;;;;;;;; 
;   ;;     ;;  ;;     ;;             ;;  ;;  ;; ;;             ;;;;;      ;;;;;  ;;;    ;;  ;;     ;;  ;;        
;   ;;     ;;  ;;     ;;             ;;  ;;  ;; ;;                ;;         ;;  ;;     ;;  ;;     ;;  ;;        
;   ;;;   ;;;  ;;     ;;             ;;  ;;  ;;  ;         ;     ;;;  ;     ;;;  ;;    ;;;  ;;;   ;;;   ;        
;    ;;;;;;;   ;;     ;;             ;;  ;;  ;;  ;;     ;  ;;;;;;;;;  ;;;;;;;;;   ;;;;;;;;   ;;;;;;;;   ;;     ; 
;     ;;;;;    ;;     ;;             ;;  ;;  ;;    ;;;;;;   ;;;;;;     ;;;;;;      ;;;  ;;    ;;;; ;;     ;;;;;; 
;                                                                                                  ;;            
;                                                                                            ;    ;;;            
;                                                                                            ;;;;;;;             
;                                                                                             ;;;;;              
;                                                                                                                



; handle-message : BSU IWorld PlayerMessage -> [U BSU ResponseBundle]
; handles messages coming from players at any point in the universe
(define (handle-message bsu world message)
  (cond [(lobby? bsu) (handle-message-in-lobby bsu world message)]
        [(bsgame? bsu) (handle-message-in-game bsu world message)]))

; handle-message-in-lobby : Lobby IWorld PlayerMessage -> [U Lobby TurnNotificationBundle]
; handles messages relevant to the lobby and ignores others
(check-expect (handle-message-in-lobby two-player-lobby-unplaced
                                       iworld1
                                       player1-place-message)
              two-player-lobby-one-placed)
(check-expect (handle-message-in-lobby two-player-lobby-unplaced
                                       iworld1
                                       attack-player2-message)
              two-player-lobby-unplaced)
(define (handle-message-in-lobby lobby world message)
  (if (string=? "place" (first message))
      (start-if-ready (place-player-pieces lobby world (placement-msg->pieces (second message))))
      lobby))

; place-player-pieces : Lobby IWorld [List-of Piece] -> Lobby
; let the player place those pieces down
; INVARIANT: there exists a player matching the IWorld in the list
(check-expect (place-player-pieces two-player-lobby-unplaced
                                   iworld1
                                   player1-pieces)
              two-player-lobby-one-placed)
(define (place-player-pieces lobby world pieces)
  (make-lobby
   (map (lambda (p) (if (iworld=? (player-world p) world)
                        (make-player (player-name p) pieces (player-world p))
                        p))
        (lobby-players lobby))
   (lobby-capacity lobby)))

; start-if-ready : Lobby -> [U Lobby TurnNotificationBundle]
; start the battleship game if ready, otherwise keep the lobby
(check-expect (start-if-ready two-player-lobby-one-placed) two-player-lobby-one-placed)
(check-expect (start-if-ready two-player-lobby-both-placed)
              (make-bundle bsu-start (list (make-mail iworld1 TURN-NOTIFICATION)) empty))
(define (start-if-ready lobby)
  (if (ready-to-start? lobby)
      (make-bundle (make-bsgame (lobby-players lobby) empty empty)
                   (list (make-turn-notification (first (lobby-players lobby))))
                   empty)
      lobby))

(define (make-turn-notification player)
  (make-mail (player-world player) TURN-NOTIFICATION))

; ready-to-start? : Lobby -> Boolean
; is the lobby at capacity?
(check-expect (ready-to-start? two-player-lobby-unplaced) #false)
(check-expect (ready-to-start? two-player-lobby-both-placed) #true)
(check-expect (ready-to-start? (make-lobby (list player1 player2-unplaced) 2)) #false)
(define (ready-to-start? lobby)
  (and (= (lobby-num-players lobby) (lobby-capacity lobby))
       (all-placed? (lobby-players lobby))))

; all-placed? : [List-of Player] -> Boolean
(check-expect (all-placed? (list player1-unplaced player2)) #false)
(check-expect (all-placed? (list player1 player2)) #true)
(define (all-placed? players)
  (andmap (lambda (p) (not (empty? (player-pieces p))))
          players))

; placement-msg->pieces : PlacementMessage -> [List-of Piece]
; convert from the message of ship placement to the server-side representation
(check-expect (placement-msg->pieces player1-placement) player1-pieces)
(check-expect (placement-msg->pieces player2-placement) player2-pieces)
(define (placement-msg->pieces placement)
  (map (lambda (piece)
         (map (lambda (p-msg) (make-posn (first p-msg) (second p-msg)))
              piece))
       placement))

; handle-message-in-game : BSGame IWorld PlayerMessage -> [U BSU EndOfTurnBundle]
; handles messages relevant to an in-progress game and ignores others
(check-expect (handle-message-in-game bsu-start iworld1 attack-player2-message)
              post-player2-turn-bundle)
(check-expect (handle-message-in-game bsu-start iworld1 player1-place-message) bsu-start)
(define (handle-message-in-game bsgame world message)
  (if (string=? "attack" (first message))
      (broadcast-turn-and-alert-next-player
       (next-player-up
        (process-attack bsgame world (second message))))
      bsgame)) ; TODO Kick player out and tell the next player it is their turn

; process-attack : BSGame IWorld AttackMessage -> BSGame
; process the attack played by the given attacker
(check-expect (process-attack bsu-start iworld1 (list player2-name (list 0 0)))
              post-player2-attack)
(define (process-attack bsgame attacker attack-message)
  (local [(define move (attack-message->move attack-message))
          (define new-moves (cons move (bsgame-moves bsgame)))
          (define target (find-player-by-name bsgame (move-target move)))
          (define target-dead? (dead? target new-moves))]
    (make-bsgame (if target-dead?
                     (remove-player target (bsgame-active-players bsgame))
                     (bsgame-active-players bsgame))
                 (if target-dead?
                     (cons target (bsgame-losers bsgame))
                     (bsgame-losers bsgame))
                 new-moves)))

; next-player-up : BSGame -> BSGame
; rotate the player lineup
(check-expect (next-player-up bsu-start) (make-bsgame (list player2 player1) empty empty))
(define (next-player-up bsg)
  (make-bsgame (snoc (first (bsgame-active-players bsg))
                     (rest (bsgame-active-players bsg)))
               (bsgame-losers bsg)
               (bsgame-moves bsg)))

; broadcast-turn-and-alert-next-player : BSGame -> EndOfTurnBundle
; broadcast the state of the board to all active players to keep playing!
(define (broadcast-turn-and-alert-next-player bsg)
  (local [(define (make-turn-mail player)
            (make-mail (player-world player) (bsg->turn-response bsg)))
          (define turn-mail (map make-turn-mail (bsgame-active-players bsg)))
          (define notify-player-mail (make-turn-notification (first (bsgame-active-players bsg))))]
    (make-bundle bsg
                 (snoc notify-player-mail turn-mail)
                 (map player-world (bsgame-losers bsg)))))

(define (bsg->turn-response bsg)
  (list (map player-name (bsgame-active-players bsg))
        (map player-name (bsgame-losers bsg))
        (map move->attack-message (bsgame-moves bsg))))

(define (attack-message->move msg)
  (make-move (first msg)
             (posn-message->posn (second msg))))

(define (move->attack-message move)
  (list (move-target move)
        (posn->posn-message (move-pos move))))

(define (posn->posn-message p)
  (list (posn-x p) (posn-y p)))

(define (posn-message->posn msg)
  (apply make-posn msg))

; dead? : Player [List-of Move] -> Boolean
(define (dead? player moves)
  (local [(define (sunk? piece)
            (local [(define (part-hit? pos)
                      (local [(define (move-landed? move)
                                (posn=? (move-pos move) pos))]
                        (ormap move-landed? moves)))]
              (andmap part-hit? piece)))]
    (andmap sunk? (player-pieces player))))

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

; find-player-by-name : BSGame String -> Player
(define (find-player-by-name bsgame name)
  (find (lambda (p) (string=? name (player-name p)))
        (bsgame-active-players bsgame)))

; find : {X} [X -> Boolean] [List-of X] -> X
(check-expect (find even? (list 1 2 3)) 2)
(check-error (find positive? (list -1 -2)))
(define (find found? lox)
  (cond [(empty? lox) (error "Element not in list")]
        [(cons? lox) (if (found? (first lox))
                         (first lox)
                         (find found? (rest lox)))]))

; remove-player : Player [List-of Player] -> [List-of Player]
(check-expect (remove-player player1 empty) empty)
(check-expect (remove-player player1 (list player1 player2)) (list player2))
(define (remove-player p lop)
  (filter (lambda (p2) (not (iworld=? (player-world p) (player-world p2))))
          lop))