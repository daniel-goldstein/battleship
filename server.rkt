;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname battleship-server) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)

; An BattleShipUniverse (BSU) is one of:
; - Lobby
; - BattleShipGame
; and represents a lobby accepting player signups or an active
; game of battleship

; A Lobby is a [List-of Player]
(define lobby? list?)

(define-struct bsgame [active-players losers moves])
; A BattleShipGame (BSGame) is a
; (make-bsgame [List-of Player] [List-of Player] [List-of Move])

(define-struct player [name pieces world])
; A Player is a (make-player String [List-of Piece] IWorld)

; A Piece is a [List-of Posn]

(define-struct move [attacker victim pos])
; A AttackMove is a (make-move String String Posn)

; A SignupBundle is a (make-bundle Lobby SignupMail '())

; A SignupMail is a (make-mail IWorld SignupResponse)
; representing confirmation of signing up for a game with the
; name that is assigned to the signed up player

; A SignupResponse is a String
; denoting the username of the player

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

(define player1-unplaced (make-player "player1" '() iworld1))
(define player2-unplaced (make-player "player2" '() iworld2))
(define player1 (make-player "player1" player1-pieces iworld1))
(define player2 (make-player "player2" player2-pieces iworld2))

(define empty-lobby '())
(define one-player-lobby-unplaced (list player1-unplaced))
(define two-player-lobby-unplaced (list player1-unplaced player2-unplaced))

(define player1-signup-mail (make-mail iworld1 "player1"))
(define player2-signup-mail (make-mail iworld2 "player2"))

(define player1-signup-bundle (make-bundle one-player-lobby-unplaced
                                           (list player1-signup-mail)
                                           '()))
(define player2-signup-bundle (make-bundle two-player-lobby-unplaced
                                           (list player2-signup-mail)
                                           '()))

(define two-player-lobby-one-placed (list player1 player2-unplaced))
(define two-player-lobby-both-placed (list player1 player2))

(define bsu-start (make-bsgame (list player1 player2) empty empty))

(define attack-player2-message (list "attack" (list "player2" (list 0 0))))

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
  (universe '()
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
  (cond [(lobby? bsu) (register-player bsu new-world)]
        [(bsgame? bsu) bsu]))

; register-player : Lobby IWorld -> SignupBundle
; register a player and produce the confirmation for that player
(check-expect (register-player empty-lobby iworld1) player1-signup-bundle)
(check-expect (register-player one-player-lobby-unplaced iworld2)
              player2-signup-bundle)
(define (register-player lobby world)
  (make-bundle (add-new-player lobby (next-name lobby) world)
               (list (make-mail world (next-name lobby)))
               '()))

; next-name : Lobby -> String
; produce the next name to be assigned to a joining player
(check-expect (next-name empty-lobby) "player1")
(check-expect (next-name one-player-lobby-unplaced) "player2")
(check-expect (next-name two-player-lobby-unplaced) "player3")
(define (next-name lobby)
  (string-append "player" (number->string (add1 (length lobby)))))

; add-new-player : Lobby String IWorld -> Lobby
(check-expect (add-new-player empty-lobby "player1" iworld1) one-player-lobby-unplaced)
(check-expect (add-new-player one-player-lobby-unplaced "player2" iworld2)
              two-player-lobby-unplaced)
(define (add-new-player lobby name world)
  (snoc (make-player name '() world) lobby))

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

; handle-message-in-lobby : Lobby IWorld PlayerMessage -> Lobby
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
      (place-player-pieces lobby world (placement-msg->pieces (second message)))
      lobby))

; place-player-pieces : Lobby IWorld [List-of Piece] -> Lobby
; let the player place those pieces down
; INVARIANT: there exists a player matching the IWorld in the list
(check-expect (handle-message-in-lobby two-player-lobby-unplaced
                                       iworld1
                                       player1-place-message)
              two-player-lobby-one-placed)
(define (place-player-pieces lobby world pieces)
  (map (lambda (p) (if (iworld=? (player-world p) world)
                       (make-player (player-name p) pieces (player-world p))
                       p))
       lobby))

; placement-msg->pieces : PlacementMessage -> [List-of Piece]
; convert from the message of ship placement to the server-side representation
(check-expect (placement-msg->pieces player1-placement) player1-pieces)
(check-expect (placement-msg->pieces player2-placement) player2-pieces)
(define (placement-msg->pieces placement)
  (map (lambda (piece)
         (map (lambda (p-msg) (make-posn (first p-msg) (second p-msg)))
              piece))
       placement))

; handle-message-in-game : BSGame IWorld PlayerMessage -> [U BSU ...]
; handles messages relevant to an in-progress game and ignores others
(define (handle-message-in-game bsgame world message)
  (cond [(string=? "attack" (first message)) ...]
        [else bsgame]))














