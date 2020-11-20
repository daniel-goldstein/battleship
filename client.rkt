;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname client) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define-struct signing-up [])
(define-struct in-lobby [name ships])
;;; A World is one of
;;; - (make-signing-up)
;;; - (make-in-lobby String)
;;; - BattleshipWorld
#;
(define (world-temp w)
  (cond [(in-lobby? w) ...]
        [(in-lobby? w) ... (in-lobby-name w)]
        [(bsworld? w) ... (bsworld-temp w)]))

;;; A Package is one of:
;;; - SignupPackage
;;; - PlacementPackage
;;; - AttackPackage

(define player2-name "Bob")

(define piece1-place-message '((0 0) (1 0) (2 0)))
(define piece2-place-message '((5 5)))
(define piece1 (list (make-posn 0 0) (make-posn 1 0) (make-posn 2 0)))
(define piece2 (list (make-posn 5 5)))

(define ships-placement (list piece1-place-message piece2-place-message))
(define ships (list piece1 piece2))

(define attack-player2-message (list "attack" (list player2-name (list 0 0))))

(define-struct bsworld [ships my-name players strikes])
; A BattleshipWorld is a (make-bsworld [List-of Piece] String [List-of String] [List-of Posn])

;;; A Piece is a [List-of Posn]

(define SCENE-WIDTH 100)
(define SCENE-HEIGHT 100)
(define BACKGROUND (empty-scene SCENE-WIDTH SCENE-HEIGHT))

; client : String -> World
(define (client my-name)
  (big-bang (make-signing-up)
    [on-tick handle-tick]
    [to-draw draw-world]
    [on-receive handle-receive]
    [name my-name]
    [register LOCALHOST]
    [port 9000]))


;;; handle-tick : World -> [U World Package]
(define (handle-tick w) w)

(define (draw-world w)
  (overlay (world-message w) BACKGROUND))

;;; world-message : World -> Image
(define (world-message w)
  (cond [(signing-up? w) (text "signing up..." 24 "black")]
        [(in-lobby? w) (text (in-lobby-name w) 24 "black")]
        [(bsworld? w) (text "Playing!" 24 "black")]))

;;; handle-receive : World ServerResponse -> World
(define (handle-receive w msg)
  (cond [(and (string? msg) (not (string=? msg "go"))) (make-placement-package msg)]
        [(and (string? msg) (string=? msg "go")) (make-attack-package w)]
        [else w]))

; make-placement-package : String -> PlacementPackage
(define (make-placement-package my-name)
    (make-package (make-bsworld ships my-name empty empty)
                  (list "place" ships-placement)))

; make-attack-package : BSWorld -> AttackPackage
(define (make-attack-package w)
  (make-package w attack-player2-message))

(define (test-clients _)
  (launch-many-worlds (client "Alice") (client "Bob")))




