;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname movement) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)

(define-struct paddle [posn moving direction])
;; a paddle is a
;; (make-paddle posn bool number)
;; where the number is from 0 to 3,
;; with each number representing a
;; cardinal direction, going clockwise.

(define-struct ws [p1 p2])
;; a ws is a
;; (make-ws paddle paddle)

(define paddle-width 10)
(define paddle-height 30)
(define screen-width 900)
(define screen-height 500)

(define initial-state
  (make-ws (make-paddle (make-posn (/ screen-width 4) (-(/ screen-height 2))) #false 0) (make-paddle (make-posn (/ screen-width (/ 4 3)) (-(/ screen-height 2))) #false 0)))

(define (key-down-handler ws ke)
  (cond [(and (not (paddle-moving (ws-p1 ws))) (key=? ke "w"))(make-ws (make-paddle (paddle-posn (ws-p1 ws)) #true 0) (ws-p2 ws))]
        [(and (not (paddle-moving (ws-p1 ws))) (key=? ke "d"))(make-ws (make-paddle (paddle-posn (ws-p1 ws)) #true 1) (ws-p2 ws))]
        [(and (not (paddle-moving (ws-p1 ws))) (key=? ke "s"))(make-ws (make-paddle (paddle-posn (ws-p1 ws)) #true 2) (ws-p2 ws))]
        [(and (not (paddle-moving (ws-p1 ws))) (key=? ke "a"))(make-ws (make-paddle (paddle-posn (ws-p1 ws)) #true 3) (ws-p2 ws))]
        [(and (not (paddle-moving (ws-p2 ws))) (key=? ke "up"))(make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #true 0))]
        [(and (not (paddle-moving (ws-p2 ws))) (key=? ke "right"))(make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #true 1))]
        [(and (not (paddle-moving (ws-p2 ws))) (key=? ke "down"))(make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #true 2))]
        [(and (not (paddle-moving (ws-p2 ws))) (key=? ke "left"))(make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #true 3))]
        [else ws]))

(define (key-up-handler ws ke)
  (cond [(and (paddle-moving (ws-p1 ws)) (key=? ke "w"))(make-ws (make-paddle (paddle-posn (ws-p1 ws)) #false 0) (ws-p2 ws))]
        [(and (paddle-moving (ws-p1 ws)) (key=? ke "d"))(make-ws (make-paddle (paddle-posn (ws-p1 ws)) #false 1) (ws-p2 ws))]
        [(and (paddle-moving (ws-p1 ws)) (key=? ke "s"))(make-ws (make-paddle (paddle-posn (ws-p1 ws)) #false 2) (ws-p2 ws))]
        [(and (paddle-moving (ws-p1 ws)) (key=? ke "a"))(make-ws (make-paddle (paddle-posn (ws-p1 ws)) #false 3) (ws-p2 ws))]
        [(and (paddle-moving (ws-p2 ws)) (key=? ke "up"))(make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #false 0))]
        [(and (paddle-moving (ws-p2 ws)) (key=? ke "right"))(make-ws (ws-p1 ws)(make-paddle  (paddle-posn (ws-p2 ws)) #false 1))]
        [(and (paddle-moving (ws-p2 ws)) (key=? ke "down"))(make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #false 2))]
        [(and (paddle-moving (ws-p2 ws)) (key=? ke "left"))(make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #false 3))]
        [else ws]))

(define (game-tick ws)
  (cond [(paddle-moving (ws-p1 ws))
         (cond [(= 1 (paddle-direction (ws-p1 ws)))
                (make-ws (make-paddle (make-posn (cond [(< (+ (posn-x (paddle-posn (ws-p1 ws))) (/ paddle-width 2)) (/ screen-width 2))
                                                        (+ 5 (posn-x (paddle-posn (ws-p1 ws))))]
                                                       [else (posn-x (paddle-posn (ws-p1 ws)))])
                                                 (posn-y (paddle-posn (ws-p1 ws))))
                         (paddle-moving (ws-p1 ws))
                         (paddle-direction (ws-p1 ws)))
                         (cond [(paddle-moving (ws-p2 ws))
                                (cond [(= 1 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (+ (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2)) screen-width)
                                                        (+ 5 (posn-x (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 0 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< 0 (abs (+ (posn-y (paddle-posn (ws-p2 ws))) (/ paddle-height 2)))) (+ 5 (posn-y (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 3 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (/ screen-width 2) (- (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2))) (- (posn-x (paddle-posn (ws-p2 ws))) 5)]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 2 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< (+ (abs (posn-y (paddle-posn (ws-p2 ws)))) (/ paddle-height 2)) screen-height) (- (posn-y (paddle-posn (ws-p2 ws))) 5)]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))])]
                               [else (ws-p2 ws)]))]
               [(= 0 (paddle-direction (ws-p1 ws)))
                (make-ws (make-paddle (make-posn (posn-x (paddle-posn (ws-p1 ws)))
                                                 (cond [(< 0 (abs (+ (posn-y (paddle-posn (ws-p1 ws))) (/ paddle-height 2)))) (+ 5 (posn-y (paddle-posn (ws-p1 ws))))]
                                                       [else (posn-y (paddle-posn (ws-p1 ws)))]))
                         (paddle-moving (ws-p1 ws))
                         (paddle-direction (ws-p1 ws)))
                         (cond [(paddle-moving (ws-p2 ws))
                                (cond [(= 1 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (+ (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2)) screen-width)
                                                        (+ 5 (posn-x (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 0 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< 0 (abs (+ (posn-y (paddle-posn (ws-p2 ws))) (/ paddle-height 2)))) (+ 5 (posn-y (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 3 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (/ screen-width 2) (- (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2))) (- (posn-x (paddle-posn (ws-p2 ws))) 5)]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 2 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< (+ (abs (posn-y (paddle-posn (ws-p2 ws)))) (/ paddle-height 2)) screen-height) (- (posn-y (paddle-posn (ws-p2 ws))) 5)]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))])]
                               [else (ws-p2 ws)]))]
               [(= 3 (paddle-direction (ws-p1 ws)))
                (make-ws (make-paddle (make-posn (cond [(< 0 (- (posn-x (paddle-posn (ws-p1 ws))) (/ paddle-width 2))) (- (posn-x (paddle-posn (ws-p1 ws))) 5)]
                                                       [else (posn-x (paddle-posn (ws-p1 ws)))])
                                                 (posn-y (paddle-posn (ws-p1 ws))))
                         (paddle-moving (ws-p1 ws))
                         (paddle-direction (ws-p1 ws)))
                         (cond [(paddle-moving (ws-p2 ws))
                                (cond [(= 1 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (+ (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2)) screen-width)
                                                        (+ 5 (posn-x (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 0 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< 0 (abs (+ (posn-y (paddle-posn (ws-p2 ws))) (/ paddle-height 2)))) (+ 5 (posn-y (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 3 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (/ screen-width 2) (- (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2))) (- (posn-x (paddle-posn (ws-p2 ws))) 5)]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 2 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< (+ (abs (posn-y (paddle-posn (ws-p2 ws)))) (/ paddle-height 2)) screen-height) (- (posn-y (paddle-posn (ws-p2 ws))) 5)]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))])]
                               [else (ws-p2 ws)]))]
               [(= 2 (paddle-direction (ws-p1 ws)))
                (make-ws (make-paddle (make-posn (posn-x (paddle-posn (ws-p1 ws)))
                                                 (cond [(< (+ (abs (posn-y (paddle-posn (ws-p1 ws)))) (/ paddle-height 2)) screen-height) (- (posn-y (paddle-posn (ws-p1 ws))) 5)]
                                                       [else (posn-y (paddle-posn (ws-p1 ws)))]))
                         (paddle-moving (ws-p1 ws))
                         (paddle-direction (ws-p1 ws)))
                         (cond [(paddle-moving (ws-p2 ws))
                                (cond [(= 1 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (+ (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2)) screen-width)
                                                        (+ 5 (posn-x (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 0 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< 0 (abs (+ (posn-y (paddle-posn (ws-p2 ws))) (/ paddle-height 2)))) (+ 5 (posn-y (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 3 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (/ screen-width 2) (- (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2))) (- (posn-x (paddle-posn (ws-p2 ws))) 5)]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 2 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< (+ (abs (posn-y (paddle-posn (ws-p2 ws)))) (/ paddle-height 2)) screen-height) (- (posn-y (paddle-posn (ws-p2 ws))) 5)]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))])]
                               [else (ws-p2 ws)]))])]
        [else
         (make-ws (ws-p1 ws)
                  (cond [(paddle-moving (ws-p2 ws))
                         (cond [(= 1 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (+ (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2)) screen-width)
                                                        (+ 5 (posn-x (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 0 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< 0 (abs (+ (posn-y (paddle-posn (ws-p2 ws))) (/ paddle-height 2)))) (+ 5 (posn-y (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 3 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (/ screen-width 2) (- (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2))) (- (posn-x (paddle-posn (ws-p2 ws))) 5)]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 2 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< (+ (abs (posn-y (paddle-posn (ws-p2 ws)))) (/ paddle-height 2)) screen-height) (- (posn-y (paddle-posn (ws-p2 ws))) 5)]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))])]
                        [else (ws-p2 ws)]))]))

(define (render ws)
  (place-images (list (rectangle paddle-width paddle-height "solid" "black")
                      (rectangle paddle-width paddle-height "solid" "black"))
                (list (make-posn (posn-x (paddle-posn (ws-p1 ws))) (- 0 (posn-y (paddle-posn (ws-p1 ws)))))
                       (make-posn (posn-x (paddle-posn (ws-p2 ws))) (- 0 (posn-y (paddle-posn (ws-p2 ws))))))
                (rectangle screen-width screen-height "outline" "white")))

(big-bang initial-state
          [to-draw render]
          [on-key key-down-handler]
          [on-release key-up-handler]
          [on-tick game-tick])