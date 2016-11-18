;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Final racket ralphs|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define screen-width 900)
(define screen-height 500)

(define-struct paddle [posn moving direction])
(define-struct ball [posn deltax deltay angle counter])
(define-struct ws [p1 p2 ball])

(define paddle-width 10)
(define paddle-height 60)

(define initial-state
  (make-ws
   (make-paddle (make-posn (/ screen-width 4) (-(/ screen-height 2))) #false 0)
   (make-paddle (make-posn (/ screen-width (/ 4 3)) (-(/ screen-height 2))) #false 0)
   (make-ball (make-posn (/ screen-width 2)(/ screen-height 2))  0  0 0  0)))
; paddle movement
(define (key-down-handler ws ke)
  (cond [(and (not (paddle-moving (ws-p1 ws))) (key=? ke "w"))(make-ws (make-paddle (paddle-posn (ws-p1 ws)) #true 0) (ws-p2 ws) (ws-ball ws))]
        [(and (not (paddle-moving (ws-p1 ws))) (key=? ke "d"))(make-ws (make-paddle (paddle-posn (ws-p1 ws)) #true 1) (ws-p2 ws) (ws-ball ws))]
        [(and (not (paddle-moving (ws-p1 ws))) (key=? ke "s"))(make-ws (make-paddle (paddle-posn (ws-p1 ws)) #true 2) (ws-p2 ws) (ws-ball ws))]
        [(and (not (paddle-moving (ws-p1 ws))) (key=? ke "a"))(make-ws (make-paddle (paddle-posn (ws-p1 ws)) #true 3) (ws-p2 ws) (ws-ball ws))]
        [(and (not (paddle-moving (ws-p2 ws))) (key=? ke "up"))(make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #true 0) (ws-ball ws))]
        [(and (not (paddle-moving (ws-p2 ws))) (key=? ke "right"))(make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #true 1) (ws-ball ws))]
        [(and (not (paddle-moving (ws-p2 ws))) (key=? ke "down"))(make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #true 2) (ws-ball ws))]
        [(and (not (paddle-moving (ws-p2 ws))) (key=? ke "left"))(make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #true 3) (ws-ball ws))]
        [else ws]))

(define (key-up-handler ws ke)
  (cond [(and (paddle-moving (ws-p1 ws)) (key=? ke "w"))(make-ws (make-paddle (paddle-posn (ws-p1 ws)) #false 0) (ws-p2 ws) (ws-ball ws))]
        [(and (paddle-moving (ws-p1 ws)) (key=? ke "d"))(make-ws (make-paddle (paddle-posn (ws-p1 ws)) #false 1) (ws-p2 ws) (ws-ball ws))]
        [(and (paddle-moving (ws-p1 ws)) (key=? ke "s"))(make-ws (make-paddle (paddle-posn (ws-p1 ws)) #false 2) (ws-p2 ws) (ws-ball ws))]
        [(and (paddle-moving (ws-p1 ws)) (key=? ke "a"))(make-ws (make-paddle (paddle-posn (ws-p1 ws)) #false 3) (ws-p2 ws) (ws-ball ws))]
        [(and (paddle-moving (ws-p2 ws)) (key=? ke "up"))(make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #false 0) (ws-ball ws))]
        [(and (paddle-moving (ws-p2 ws)) (key=? ke "right"))(make-ws (ws-p1 ws)(make-paddle  (paddle-posn (ws-p2 ws)) #false 1) (ws-ball ws))]
        [(and (paddle-moving (ws-p2 ws)) (key=? ke "down"))(make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #false 2) (ws-ball ws))]
        [(and (paddle-moving (ws-p2 ws)) (key=? ke "left"))(make-ws (ws-p1 ws) (make-paddle (paddle-posn (ws-p2 ws)) #false 3) (ws-ball ws))]
        [else ws]))
;p1 and p2 are inststances of paddle
(define (ball-bounce ws)
  (cond [(>= (posn-x (ball-posn (ws-ball ws))) screen-width) (make-ws (ws-p1 ws) (ws-p2 ws) (make-ball (ball-posn (ws-ball ws)) (* -1 (ball-deltax (ws-ball ws))) (ball-deltay (ws-ball ws)) (ball-angle (ws-ball ws)) (+ 1 (ball-counter (ws-ball ws)))))] 
        [(<= (posn-x (ball-posn (ws-ball ws))) 0)           (make-ws (ws-p1 ws) (ws-p2 ws) (make-ball (ball-posn (ws-ball ws)) (* -1 (ball-deltax (ws-ball ws))) (ball-deltay (ws-ball ws)) (ball-angle (ws-ball ws)) (+ 1 (ball-counter (ws-ball ws)))))]
        [(>= (posn-y (ball-posn (ws-ball ws))) screen-height)(make-ws (ws-p1 ws) (ws-p2 ws) (make-ball (ball-posn (ws-ball ws)) (ball-deltax  (ws-ball ws)) (* -1 (ball-deltay (ws-ball ws)))(ball-angle (ws-ball ws)) (+ 1 (ball-counter (ws-ball ws)))))]
        [(<= (posn-y (ball-posn (ws-ball ws))) 0)           (make-ws (ws-p1 ws) (ws-p2 ws) (make-ball (ball-posn (ws-ball ws)) (ball-deltax  (ws-ball ws)) (* -1 (ball-deltay (ws-ball ws)))(ball-angle (ws-ball ws)) (+ 1 (ball-counter (ws-ball ws)))))]
        [else (make-ws (ws-p1 ws) (ws-p2 ws) (make-ball (ball-posn (ws-ball ws)) (ball-deltax  (ws-ball ws)) (ball-deltay (ws-ball ws)) (ball-angle (ws-ball ws)) (+ 1 (ball-counter (ws-ball ws)))))]
        ))
;lets draw the ball
(define gp (circle 12 "solid" "red"))
(define field (empty-scene screen-width screen-height))
(place-image gp 100 100 field)

(define (drawEmpty ws)
  (place-images (list (rectangle paddle-width paddle-height "solid" "black")
                      (rectangle paddle-width paddle-height "solid" "black")
                      gp) 
                (list (make-posn (posn-x (paddle-posn (ws-p1 ws))) (- 0 (posn-y (paddle-posn (ws-p1 ws)))))
                      (make-posn (posn-x (paddle-posn (ws-p2 ws))) (- 0 (posn-y (paddle-posn (ws-p2 ws)))))
                      
                      (make-posn (+ (posn-x (ball-posn (ws-ball ws)))  (ball-deltax (ws-ball ws)))
                                 (+ (posn-y (ball-posn (ws-ball ws)))  (ball-deltay (ws-ball ws)))))
                
                (rectangle screen-width screen-height "outline" "white")))
(define (both a b)
  b)
(define (ballMove ws)   
   (make-ws (ws-p1 ws) (ws-p2 ws) (make-ball
                        (make-posn
                         (+ (posn-x (ball-posn (ws-ball ws)))
                            (ball-deltax (ws-ball ws)))
                         (+ (posn-y (ball-posn (ws-ball ws)))
                            (ball-deltay (ws-ball ws))))
                        (ball-deltax(ws-ball ws))
                        (ball-deltay (ws-ball ws))
                        (ball-angle (ws-ball ws))
                        (+ 1 (ball-counter (ws-ball ws))))))


(define test-state (make-ws (make-paddle (make-posn 0 0) #true "forward")
                            (make-paddle (make-posn 0 0) #true "forward")
                            (make-ball
                              (make-posn 100 100)
                              10
                              10
                              0
                              1)))
(define (ballPhysics ws)
  (both (cond
    [(even? (ball-counter (ws-ball ws))) (ballMove ws)]
    [(odd? (ball-counter (ws-ball ws))) (ball-bounce ws)])
    (cond [(paddle-moving (ws-p1 ws))
         (cond [(= 1 (paddle-direction (ws-p1 ws)))
                (make-ws (make-paddle (make-posn (cond [(< (+ (posn-x (paddle-posn (ws-p1 ws))) (/ paddle-width 2)) (/ screen-width 2))
                                                        (+ 10 (posn-x (paddle-posn (ws-p1 ws))))]
                                                       [else (posn-x (paddle-posn (ws-p1 ws)))])
                                                 (posn-y (paddle-posn (ws-p1 ws))))
                         (paddle-moving (ws-p1 ws))
                         (paddle-direction (ws-p1 ws)))
                         (cond [(paddle-moving (ws-p2 ws))
                                (cond [(= 1 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (+ (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2)) screen-width)
                                                        (+ 10 (posn-x (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 0 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< 0 (abs (+ (posn-y (paddle-posn (ws-p2 ws))) (/ paddle-height 2)))) (+ 10 (posn-y (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 3 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (/ screen-width 2) (- (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2))) (- (posn-x (paddle-posn (ws-p2 ws))) 10)]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 2 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< (+ (abs (posn-y (paddle-posn (ws-p2 ws)))) (/ paddle-height 2)) screen-height) (- (posn-y (paddle-posn (ws-p2 ws))) 10)]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))])]
                               [else (ws-p2 ws)]) (ws-ball ws))]
               [(= 0 (paddle-direction (ws-p1 ws)))
                (make-ws (make-paddle (make-posn (posn-x (paddle-posn (ws-p1 ws)))
                                                 (cond [(< 0 (abs (+ (posn-y (paddle-posn (ws-p1 ws))) (/ paddle-height 2)))) (+ 10 (posn-y (paddle-posn (ws-p1 ws))))]
                                                       [else (posn-y (paddle-posn (ws-p1 ws)))]))
                         (paddle-moving (ws-p1 ws))
                         (paddle-direction (ws-p1 ws)))
                         (cond [(paddle-moving (ws-p2 ws))
                                (cond [(= 1 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (+ (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2)) screen-width)
                                                        (+ 10 (posn-x (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 0 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< 0 (abs (+ (posn-y (paddle-posn (ws-p2 ws))) (/ paddle-height 2)))) (+ 10 (posn-y (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 3 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (/ screen-width 2) (- (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2))) (- (posn-x (paddle-posn (ws-p2 ws))) 10)]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 2 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< (+ (abs (posn-y (paddle-posn (ws-p2 ws)))) (/ paddle-height 2)) screen-height) (- (posn-y (paddle-posn (ws-p2 ws))) 10)]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))])]
                               [else (ws-p2 ws)]) (ws-ball ws))]
               [(= 3 (paddle-direction (ws-p1 ws)))
                (make-ws (make-paddle (make-posn (cond [(< 0 (- (posn-x (paddle-posn (ws-p1 ws))) (/ paddle-width 2))) (- (posn-x (paddle-posn (ws-p1 ws))) 10)]
                                                       [else (posn-x (paddle-posn (ws-p1 ws)))])
                                                 (posn-y (paddle-posn (ws-p1 ws))))
                         (paddle-moving (ws-p1 ws))
                         (paddle-direction (ws-p1 ws)))
                         (cond [(paddle-moving (ws-p2 ws))
                                (cond [(= 1 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (+ (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2)) screen-width)
                                                        (+ 10 (posn-x (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 0 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< 0 (abs (+ (posn-y (paddle-posn (ws-p2 ws))) (/ paddle-height 2)))) (+ 10 (posn-y (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 3 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (/ screen-width 2) (- (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2))) (- (posn-x (paddle-posn (ws-p2 ws))) 10)]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 2 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< (+ (abs (posn-y (paddle-posn (ws-p2 ws)))) (/ paddle-height 2)) screen-height) (- (posn-y (paddle-posn (ws-p2 ws))) 10)]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))])]
                               [else (ws-p2 ws)])(ws-ball ws))]
               [(= 2 (paddle-direction (ws-p1 ws)))
                (make-ws (make-paddle (make-posn (posn-x (paddle-posn (ws-p1 ws)))
                                                 (cond [(< (+ (abs (posn-y (paddle-posn (ws-p1 ws)))) (/ paddle-height 2)) screen-height) (- (posn-y (paddle-posn (ws-p1 ws))) 10)]
                                                       [else (posn-y (paddle-posn (ws-p1 ws)))]))
                         (paddle-moving (ws-p1 ws))
                         (paddle-direction (ws-p1 ws)))
                         (cond [(paddle-moving (ws-p2 ws))
                                (cond [(= 1 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (+ (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2)) screen-width)
                                                        (+ 10 (posn-x (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 0 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< 0 (abs (+ (posn-y (paddle-posn (ws-p2 ws))) (/ paddle-height 2)))) (+ 10 (posn-y (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 3 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (/ screen-width 2) (- (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2))) (- (posn-x (paddle-posn (ws-p2 ws))) 10)]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 2 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< (+ (abs (posn-y (paddle-posn (ws-p2 ws)))) (/ paddle-height 2)) screen-height) (- (posn-y (paddle-posn (ws-p2 ws))) 10)]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))])]
                               [else (ws-p2 ws)])(ws-ball ws))])]
        [else
         (make-ws (ws-p1 ws)
                  (cond [(paddle-moving (ws-p2 ws))
                         (cond [(= 1 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (+ (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2)) screen-width)
                                                        (+ 10 (posn-x (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 0 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< 0 (abs (+ (posn-y (paddle-posn (ws-p2 ws))) (/ paddle-height 2)))) (+ 10 (posn-y (paddle-posn (ws-p2 ws))))]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 3 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (cond [(< (/ screen-width 2) (- (posn-x (paddle-posn (ws-p2 ws))) (/ paddle-width 2))) (- (posn-x (paddle-posn (ws-p2 ws))) 10)]
                                                       [else (posn-x (paddle-posn (ws-p2 ws)))])
                                                        (posn-y (paddle-posn (ws-p2 ws))))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))]
                               [(= 2 (paddle-direction (ws-p2 ws)))
                                (make-paddle (make-posn (posn-x (paddle-posn (ws-p2 ws)))
                                                        (cond [(< (+ (abs (posn-y (paddle-posn (ws-p2 ws)))) (/ paddle-height 2)) screen-height) (- (posn-y (paddle-posn (ws-p2 ws))) 10)]
                                                       [else (posn-y (paddle-posn (ws-p2 ws)))]))
                                             (paddle-moving (ws-p2 ws))
                                             (paddle-direction (ws-p2 ws)))])]
                        [else (ws-p2 ws)]) (ws-ball ws))])))
(big-bang initial-state
            [to-draw drawEmpty]
            [on-tick ballPhysics]
            [on-key key-down-handler]
            [on-release key-up-handler])