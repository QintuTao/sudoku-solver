;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |9*9 Sudoku Solver |) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Sudoku Solver
;; Qintu Tao
;; 2019-12-31, NewYear's Eve
;; A 9 * 9 Soduku Solver, following coding standards and processes introduced
;;  in UBC CPSC 110 Course and is using language BSL.
(require spd/tags)
(require 2htdp/image)
;; Constant
(define BOARD-SIZE 9)

;; Data Structure for sudoku
(@htdd Board)
;; Board is (listof Unit)
;; interp. the 9 * 9 Board for the Sudoku Solver

(define BOARD1 (list  5  3  #f   #f 7  #f   #f #f #f 
                      6  #f #f   1  9  5    #f #f #f 
                      #f 9  8    #f #f #f   #f 6  #f 
                    
                      8  #f #f   #f 6  #f   #f #f 3  
                      4  #f #f   8  #f  3   #f #f 1  
                      7  #f #f   #f 2  #f   #f #f 6  
                    
                      #f 6  #f   #f #f #f   2  8  #f 
                      #f #f #f   4  1  9    #f #f 5  
                      #f #f #f   #f 8  #f   #f 7  9 ))
(@htdd Unit)
;; Unit is one of:
;; - #f
;; - Natural
;; interp. unit is the state of the unit
;;  #f means empty;
;; CONSTRAINT, when Natural, range in [1,9]

(define UNIT1 1)
(define UNIT #f)
(check-expect (solve BOARD1)
              (list 5 3 4   6 7 8   9 1 2
                    6 7 2   1 9 5   3 4 8
                    1 9 8   3 4 2   5 6 7
                    
                    8 5 9   7 6 1   4 2 3
                    4 2 6   8 5 3   7 9 1
                    7 1 3   9 2 4   8 5 6
                    
                    9 6 1   5 3 7   2 8 4
                    2 8 7   4 1 9   6 3 5
                    3 4 5   2 8 6   1 7 9))
              
;; Solve Function
(define (solve b0)
  (local [(define-struct state (b i))
          (define (solve-s s)
            (if (solved? (state-b s))
                (state-b s) ;todo: add local for state-b s
                (solve-los (next-state s))))

          (define (solve-los los)
            (cond [(empty? los) false]
                  [else
                   (local [(define try (solve-s (first los)))]
                     (if (not (false? try))
                         try
                         (solve-los (rest los))))]))

          (define (next-state s)
            (local [(define bd (state-b s))
                    (define id (state-i s))
                    (define u (list-ref bd id))
                    (define next-i (add1 id))]
              (if (false? u)
                  (local [(define row (bd-row bd id))
                          (define col (bd-col bd id))
                          (define sqr3 (bd-sqr bd id))]
                    (map (λ (n)
                           (make-state (assign bd n)
                                       next-i))
                         (filter (λ (n)
                                   (valid? n row col sqr3))
                                 (build-list BOARD-SIZE add1))))
                  (list (make-state bd next-i)))))]
    
    (solve-s (make-state b0 0))))


(@htdf solved?)
(@signature Board -> Boolean)
;; produce true if there is no #f left on the board

(check-expect (solved? BOARD1) false)
(check-expect (solved? (list 3)) true)

(define (solved? b)
  (not (member? #f b)))

(@htdf bd-row)
(@signature Board Natural -> (listof Unit))
;; produce the current row of an index
(check-expect (bd-row BOARD1 32)
              (list
               8
               #false
               #false
               #false
               6
               #false
               #false
               #false
               3))
(check-expect  (bd-row BOARD1 80)
               (list
                #false
                #false
                #false
                #false
                8
                #false
                #false
                7
                9))             
(define (bd-row b i)
  (build-list BOARD-SIZE (λ (x)
                  (list-ref b (+ (* (quotient i BOARD-SIZE) BOARD-SIZE) x)))))


(@htdf bd-col)
(@signature Board Natural -> (listof Unit))
;; produce the current col of an index
(check-expect (bd-col BOARD1 32)
              (list
               #false
               5
               #false
               #false
               3
               #false
               #false
               9
               #false))
(check-expect (bd-col BOARD1 80)
              (list
               #false
               #false
               #false
               3
               1
               6
               #false
               5
               9))
(define (bd-col b i)
  (build-list BOARD-SIZE (λ (x)
                  (list-ref b (+ (* x BOARD-SIZE) (remainder i BOARD-SIZE))))))


(@htdf bd-sqr)
(@signature Board Natural -> (listof Unit))
;; produce the 3*3 square of an index
(check-expect (bd-sqr BOARD1 32)
              (list
               #false
               6
               #false
               8
               #false
               3
               #false
               2
               #false))
(check-expect (bd-sqr BOARD1 80)
              (list
               2
               8
               #false
               #false
               #false
               5
               #false
               7
               9))
(define (bd-sqr b i)
  (build-list 9 (λ (x)
                  (list-ref b (+ (+ (* (quotient (quotient i BOARD-SIZE) 3) 
                                       27)
                                    (* (quotient x 3) BOARD-SIZE))
                                 (+ (* (quotient (remainder i BOARD-SIZE) 3)
                                       3)
                                    (remainder x 3)))))))
                            

(@htdf valid?)
(@signature Natural (listof Unit) (listof Unit) (listof Unit) -> Boolean)
;; produce true if the unit is valid in the given
;;  row, col, and the 3*3 square, i.e. No Duplicates
(define TEST-ROW (bd-row BOARD1 32))
(define TEST-COL (bd-col BOARD1 32))
(define TEST-SQR (bd-sqr BOARD1 32))

(check-expect (valid? 4 TEST-ROW TEST-COL TEST-SQR) true)
(check-expect (valid? 3 TEST-ROW TEST-COL TEST-SQR) false)

(define (valid? n row col sqr)
  (not (or (member? n row)
           (member? n col)
           (member? n sqr))))

            
(@htdf assign)
(@signature Board Natural -> Board)
;; assign the unit of the given index to the given value,
;;  produces the changed board

;; NOTE: test doen interactively 
(define (assign b0 n)
  (local [(define (change b acc)
            ;; Accumulator
            ;; acc is (listof Unit)
            ;;  is the previous part of the board

            (cond [(empty? b) acc]
                  [(false? (first b))
                   (append acc (list n)
                           (rest b))]
                  [else
                   (change (rest b)
                           (append acc (list (first b))))]))]
    (change b0 empty)))                         
                    
