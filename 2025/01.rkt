#lang racket

(define input (file->lines "input/input_1.txt"))

(define (parse inst)
  (values (string-ref inst 0) 
          (string->number (substring inst 1))))
;; p1
(define (part1)
  (define-values (pos zeros)
    (for/fold ([position 50] [zero-count 0]) ([inst input])
      (define-values (dir dist) (parse inst))
      (define new-pos (modulo (+ position (* dist (if (char=? dir #\L) -1 1))) 100))
      (values new-pos (+ zero-count (if (= new-pos 0) 1 0)))))
  zeros)

;; p2
(define (part2)
  (define-values (pos zeros)
    (for/fold ([position 50] [zero-count 0]) ([inst input])
      (define-values (dir dist) (parse inst))
      (define delta (if (char=? dir #\L) -1 1))
      (for/fold ([p position] [z zero-count]) ([_ (in-range dist)])
        (define new-p (modulo (+ p delta) 100))
        (values new-p (+ z (if (= new-p 0) 1 0))))))
  zeros)

(printf "Part 1: ~a\n" (part1))
(printf "Part 2: ~a\n" (part2))