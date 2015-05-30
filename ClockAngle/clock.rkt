#lang racket

(define INPUT (file->lines "ActualInput.txt"))
(define OUTFILE "ActualOutput.txt")

(define num-input (car INPUT))
(define times (map (compose (curry map string->number)
                            (curryr string-split ":"))
                   (cdr INPUT)))


;; (h m s) -> (ang-h ang-m ang-s)
(define/match (abs-hand-positions t)
  ((`(,h ,m ,s))
   ; =>
   `(,(+ (* 30 h) (* 0.5 m) (* (/ 0.5 60) s))
     ,(+ (* 6 m) (* (/ 6 60) s))
     ,(* 6 s))))

(define (normalize-angle a)
  (if (> a 180) (- 360.0 a) a))

(define (>π? a) (> a 180))
(define (<π? a) (not (>π? a)))

(define (Δang a1 a2)
  (cond
    [(and (<π? a1) (<π? a2))
     ; =>
     (normalize-angle
      (- (max a1 a2) (min a1 a2)))]
    [(and (<π? a1) (>π? a2))
     ; =>
     (normalize-angle
      (+ a1 (normalize-angle a2)))]
    [(and (>π? a1) (<π? a2))
     ; =>
     (Δang a2 a1)]
    [(and (>π? a1) (>π? a2))
     ; =>
     (Δang (normalize-angle a1)
           (normalize-angle a2))]))

(define (calc-angles t)
  (match-let ((`(,h ,m ,s) (abs-hand-positions t)))
    (map (curryr ~r #:precision '(= 2))
    `(,(Δang h m)
      ,(Δang h s)
      ,(Δang m s)))))

(define OUTPUT
  ((curryr string-join "\n")
   (cons num-input
         (map (curryr string-join ", ")
              (map calc-angles times)))))


(display-to-file OUTPUT OUTFILE #:exists 'replace)
