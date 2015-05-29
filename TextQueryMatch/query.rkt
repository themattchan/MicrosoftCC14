#lang racket

(define INFILE (open-input-file "SampleInput.txt" #:mode 'text))

(define (read-formatted-line IN)
  (string-downcase (read-line IN)))

(define num-pairs 
  (string->number (read-formatted-line INFILE)))

(define input-pairs
  (for/list ((_ num-pairs))
    (cons (read-formatted-line INFILE)
          (read-formatted-line INFILE))))


input-pairs

(define (match? pair)
  (let ((query (car pair))
        (string (cdr pair)))
    
    ))


;(map match? input-pairs)