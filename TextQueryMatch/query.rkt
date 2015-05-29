#lang racket

(define INFILE (file->lines "SampleInput.txt"))

(define num-of-pairs (string->number (car INFILE)))
(define query-strings (cdr INFILE))

(define (match? query string)
  "fuck"
  )

(define/match (doList lst)
  [((? null?)) '()]
  [(`(,query ,string ,rest ...))
   ; => 
   (cons (match? query string) (doList rest))])

(doList query-strings)

;(map match? input-pairs)