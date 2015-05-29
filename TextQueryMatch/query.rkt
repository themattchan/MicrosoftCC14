#lang racket

(define INFILE (file->lines "SampleInput.txt"))
(define OUTFILE "SampleOutput.txt")

(define num-of-pairs (car INFILE))
(define query-strings (cdr INFILE))

(define (runQuery query string)
  "fuck"
  )

(define/match (doList lst)
  [((? null?)) '()]
  [(`(,query ,string ,rest ...))
   ; => 
   (cons (runQuery query string) (doList rest))])


;(let ((OUTPUT (string-join 
;            (cons num-of-pairs (doList query-strings))
;            "\n")))
;  (display-to-file OUTPUT OUTFILE
;                   #:exists 'replace))
