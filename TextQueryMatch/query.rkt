#lang racket

(define (runQuery query string)
  "fuck"
  )

(define/match (doList lst)
  [((? null?)) '()]
  [(`(,query ,string ,rest ...))
   ; => 
   (cons (runQuery query string) (doList rest))])

;; run
(match-let* ([INFILE  "SampleInput.txt"]
             [OUTFILE "SampleOutput.txt"]
             ; ----
             [`(,num-of-pairs ,query-strings ...)
              ; =>
              (file->lines INFILE)]
             [OUTPUT 
              ; =>
              (string-join 
               (cons num-of-pairs (doList query-strings))
               "\n")])
  (display-to-file OUTPUT OUTFILE
                   #:exists 'replace))
