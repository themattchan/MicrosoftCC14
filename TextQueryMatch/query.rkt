#lang racket

(define count-substring
  (compose length regexp-match*))

(define (unspace str)
  (string-join (string-split str) ""))

(define (runQuery query string)
  (if (or (< 0 (count-substring query string))
          (< 0 (count-substring query (unspace string))))
      "true" "false"))

(define/match (doList lst)
  [((? null?)) '()]
  [(`(,query ,string ,rest ...))
   (cons (runQuery query string) (doList rest))])

;; run
(match-let* ([INFILE  "ActualInput.txt"]
             [OUTFILE "ActualOutput.txt"]
             [`(,num ,xs ...)
              (map string-downcase (file->lines INFILE))]
             [OUTPUT
              (string-join (cons num (doList xs)) "\n")])

  (display-to-file OUTPUT OUTFILE
                   #:exists 'replace))
