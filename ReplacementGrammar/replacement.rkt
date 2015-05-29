#lang racket

(define INPUT-FILE "ActualInput.txt")
;(define Sample-soln "ReplacementGrammar.txt")
(define OUTPUT-FILE "ActualOutput.txt")

(define INPUT (file->lines INPUT-FILE))

(define message (last INPUT))

(define rules 
  (let ((split (lambda (str) 
                 (string-split str "|" #:trim? #f))))
    
         (filter-not null?
                     (map split (drop-right INPUT 1)))))

(define (rewrite rules message)
  
  (define (rewrite1 rule message)
    (let ((from (car rule))
          (to (cadr rule)))
      (string-replace message from to)))
  
  (string-append (foldl rewrite1 message rules) "\n"))

;; write
(display-to-file (rewrite rules message) OUTPUT-FILE 
                 #:exists 'replace)
