#lang racket
(require "parsing.rkt")
(require rackunit)

; TODO: WRITE TESTS!!
;testcases for function parse-html-tag
(check-equal? (parse-html-tag "<html></html>") '("<html>" "</html>") "Basic case")
(check-equal? (parse-html-tag "<html>") '("<html>" "") "Extreme case: empty end text")
(check-equal? (parse-html-tag "<html> abcde ") '("<html>" " abcde ") "Random case")
(check-equal? (parse-html-tag "<hey><html>") '(error "<hey><html>") "Error case: wrong header")
(check-equal? (parse-html-tag "<htm></html>") '(error "<htm></html>") "Error case: wrong header")
(check-equal? (parse-html-tag "html></html>") '(error "html></html>") "Error case: wrong header")
(check-equal? (parse-html-tag " <html></html>") '(error " <html></html>")
              "Error case: space not allowed before head")
(check-equal? (parse-html-tag "") '(error "") " Error case: no head")
(check-equal? (parse-html-tag "ab") '(error "ab") "Error case: no head")

;testcases for function make-text-parser
(define parse-hi (make-text-parser "hi"))
(check-equal? (parse-hi "hiya!") '("hi" "ya!") "Test case")
(check-equal? (parse-hi "h") '(error "h") "Error case: wrong header")
(define parse-empty (make-text-parser ""))
(check-equal? (parse-empty "") '("" "") "Extreme case: empty header")
(check-equal? (parse-empty "abc") '("" "abc") "Extreme case: empty header")
(define parse-324 (make-text-parser "324"))
(check-equal? (parse-324 "csc324 is best") '(error "csc324 is best") "Error case: wrong header")
(define parse-space (make-text-parser "  "))
(check-equal? (parse-324 " 1") '(error " 1") "Error case: wrong header")
(check-equal? (parse-space "  11  ") '("  " "11  ") "Special case: space character")