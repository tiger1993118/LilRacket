#lang racket
(require "parsing.rkt")
(require rackunit)

; TODO: WRITE TESTS!!
;testcases for function parse-html-tag
(check-equal? (parse-html-tag "<html></html>") '("<html>" "</html>") "Basic case")
(check-equal? (parse-html-tag "<html>") '("<html>" "") "Extreme case: empty end text")
(check-equal? (parse-html-tag "<html> abcde ") '("<html>" " abcde ") "Random case")
(check-equal? (parse-html-tag "<hey><html>") '(error "<hey><html>") "Error case: wrong head")
(check-equal? (parse-html-tag "<htm></html>") '(error "<htm></html>") "Error case: wrong head")
(check-equal? (parse-html-tag "html></html>") '(error "html></html>") "Error case: wrong head")
(check-equal? (parse-html-tag " <html></html>") '(error " <html></html>")
              "Error case: space not allowed before head")
(check-equal? (parse-html-tag "") '(error "") " Error case: no head")
(check-equal? (parse-html-tag "ab") '(error "ab") "Error case: no head")