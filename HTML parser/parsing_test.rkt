#lang racket
(require "parsing.rkt")
(require rackunit)

;parse-html-tag
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


;make-text-parser
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


;parse-non-special-char
(check-equal? (parse-non-special-char "hi") '(#\h "i") "Normal text")
(check-equal? (parse-non-special-char "one") '(#\o "ne") "Normal text")
(check-equal? (parse-non-special-char "a") '(#\a "") "Extreme case: String length is 1")
(check-equal? (parse-non-special-char "  ") '(#\space " ") "Special case: space character")
(check-equal? (parse-non-special-char "111") '(#\1 "11") "Normal text: number")
(check-equal? (parse-non-special-char "<html>") '(error "<html>") "Error case: special char <")
(check-equal? (parse-non-special-char "=5") '(error "=5") "Error case: special char =")
(check-equal? (parse-non-special-char "/say what") '(error "/say what") "Error case: special char /")


;parse-plain-char
(check-equal? (parse-plain-char "bbc") '(#\b "bc") "Normal text")
(check-equal? (parse-plain-char ">tell me tell me") '(error ">tell me tell me")
              "Error case: special char")
(check-equal? (parse-plain-char "a=") '(#\a "=") "Normal text")
(check-equal? (parse-plain-char " white space error") '(error " white space error")
              "Error case: white space")
;(check-equal? (parse-plain-char "") '(error "") "Error case: empty text")
(check-equal? (parse-plain-char "\"") '(error "\"") "Error case: special char")


;either
(check-equal? ((either parse-html-tag parse-non-special-char)"<html> good <html>") '("<html>" " good <html>")
              "Test <html> tag")
(check-equal? ((either parse-html-tag parse-non-special-char)"<html good <html>") '(error "<html good <html>")
              "Error case: both parser failed")
(check-equal? ((either parse-html-tag parse-non-special-char)"html good <html>") '(#\h "tml good <html>")
              "Test 2nd parser")
;self defined parser
(define parse-abc (make-text-parser "abc"))
(check-equal? ((either parse-abc parse-plain-char)"abc") '("abc" "") "Test 1st parser")
(check-equal? ((either parse-abc parse-plain-char)"ab") '(#\a "b") "Test 2nd parser")
(check-equal? ((either parse-abc parse-plain-char)"<ab") '(error "<ab") "Error case: both parser failed")
;self defined parser - special character
(define parse-bracket (make-text-parser "<"))
(check-equal? ((either parse-abc parse-bracket)"<ab") '("<" "ab") "Test 2nd parser")
(check-equal? ((either parse-plain-char parse-bracket)"<ab") '("<" "ab") "Test 2nd parser")
(check-equal? ((either parse-non-special-char parse-plain-char)" call 911") '(#\space "call 911") "Test 1st parser")
(check-equal? ((either parse-plain-char parse-non-special-char)" call 911") '(#\space "call 911") "Test 2nd parser")
(check-equal? ((either parse-plain-char parse-non-special-char)"=") '(error "=") "Error case: special char")


;both
;self defined parser
(define parse-blue (make-text-parser "blue"))
;self defined parser
(define parse-green (make-text-parser "green"))
(check-equal? ((both parse-blue parse-green) "bluegreen!") '(("blue" "green") "!")
              "Two working words parser")
(check-equal? ((both parse-green parse-blue) "greenblueyeah") '(("green" "blue") "yeah")
              "Two working words parser")
(check-equal? ((both parse-green parse-blue) "greenblu") '(error "greenblu")
              "Error case: 2nd parser failed")
(check-equal? ((both parse-bracket parse-blue) "blue") '(error "blue")
              "Error case: 1st parser failed")
(check-equal? ((both parse-bracket parse-blue) "<blue") '(("<" "blue") "")
              "Two working parser, one special char, 2nd one word")
;(check-expect ((both parse-non-special-char parse-plain-char)" ") '(error " ")
;              "Error case: 2nd one failed")
(check-equal? ((both parse-non-special-char parse-plain-char)" N") '((#\space #\N) "")
              "Two working char parser")
(check-equal? ((both parse-non-special-char parse-plain-char)"  ") '(error "  ")
              "Error case: 2nd parser failed")
(check-equal? ((both parse-non-special-char parse-html-tag)"<html>") '(error "<html>")
              "Error case: 2nd parser failed")
;(check-expect ((both parse-html-tag parse-non-special-char)"<html>") '(error "<html>")
;              "Error case: 2nd parser failed")
(check-equal? ((both parse-html-tag parse-non-special-char)"<html> ") '(("<html>" #\space) "")
              "Two working parser: one word, one char")



