#lang racket

(provide parse-html-tag make-text-parser
         parse-non-special-char parse-plain-char
         either both star
         parse-html)

;Helper functions
(provide parse-success apply-parse-char
         parse-non-special-text parse-plain-text 
         parse-open-tag)

#|
(parse-html-tag str)

Returning:
  '("<html>" "rest-of-str")
  '(error "str")

  If str starts with "<html>", returns a pair (list "<html>" rest), where
  rest is the part of str after "<html>".
  Otherwise, returns (list 'error str), signifying an error.

> (parse-html-tag "<html></html>")
'("<html>" "</html>")
> (parse-html-tag "<hey><html>")
'(error "<hey><html>")
|#
(define (parse-html-tag str)
  (if (string-prefix? str "<html>")
      (list "<html>" (substring str 6))
      (list 'error str)))


#|
(make-text-parser t)

Returning:
  parser function

  Return a parser that tries to read *one* occurrence of t at the
  start of its input.

> (define parse-hi (make-text-parser "hi"))
> (parse-hi "hiya!")
'("hi" "ya!")
> (parse-hi "goodbye hi")
'(error "goodbye hi")
|#
(define (make-text-parser t)
  (lambda (str)
    (if (string-prefix? str t)
        (list t (substring str (string-length t)))
        (list 'error str))))


#|
(parse-non-special-char str)

Returning:
  '(char "rest-of-string")
  '(error "str")
  
  Try to parse *one* non-special character at the start of str.

> (parse-non-special-char "hi")
'(#\h "i")
> (parse-non-special-char "<html>")
'(error "<html>")
|#
(define (parse-non-special-char str)
  (let ([char1 (string-ref str 0)])
    (if (or (equal? char1 #\<) (equal? char1 #\>)
            (equal? char1 #\=) (equal? char1 #\") (equal? char1 #\/))
        (list 'error str)
        (list char1 (substring str 1)))))
   


#|
(parse-plain-char str)

Returning:
  '(char "rest-of-string")
  '(error "str")
  
  Try to parse *one* non-special, non-white character at the start of str.

> (parse-plain-char "hi")
'(#\h "i")
> (parse-plain-char " hello!")
'(error " hello!")
|#
(define (parse-plain-char str)
  (if (equal? (string-ref str 0) #\space)
      (list 'error str)
      (parse-non-special-char str)))


#|--------------Parsing Combinators----------------------|#

#|
(either parser1 parser2)

Returning:
  '(parsed-part "rest-of-string")
  '(error "str")

  Return a new parser that does the following:
    - Try to apply parser 1; if success, return that result
    - Otherwise, return the result of applying parser 2

> ((either parse-plain-char parse-html-tag) "hello")
'(#\h "ello")
> ((either parse-plain-char parse-html-tag) "<html>hello")
'("<html>" "hello")
> ((either parse-plain-char parse-html-tag) "<xml>hello")
'(error "<xml>hello")
|#
(define (either parser1 parser2)
  (lambda (str)
    (let ([result1 (parser1 str)])
      (if (parse-success result1)
         result1
         (parser2 str)))))



#|
(both parser1 parser2)

Returning:
  '('(parsed-by-1 parsed-by-2) "rest-of-str")
  '(error "str")

  Return a new parser that does the following:
    - Apply parser1; if failure, return failure
    - Otherwise, apply parser2 to the rest of the string
      not parsed by parser1
    - If failure, emit failure, together with *original* string
    - If success, return (list data rest), where data is a *LIST*
      containing the data parsed by parser1 and parser2, in that order,
      and rest is the part of the string not parsed by either
      parser1 or parser2.

> ((both parse-html-tag parse-plain-char) "<html>hello")
'(("<html>" #\h) "ello")
> ((both parse-html-tag parse-plain-char) "<xml>hello")
'(error "<xml>hello")
> ((both parse-html-tag parse-plain-char) "<html> hello")
'(error "<html> hello")
|#
(define (both parser1 parser2)
  (lambda (str)
    (let* ([result1 (parser1 str)]
           [char1 (first result1)]
           [rest1 (second result1)])
      
      (if (parse-success result1)
          
          (let* ([result2 (parser2 rest1)]
                 [char2 (first result2)]
                 [rest2 (second result2)])
            
            (if (parse-success result2)
                (list (list char1 char2) rest2)
            (list 'error str)))
          
          (list 'error str)))))


#|
(star parser)

Returning:
  '('(all-parsed-part) "rest-of-string")

  Return a new parser that tries to parse using parser
  0 or more times, returning as its data a list of *all*
  parsed values. This new parser should be *greedy*: it
  always uses the input parser as many times as it can,
  until it reaches the end of the string or gets an error.

  Note that the new parser never returns an error; even if
  the first attempt at parsing fails, the data returned
  is simply '().

> ((star parse-plain-char) "hi")
'((#\h #\i) "")
> ((star parse-plain-char) "hi there")
'((#\h #\i) " there")
> ((star parse-plain-char) "<html>hi")
'(() "<html>hi")
|#
(define (star parser)
  (lambda (str)
    (star-recur parser str empty)))

(define (star-recur parser str lst)
  (if (equal? str "")
      (list lst str)
      (let* ([result1 (parser str)]
             [head1 (first result1)]
             [rest1 (second result1)])
        (if (equal? head1 'error)
            (list lst str)
            (star-recur parser rest1 (append lst (list head1))) ))))
            

#| HTML Parsing |#


#|
(parse-html str)

  Parse HTML content at the beginning of str, returning (list data rest),
  where data is the tree representation of the parsed HTML specified in the
  assignment handout, and rest is the rest of str that has not been parsed.

  If the string does not start with a valid html string, return
  (list 'error str) instead.

> (parse-html "<html><body class=\"hello\" >Hello, world!</body></html> Other")
'(("html"
   ()
   ("body"
    (("class" "hello"))
    "Hello, world!"))
  " Other")
> (parse-html "<blAh></blAh>")
'(("blAh"
   ()
   "")
  "")
> (parse-html "<body><p>Not good</body></p>")
'(error "<body><p>Not good</body></p>")
|#
(define (parse-html str) (void))



;--------------------Helper functions--------------------

#|

Some Pesudo Code
(define (parse-html str)
  (element-parser str))

(define (element-parser str)
  (open-parser str)
  (either text-parser (star element-parser))
  (close-parser str))

(define (open-parser str)(void))
(define (close-parser str)(void))

|#

#|
|#

(define parse-open-bracket (make-text-parser "<"))
(define parse-close-bracket (make-text-parser ">"))


#|
(parse-sucess lst)

Returning:
  #t
  #f

  Return True if parsing was successful. False elsewise.
    - Parsing failed when the first element is 'error.
 
|#
(define (parse-success lst)
  (if (equal? (first lst) 'error)
      #f
      #t))


#|
(apply-parse-char parser)

Returning:
  function

  Parsing a higher order function that parse characters continously and combine them into string

>(define f (apply-parse-char parse-non-special-char))
>(f "  This is a header </header>")
'("  This is a header " "</header>")
|#
(define (apply-parse-char parse-char)
  (lambda (str)
    (let ([result ((star parse-char) str)])
      (list-set result 0 (apply string (first result))))))

      
#|
(parse-non-special-text str)

Returning:
  '("parsed-string" "rest-of-str")

  Parsing a long text that only contains non-special characters.
  Returning a list including its parsed text and the rest of string.

>(parser-non-special-text "  This is a header </header>")
'("  This is a header " "</header>")
|#
(define (parse-non-special-text str)
  ((apply-parse-char parse-non-special-char) str))

#|
(parse-plain-text str)

Returning:
  '("parsed-string" "rest-of-str")

  Parsing a long text that only contains non-special characters.
  Returning a list including its parsed text and the rest of string.

>(parser-plain-text "Text </header>")
'("Text" " </header>")
|#
(define (parse-plain-text str)
  ((apply-parse-char parse-plain-char) str))

#|
(parse-open-tag str)

Returning:
  '('("tag" '(pairs of attributes)) "rest-of-str")
  '('error "str")

  Parsing the opening tag of a string.
  Returning a list containing the tag information and the rest of string,
  Tag information(list) is consisted of the tag name and a list of attributes.
  Returning '(error str) if parsing failed.

>(parse-open-tag "  <tag1>text </tag1>")
'(("tag1" ()) "text </tag1>")
>(parse-open-tag " <p id ="main" class= "super">Hey</p>")
'(("p" '(("class" "main") ("id" "me "))) "Hey</p>")
>(parse-open-tag "  </tag2> error </tag1>"
'('error "  </tag2> error </tag1>")
|#

(define (parse-open-tag str)
  ;Parse "<"
  (let* ([str1 (string-trim str #:right? #f)];Trim the white space on the left
         [lst1 (parse-open-bracket str1)])
    ;Parse opening tag
    (if (parse-success lst1)
        (let* ([str2 (second lst1)]
               [lst2 (parse-plain-text str2)];result of tag and rest-str
               [tag (first lst2)];opening tag
               ;Parse Attributes
               [str3 (second lst2)];TODO : ATTRIBUTES PARSE
               ;Parse ">"
               [str4 (string-trim str3 #:right? #f)];Trim white space before ">"
               [rest-str (string-trim str4 ">" #:right? #f)]);Trim ">"
          ;Combine them and return
          (list (list tag empty) rest-str))
        (list 'error str))))





























