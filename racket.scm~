#lang racket
(require net/url)
(require (planet neil/html-parsing:2:0))

(define (range low high)
  (if (> low high)
      empty
      (cons low (range (+ low 1) high))))
 
(define (reduce f v)
  (cond
     [(empty? (cdr v)) (car v)]
     [else (f (car v) (reduce f (cdr v)))]))

;download a url to a string
(define (download-url url)
  (define in (get-pure-port (string->url url) ))
  (define response-string (port->string in))
  (close-input-port in)
  response-string)

(define example-html 
  (string-append "<html><head><title></title><title>whatever</title></head>"
    "<body> <a href=\"url\">link</a><div class='bla' id='outer'><p align=center id='theid'>"
    "<ul compact style=\"aa\"> <p>BLah<!-- comment <comment> -->"
    " <i> italic <b> bold <div id='inner' class='bla'>some inner content that i want</div> ened</i> still &lt; bold </b>"
    "</body><P> But not<p>nested</p> done yet...</p></div>"))


(define k_temp (html->xexp (download-url "http://www.iaaf.org/statistics/toplists/inout=o/age=n/season=2012/sex=M/all=n/legal=A/disc=100/detail.html")))
;(define k_temp (html->xexp example-html))
(define k  k_temp)

;implementation of flatten
(define (flat k)
  (if (list? k)      
      (append (flat (car k))
              (if (empty? (cdr k))
                  empty
                  (flat (cdr k))))
      (list k)))

(define (match element attributename attributevalue)
  (if (empty? element)
      #f
      (let ([v (assoc attributename (cdr (car element)))])
        (if (not v)
            #f
             (begin (equal? attributevalue (car (cdr v))))
            )
        )      
      )
)

(define (traverse k htmlelement attributename attributevalue)
  (if (empty? k)
      empty
      (if (list? k)
          (begin 
            (if (and (eqv? htmlelement (car k)) (match (assoc-all '@ (cdr k)) attributename attributevalue))
                (begin  (append (list k) (traverse (cdr k) htmlelement attributename attributevalue)))
                (append (traverse (car k) htmlelement attributename attributevalue) (traverse (cdr k) htmlelement attributename attributevalue))))
          empty
          )))


(define temp (list 1 2 (list 3 4) (list 5 (list 6) 7) 8))
(define t '((div         (@          (id "linkgp08")          (class "minisiteLink")          (style "display:none;")          (onclick "javascript:location.href=/gp08/index.html")          (title "GP08 Home"))         " ")        (div         (@          (id "linkwrc08")          (class "minisiteLink")          (style "display:none;")          (onclick "javascript:location.href=/wrc08/index.html")          (title "WRC08 Home"))         " ")))

(define (assoc-all key alist)   (if (not (list?  alist))
      empty
      (if (not (list? (car alist)))
          empty
          (let ([val (myassoc key alist)])
            (if (not val)
                empty
                (append (list val)
                        (assoc-all key (remove val alist))))))))

;assoc function that does not bomb on invalid input
(define (myassoc key l)
  (if (empty? l)
      #f
      (if (list? (car l))
          (if (eqv? key (car (car l)))
              (car l)
              (myassoc key (cdr l)))
          #f
          ))
  )


(define assoctest '((a (1 2)) (2 "hi") (b 2) 5) )

(define result (traverse k 'div 'id "TLdetails"))
(define tr_results (traverse result 'tr 'class " on"))

(define (parse_results r)
  (if (empty? r)
      empty
      (begin (let ([timet (cadar (cddddr (car r)))])
               (let ([athlete (caddar (traverse (car r) 'a 'class "athLink"))])
                 (cons (list timet athlete) (parse_results (cdr r)))))))
  )
(parse_results tr_results)
;(define result2 (traverse result '@ 'id 'id))
;(define test (list '(@ (class "bla") (id "outer"))'(@ (id "inner"))'(b (id "bb"))))
;result