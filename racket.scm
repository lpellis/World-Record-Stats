#lang scheme
(require net/url)
(require (planet neil/html-parsing:2:0))
(require plot)
(require racket/list)

(define (buck l)
  (if (empty? l)
      empty
      (cons (vector (cadar l) (caar l)) (buck (cdr l)))))

(define (plot-list l title)  
  (plot
   (list
    (discrete-histogram 
     (buck l))) #:width 1200 #:y-min 9 #:x-label "Athlete" #:y-label "Time (s)" #:title title))
; range function
(define (rangex low high)
  (if (> low high)
      empty
      (cons low (range (+ low 1) high))))

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


(define (match element attributename attributevalue)
  (if (empty? element)
      #f
      (let ([v (assoc attributename (cdr (car element)))])
        (if (not v)
            #f
            (begin (or (equal? attributevalue "") (equal? attributevalue (car (cdr v)))))
            ))))

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
; assoc function that returns all keys
(define (assoc-all key alist)   
  (if (not (list?  alist))
      empty
      (if (empty? alist)
          empty
          (if (not (list? (car alist)))
              empty
              (let ([val (myassoc key alist)])
                (if (not val)
                    empty
                    (append (list val)
                            (assoc-all key (remove val alist)))))))))

;assoc function that does not bomb on invalid input
(define (myassoc key l)
  (if (empty? l)
      #f
      (if (list? (car l))
          (if (eqv? key (car (car l)))
              (car l)
              (myassoc key (cdr l)))
          #f
          )))

(define (get-results-from-url url)
  (let ([xtags (html->xexp (download-url url))])    
    (let ([result (traverse xtags 'div 'id "TLdetails")])   
      (let ([tr_results (traverse result 'tr 'class "")])
        (parse_results tr_results)
        ))))

(define (parse-number s)
  (begin 
    (let ([rmhh (regexp-match "(.*):(.*):(.*):(.*)"  (regexp-replace* #rx"\\." s ":"))]) ;hour with decimal eg 1:28:36.6
      (if rmhh
          (+ (* 3600 (string->number (cadr rmhh))) (* 60 (string->number (caddr rmhh))) (string->number(cadddr rmhh))  (* 0.1 (string->number(cadddr (cdr rmhh)))))
          
          (let ([rmh (regexp-match "(.*):(.*):(.*)"  (regexp-replace* #rx"\\." s ":"))]) ;hour eg 1:28:36
            (if rmh
                (+ (* 3600 (string->number (cadr rmh))) (* 60 (string->number (caddr rmh))) (string->number(cadddr rmh)))
                (let ([rmm (regexp-match "(.*):(.*)" s)]) ;minutes eg 23:46
                  (if rmm              
                      (+ (* 60 (string->number (cadr rmm))) (string->number(caddr rmm)))
                      (string->number s)))))))))

(define (parse_results r)
  (if (empty? r)
      empty
      (begin (let ([timet (cadar (cddddr (car r)))])
               (let ([athlete (caddar (traverse (car r) 'a 'class "athLink"))])
                 (let ([date (car (cddadr (traverse (car r) 'td 'class "c")))])
                   (let ([country (cddar (traverse (car r) 'td 'class "l"))])
                     (cons (list (parse-number (car (regexp-match #rx"[0-9:.]*" timet)))  athlete date) (parse_results (cdr r))))))))))


(define (find-name name l)
  (begin 
  (if (empty? l)
      #f
      (if (equal? name (cadr (car l)))
          #t
          (find-name name (cdr l))))))
(define (make-unique l)
  (make-unique-intermediate l empty))

(define (make-unique-intermediate times results)
  (if (empty? times)
      results
      (if (find-name (cadr (car times)) results)
          (make-unique-intermediate (cdr times) results)
          (make-unique-intermediate (cdr times) (append results (list (car times)))))))
  

(define events '(("100" "100 Metres") ("200" "200 Metres") ("400" "400 Metres") ("800" "800 Metres") ("1000" "1000 Metres") 
                                      ("1500" "1500 Metres") ("MILE" "Mile") ("3000" "3000 Metres") ("5000" "5000 Metres") ("10K" "10,000 Metres")
                                      ("10RR" "10 Kilometres") ("15RR" "15 Kilometres") ("20RR" "20 Kilometres") ("HMAR" "Half Marathon") 
                                      ("25RR" "25 Kilometres") ("30RR" "30 Kilometres") ("MAR" "Marathon") ("100K" "100 Kilometres")
                                      ("3KSC" "3000 Metres Steeplechase") ("110H" "110 Metres Hurdles") ("400H" "400 Metres Hurdles") 
                                      ("HJ" "High Jump") ("PV" "Pole Vault") ("LJ" "Long Jump") ("TJ" "Tripple Jump") ("SP" "Shot Put")
                                      ("DT" "Discus Throw") ("HT" "Hammer Throw") ("JT" "Javelin Throw") ("20KW" "20,000 Metres Race Walk")
                                      ("20KR" "20 Kilometres Race Walk") ("50KR" "50 Kilometres Race Walk") ))

(define base-url "http://www.iaaf.org/statistics/toplists/inout=o/age=n/season=0/sex=M/all=y/legal=A/disc=_EVENT_/detail.html")
(define (loop-plot l)
  (begin 
    (if (empty? l)
        empty
        (cons (plot-list  (make-unique (take (get-results-from-url (regexp-replace #rx"_EVENT_" base-url (caar l))) 50))
                          (cadar l)) 
              (loop-plot (cdr l))))))

(define r (take (get-results-from-url (regexp-replace #rx"_EVENT_" base-url "100")) 50))



  


(make-unique r)
(loop-plot '(("100" "100 Metres")))
;(loop-plot events )