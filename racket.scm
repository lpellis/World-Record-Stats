#lang scheme
(require net/url)
(require (planet neil/html-parsing:2:0))
(require plot)
(require racket/list)

(define (reduce f l)
  (if (empty? (cdr l))
      (car l)
  (f (car l) (reduce f (cdr l)))))

(define example-html 
  (string-append "<html><head><title></title><title>whatever</title></head>"
                 "<body> <a href=\"url\">link</a><div class='bla' id='outer'><p align=center id='theid'>"
                 "<ul compact style=\"aa\"> <p>BLah<!-- comment <comment> -->"
                 " <i> italic <b> bold <div id='inner' class='bla'>some inner content that i want</div> ened</i> still &lt; bold </b>"
                 "</body><P> But not<p>nested</p> done yet...</p></div>"))

(define events '(("100" "100 Metres") ("200" "200 Metres") ("400" "400 Metres") ("800" "800 Metres") ("1000" "1000 Metres") 
                                      ("1500" "1500 Metres") ("MILE" "Mile") ("3000" "3000 Metres") ("5000" "5000 Metres") ("10K" "10,000 Metres")
                                      ("10RR" "10 Kilometres") ("15RR" "15 Kilometres") ("20RR" "20 Kilometres") ("HMAR" "Half Marathon") 
                                      ("25RR" "25 Kilometres") ("30RR" "30 Kilometres") ("MAR" "Marathon") 
                                      ("3KSC" "3000 Metres Steeplechase") ("110H" "110 Metres Hurdles") ("400H" "400 Metres Hurdles") 
                                    ;  ("HJ" "High Jump") ("PV" "Pole Vault") ("LJ" "Long Jump") ("TJ" "Tripple Jump") ("SP" "Shot Put")
                                    ;  ("DT" "Discus Throw") ("HT" "Hammer Throw") ("JT" "Javelin Throw") 
                                      ("20KR" "20 Kilometres Race Walk") ("50KR" "50 Kilometres Race Walk") ))

(define base-url "http://www.iaaf.org/statistics/toplists/inout=o/age=n/season=0/sex=M/all=y/legal=A/disc=_EVENT_/detail.html")

; Calculate the Standard Deviation of values in list
;     __________________________________________
;    /
;   /(x1 - u)^2 + (x2 - u)^2 + ... + (xn - u)^2
;  / ------------------------------------------
;\/                  n - 1
(define (std-dev l)
  (let ([u (/ (foldl + 0 l) (length l))]) ;the avg value in l
    (let ([k (map (lambda (number) (- number u)) l)]) ;(x1 -u), (x2 - u) ...
      (let ([square-list (map (lambda (number) (* number number)) k)]) ;(x1 - u)^2, (x2 - u)^2 ...
        (let ([s (foldl + 0 square-list)])  
          (expt (/ s (- (length l) 1)) 0.5))))))
  

(define (buck l)
  (if (empty? l)
      empty
      (cons (vector (cadar l) (caar l)) (buck (cdr l)))))

(define (plot-list l title y-label)  
  (begin 
  (let ([y-values (map (lambda (entry) (car entry)) l)])
    (let ([max-y (reduce max y-values)])
      (let ([min-y (reduce min y-values)])
        (plot
         (list  
          (discrete-histogram 
           (buck l) #:color 1) (discrete-histogram 
                                (buck (take l 1)) #:color 2) )
         #:out-file (string-append title ".png")
         #:width 1200 
         #:y-min (max 0 (- min-y (* 0.33 (- max-y min-y))))
         #:x-label "Athlete"
         #:y-label y-label
         #:title title ))))))

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

;check if the given element matches the attributes (jquery like)
(define (match element attributename attributevalue)
  (if (empty? element)
      #f
      (let ([v (assoc attributename (cdr (car element)))])
        (if (not v)
            #f
            (begin (or (equal? attributevalue "") (equal? attributevalue (car (cdr v)))))))))

(define (traverse k htmlelement attributename attributevalue)
  (if (empty? k)
      empty
      (if (list? k)
          (begin 
            (if (and (eqv? htmlelement (car k)) (match (assoc-all '@ (cdr k)) attributename attributevalue))
                (begin  (append (list k) (traverse (cdr k) htmlelement attributename attributevalue)))
                (append (traverse (car k) htmlelement attributename attributevalue) (traverse (cdr k) htmlelement attributename attributevalue))))
          empty)))

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
        (parse_results tr_results)))))

; converts strings such as 1:12.23 , 1:23 , 1:12:43.2 to number of seconds
(define (parse-number s)
  (begin 
    (let ([rmhh (regexp-match "(.*):(.*):(.*):(.*)"  (regexp-replace* #rx"\\." s ":"))]) ;hour with decimal eg 1:28:36.6
      (if rmhh
          (+ (* 3600 (string->number (cadr rmhh))) (* 60 (string->number (caddr rmhh))) (string->number(cadddr rmhh))  (* 0.1 (string->number(cadddr (cdr rmhh)))))
          
          (let ([rmms (regexp-match "(.*):(.*\\.[0-9]*)"  s )]) ;hour eg 1:28:36
            (if rmms  
                (+ (* 60 (string->number (cadr rmms))) (string->number (caddr rmms)))
                (let ([rmh (regexp-match "(.*):(.*):(.*)"  (regexp-replace* #rx"\\." s ":"))]) ;hour eg 1:28:36
                  (if rmh
                      (+ (* 3600 (string->number (cadr rmh))) (* 60 (string->number (caddr rmh))) (string->number(cadddr rmh)))
                      (let ([rmm (regexp-match "(.*):(.*)" s)]) ;minutes eg 23:46
                        (if rmm              
                            (+ (* 60 (string->number (cadr rmm))) (string->number(caddr rmm)))
                            (string->number s)))))))))))

; parses out the actual times, athlete name from the html-like list structure
(define (parse_results r)
  (if (empty? r)
      empty
      (begin (let ([timet (cadar (cddddr (car r)))])
               (let ([athlete (caddar (traverse (car r) 'a 'class "athLink"))])
                 (let ([date (car (cddadr (traverse (car r) 'td 'class "c")))])
                   (let ([country (cddar (traverse (car r) 'td 'class "l"))])
                     (cons (list (parse-number (car (regexp-match #rx"[0-9:.]*" timet)))  athlete date) (parse_results (cdr r))))))))))

; checks if a given athlete name is in the list (find-name "Usain Bolt" l) => #t 
(define (find-name name l)
  (begin 
  (if (empty? l)
      #f
      (if (equal? name (cadr (car l)))
          #t
          (find-name name (cdr l))))))

; keeps only the best time of each athlete in list
(define (make-unique l)
  (define (make-unique-intermediate times results)
    (if (empty? times)
        results
        (if (find-name (cadr (car times)) results)
            (make-unique-intermediate (cdr times) results)
            (make-unique-intermediate (cdr times) (append results (list (car times)))))))
  (make-unique-intermediate l empty))

(define (loop-plot l)
  (begin 
    (if (empty? l)
        empty
        (let ([r (take (make-unique (get-results-from-url (regexp-replace #rx"_EVENT_" base-url (caar l)))) 12)])
          (append (list (plot-list r (cadar l) "Time (s)")) (list (plot-list (diff-list r) (string-append (cadar l) " % Improvement over next best athlete") "% Improvement"))
                (loop-plot (cdr l)))))))
  
(define r (take (make-unique (get-results-from-url (regexp-replace #rx"_EVENT_" base-url "400"))) 50))

(define (diff-list l)
  (if (empty? (cdr l))
      empty
      (cons (append (list(* 100 (/ (- (caar (cdr l)) (caar l)) (caar (cdr l)))) ) (cdar l)) (diff-list (cdr l)))))
  


;(loop-plot '(("50KW" "5000 Metres")))
(loop-plot events )