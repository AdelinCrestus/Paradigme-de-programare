#lang racket

(provide (all-defined-out))

;; Același arbore de TPP obținut în etapa 1 prin aplicarea
;; transformărilor T1, T2, T3 poate fi generat folosind 
;; tupluri GH (Gopal-Hemachandra).
;;
;; Pentru o pereche oarecare (g, e), secvența GH este:
;;    g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;; Pentru (g, e) = (1, 1) obținem șirul lui Fibonacci.
;;
;; Primele 4 numere din secvență formează cvartetul GH:
;;    (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;; Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; și putem demonstra că (a,b,c) este un triplet pitagoreic.
;;
;; (a,b,c) este chiar TPP, dacă adăugăm condițiile:
;;    g, e, f, h prime între ele
;;    g impar
;; însă nu veți avea nevoie să faceți asemenea verificări,
;; întrucât avem la dispoziție un algoritm care generează
;; exclusiv TPP.
;;
;; Acest algoritm este foarte asemănător cu cel din etapa
;; anterioară, cu următoarele diferențe:
;;  - nodurile din arbore sunt cvartete, nu triplete
;;    (din cvartet obținem un TPP conform formulelor)
;;    (ex: (1,1,2,3) => (1*3,2*1*2,1^2+2^2) = (3,4,5))
;;  - obținem următoarea generație de cvartete folosind 
;;    trei transformări Q1, Q2, Q3 pentru cvartete, în loc
;;    de T1, T2, T3 care lucrau cu triplete
;; 
;; Q1(g,e,f,h) = (h,e,h+e,h+2e)
;; Q2(g,e,f,h) = (h,f,h+f,h+2f) 
;; Q3(g,e,f,h) = (g,f,g+f,g+2f)
;;
;; Arborele rezultat arată astfel:
;;
;;                        (1,1,2,3)
;;              ______________|______________
;;             |              |              |
;;         (3,1,4,5)      (3,2,5,7)      (1,2,3,5)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;;  (5,1,6,7) .........................................

;; Definim funcțiile Q1, Q2, Q3:
(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Reimplementați funcția care calculează produsul scalar
; a doi vectori X și Y, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
(define (dot-product X Y)
  (foldl + 0 (map * X Y))
  )
; TODO
; Reimplementați funcția care calculează produsul dintre
; o matrice M și un vector V, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|

(define (multiply M V)
   (foldr (λ (X acc) (cons (dot-product X V) acc) ) null M))
; TODO
; Aduceți aici (nu sunt necesare modificări) implementarea
; funcției get-transformations de la etapa 1.
; Această funcție nu este re-punctată de checker, însă este
; necesară implementărilor ulterioare.

(define (get-height n s h p)
  (if (>= s n)
      h
      (get-height n (+ s p) (+ h 1) (* p 3))
  
  )
  )

(define (min-index h i mi p)
  (if (>= i h)
      (+ mi 1)
      (min-index h (+ i 1) (+ mi p) (* p 3)) 
  ))

(define (max-index h i maxip p)
  (if (> i h)
      maxip
      (max-index h (+ i 1) (+ maxip p) (* p 3)
  )))

(define (get-interval-length min max)
  (/ (+ (- max min) 1) 3)
  )

(define (get-interval1 min max)
  (cons min (- (+ min (get-interval-length min max)) 1)
  ))

(define (get-interval2 min max)
  (cons (+ (cdr (get-interval1 min max)) 1)  (+ (cdr (get-interval1 min max)) (get-interval-length min max)))
  )

(define (get-interval3 min max)
  (cons (+ (cdr (get-interval2 min max)) 1) (+ (cdr (get-interval2 min max)) (get-interval-length min max)))
  )

(define (get-transf-helper n height c index-min index-max acc)
  (if (>= c height)
      acc
      (cond
        ( (and (<= n (cdr (get-interval1 index-min index-max))) (>= n (car (get-interval1 index-min index-max)))) (get-transf-helper n height (+ c 1) (car (get-interval1 index-min index-max)) (cdr (get-interval1 index-min index-max)) (append (list 1) acc))) 
        ( (and (<= n (cdr (get-interval2 index-min index-max))) (>= n (car (get-interval2 index-min index-max)))) (get-transf-helper n height (+ c 1) (car (get-interval2 index-min index-max)) (cdr (get-interval2 index-min index-max)) (append (list 2) acc)))
         ( (and (<= n (cdr (get-interval3 index-min index-max))) (>= n (car (get-interval3 index-min index-max)))) (get-transf-helper n height (+ c 1) (car (get-interval3 index-min index-max)) (cdr (get-interval3 index-min index-max)) (append (list 3) acc)))
        (else null)
      )
  )
)

(define (get-transformations n)
  (reverse (get-transf-helper n (get-height n 1 0 3) 0 (min-index (get-height n 1 0 3) 0 0 1) (max-index (get-height n 1 0 3) 0 0 1) null) ))


; TODO
; În etapa anterioară ați implementat o funcție care primea
; o listă Ts de tipul celei întoarsă de get-transformations
; și un triplet de start ppt și întorcea tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Acum dorim să generalizăm acest proces, astfel încât să
; putem reutiliza funcția atât pentru transformările de tip
; T1, T2, T3, cât și pentru cele de tip Q1, Q2, Q3.
; În acest scop operăm următoarele modificări:
;  - primul parametru este o listă de funcții Fs
;    (în loc de o listă numerică Ts)
;  - al doilea parametru reprezintă un tuplu oarecare
;    (aici modificarea este doar "cu numele", fără a schimba
;    funcționalitatea, este responsabilitatea funcțiilor din
;    Fs să primească parametri de tipul lui tuple)
; Nu folosiți recursivitate explicită (ci funcționale).
(define (apply-functional-transformations Fs tuple)
  (foldl (λ (x acc) (apply x (list acc)) ) tuple Fs)
  )

;apply-functional-transformations (list Q1 Q3) '(1 2 2 3))
; TODO
; Tot în spiritul abstractizării, veți defini o nouă funcție
; get-nth-tuple, care calculează al n-lea tuplu din arbore. 
; Această funcție va putea fi folosită:
;  - și pentru arborele de triplete (caz în care plecăm de la
;    (3,4,5) și avansăm via T1, T2, T3)
;  - și pentru arborele de cvartete (caz în care plecăm de la
;    (1,1,2,3) și avansăm via Q1, Q2, Q3)
; Rezultă că, în afară de parametrul n, funcția va trebui să
; primească un tuplu de start și 3 funcții de transformare a
; tuplurilor.
; Definiți get-nth-tuple astfel încât să o puteți reutiliza
; cu minim de efort pentru a defini funcțiile următoare:
;    get-nth-ppt-from-matrix-transformations
;    get-nth-quadruple
; (Hint: funcții curry)
; În define-ul de mai jos nu am precizat parametrii funcției
; get-nth-tuple pentru ca voi înșivă să decideți care este
; modul optim în care funcția să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție,
; dar asistentul va observa dacă implementarea respectă cerința.




;(define (map-transf F1 F2 F3 L)
; (map (λ(x) (cond
;               ((= x 1) (λ (y) (apply F1 y) ))
;               ((= x 2) (λ (y) (apply F2 y) ))
;               ((= x 3) (λ (y) (apply F3 y) ))
;               )) L))
;
;(define (map-matrix F1 F2 F3 L)
;  (map (λ(x) (cond
;               ((= x 1) (λ (y) (multiply F1 y)))
;               ((= x 2) (λ (y) (multiply F2 y)))
;               ((= x 3) (λ (y) (multiply F3 y))))) L))

;(map-transf Q1 Q2 Q3 '(2 1 3) )
; Am scris ambele functii pt a parametriza ce difera

(define (map-lista f)
  (λ (F1 F2 F3 L)
  (map (λ(x) (cond
               ((= x 1) (λ (y) (f F1 y)))
               ((= x 2) (λ (y) (f F2 y)))
               ((= x 3) (λ (y) (f F3 y)))
               )) L)))

(define (get-nth-tuple tuple F1 F2 F3 M)
  (λ (n) (apply-functional-transformations (M F1 F2 F3 (get-transformations n)) tuple))
)

(define map-matrix (map-lista multiply))
(define map-transf (map-lista apply))


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil
; (hint: aplicare parțială) o funcție care calculează al n-lea
; TPP din arbore, folosind transformările pe triplete.
(define get-nth-ppt-from-matrix-transformations (get-nth-tuple '(3 4 5) T1 T2 T3 map-matrix))

;(get-nth-ppt-from-matrix-transformations 3)


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil 
; (hint: aplicare parțială) o funcție care calculează al n-lea 
; cvartet din arbore, folosind transformările pe cvartete.
(define get-nth-quadruple (get-nth-tuple '(1 1 2 3) Q1 Q2 Q3 map-transf))
;(get-nth-quadruple 3)

; TODO
; Folosiți rezultatul întors de get-nth-quadruple pentru a 
; obține al n-lea TPP din arbore.
(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)

(define (helper n L)
  (list (* (first L) (fourth L)) (* 2 (second L) (third L)) (+ (sqr (second L)) (sqr (third L))))
  )
  
(define get-nth-ppt-from-GH-quadruples
  (λ (n) (helper n (get-nth-quadruple n))))
