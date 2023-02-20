#lang racket

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.
(define (dot-product X Y)
  (if (null? X)
      0
      (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y)) ) ))
; TODO
; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.
(define (multiply-aux M v acc)
  (if (null? M)
      acc
      (multiply-aux (cdr M) v ( append acc (list (dot-product (car M) v))))
      ))
      
     
(define (multiply M V)
  (multiply-aux M V null)
  )
; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)
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
  


;(get-interval3 1 12)
;(get-height 64 0 0 3)
;(min-index 3 0 0 1)
;(max-index 1 0 0 1)

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
  (reverse (get-transf-helper n (get-height n 1 0 3) 0 (min-index (get-height n 1 0 3) 0 0 1) (max-index (get-height n 1 0 3) 0 0 1) null) )
  )
; TODO
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.
(define (apply-matrix-transformations Ts ppt)
  (if (null? Ts)
      ppt
      (cond
        ( (= (car Ts) 1) (apply-matrix-transformations (cdr Ts) (multiply T1 ppt)))
        ( (= (car Ts) 2) (apply-matrix-transformations (cdr Ts) (multiply T2 ppt)))
        ( (= (car Ts) 3) (apply-matrix-transformations (cdr Ts) (multiply T3 ppt)))
  )
  )
)

; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) '(3 4 5)) 
  )

