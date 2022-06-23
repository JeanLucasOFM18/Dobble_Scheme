#lang racket

; FUNCION 1 (LISTA PERO FALTAN RESTRICCIONES)
(define cardsSet
  (lambda (Elements numE maxCards)
    (if (equal? maxCards -1)
        (list (createDeck Elements numE) Elements numE maxCards)
        (list (cutDeck (createDeck Elements numE) maxCards) Elements numE maxCards))))

; GETTERS
(define getDeck
  (lambda (cardsSet)
    (car cardsSet)))

(define getElements
  (lambda (cardsSet)
    (car (cdr cardsSet))))

(define getNumE
  (lambda (cardsSet)
    (car (cdr (cdr cardsSet)))))

(define getMaxC
  (lambda (cardsSet)
    (car (cdr (cdr (cdr cardsSet))))))

(define calculo
  (lambda (numElementsPerCard)
    (+ (+ (sqr (- numElementsPerCard 1)) (- numElementsPerCard 1)) 1)))

(define createDeck
  (lambda (Elements numE)
    (append (list (firstCard Elements numE 0))(append (nextCards Elements numE 1 0)(lastCards Elements numE 0 0 0)))))

(define firstCard
  (lambda (Elements numE aux)
    (if (equal? numE aux)
        '()
        (cons (car Elements) (firstCard (cdr Elements) numE (+ aux 1))))))

(define nextCards
  (lambda (Elements numE aux aux2)
    (if (equal? numE aux)
        '()
        (if (equal? numE aux2)
            (nextCards Elements numE (+ aux 1) 0)
            (cons (createCard Elements numE aux aux2 0) (nextCards Elements numE aux numE))))))

(define lastCards
  (lambda (Elements numE aux aux2 aux3)
    (if (equal? (- numE 1) aux)
        '()
        (if (equal? (- numE 1) aux2)
            (lastCards Elements numE (+ aux 1) 0 0)
            (if (equal? numE aux3)
                (lastCards Elements numE aux (+ aux2 1) 0)
                (cons (createLastCards Elements numE aux aux2 aux3 0) (lastCards Elements numE aux aux2 numE)))))))
    
(define createCard
  (lambda (Elements numE aux aux2 aux3)
    (if (equal? (- numE 1) aux2)
        '()
        (if (equal? aux3 0)
            (cons (getElemento Elements 0) (createCard Elements numE aux aux2 1))
            (cons (getElemento Elements (+ (* (- numE 1) aux) (+ aux2 1))) (createCard Elements numE aux (+ aux2 1) aux3))))))

(define createLastCards
  (lambda (Elements numE aux aux2 aux3 aux4)
    (if (equal? aux3 (- numE 1))
        '()
        (if (equal? aux4 0)
            (cons (getElemento Elements (+ aux 1)) (createLastCards Elements numE aux aux2 aux3 1))
            (cons (getElemento Elements (+ (+ (+ (- numE 1) 1) (* (- numE 1) aux3)) (remainder (+ (* aux aux3) aux2) (- numE 1)))) (createLastCards Elements numE aux aux2 (+ aux3 1) aux4))))))
 
(define cutDeck
  (lambda (set maxC)
    (if (equal? maxC 0)
        '()
        (cons (car set) (cutDeck (cdr set) (- maxC 1))))))
     
(define getElemento
  (lambda (Elements num)
    (if (equal? num 0)
        (car Elements)
        (getElemento (cdr Elements) (- num 1)))))

; FUNCION 2 (LISTA)
(define dobble?
  (lambda (cardsSet)
    (if (equal? (length (getDeck cardsSet)) (calculo (length (car (getDeck cardsSet)))))
        (if (equal? #t (reviewDeck (getDeck cardsSet) (length (getDeck cardsSet)) 0 1))
            #t
            #f)
        #f)))

(define reviewDeck
  (lambda (cardsSet largo aux aux2)
    (if (equal? (- largo 1) aux)
        #t
        (if (equal? largo aux2)
            (reviewDeck cardsSet largo (+ aux 1) (+ aux 2))
            (if (equal? #t (compareCard (getElemento cardsSet aux) (getElemento cardsSet aux2) (length (car cardsSet)) 0 0 0 0))
                (reviewDeck cardsSet largo aux (+ aux2 1))
                #f)))))

(define compareCard
  (lambda (card1 card2 largo aux aux2 comparaciones total)
    (if (equal? aux largo)
        (if (equal? total 1)
            #t
            #f)
        (if (equal? aux2 largo)
            (if (equal? comparaciones 1)
                (compareCard card1 card2 largo (+ aux 1) 0 0 (+ total 1))
                (compareCard card1 card2 largo (+ aux 1) 0 0 total))
            (if (equal? (getElemento card1 aux)(getElemento card2 aux2))
                (compareCard card1 card2 largo aux (+ aux2 1) (+ comparaciones 1) total)
                (compareCard card1 card2 largo aux (+ aux2 1) comparaciones total))))))
           
; FUNCION 3 (LISTA, REVISAR USO DE LENGTH)
(define numCards
  (lambda (cardsSet)
    (length (getDeck cardsSet))))
        
; FUNCION 4 (LISTA)
(define nthCard
  (lambda (cardsSet Entero)
    (findCard (getDeck cardsSet) Entero)))

(define findCard
  (lambda (Deck Entero)
    (if (equal? Entero 0)
        (car Deck)
        (findCard (cdr Deck) (- Entero 1)))))

; FUNCION 5 (LISTA, REVISAR USO DE LENGTH)
(define findTotalCards
  (lambda (card)
    (calculo (length card))))

; FUNCION 6 (LISTA, REVISAR USO DE LENGTH)
(define requiredElements
  (lambda (card)
    (calculo (length card))))

; FUNCION 7 (LISTA)
(define missingCards
  (lambda (cardsSet)
    (compareDecks (createDeck (getElements cardsSet) (getNumE cardsSet)) (getDeck cardsSet) (calculo (getNumE cardsSet)) (length (getDeck cardsSet)) 0 0)))

(define compareDecks
  (lambda (deck1 deck2 largo1 largo2 aux aux2)
    (if (equal? largo1 aux)
        '()
        (if (equal? largo2 aux2)
            (cons (getElemento deck1 aux)(compareDecks deck1 deck2 largo1 largo2 (+ aux 1) 0))
            (if (equal? #t (compareCard2 (getElemento deck1 aux) (getElemento deck2 aux2) (length (getElemento deck1 0)) 0 0 0 0))
                (compareDecks deck1 deck2 largo1 largo2 (+ aux 1) 0)
                (compareDecks deck1 deck2 largo1 largo2 aux (+ aux2 1)))))))

(define compareCard2
  (lambda (card1 card2 largo aux aux2 comparaciones total)
    (if (equal? aux largo)
        (if (equal? total 3)
            #t
            #f)
        (if (equal? aux2 largo)
            (if (equal? comparaciones 1)
                (compareCard2 card1 card2 largo (+ aux 1) 0 0 (+ total 1))
                (compareCard2 card1 card2 largo (+ aux 1) 0 0 total))
            (if (equal? (getElemento card1 aux)(getElemento card2 aux2))
                (compareCard2 card1 card2 largo aux (+ aux2 1) (+ comparaciones 1) total)
                (compareCard2 card1 card2 largo aux (+ aux2 1) comparaciones total))))))

; FUNCION 8 (NO TERMINADA)
(define cardsSet->String
  (lambda ()
    '()))

; FUNCIÓN 9
(define game
  (lambda (numPlayers cardsSet mode)
    (list numPlayers cardsSet mode '() '() '() '() '())))

; FUNCIÓN 10
(define stackMode
  (lambda ()
    '()))

(define elementsSet (list "A" "B" "C" "D" "E" "F" "G"))
(define numPlayers 2)
(define numElementsPerCard 3)
(define maxCards 5)

; PRUEBAS SCRIPTS
(define dobbleSet0 (cardsSet elementsSet numElementsPerCard maxCards))
(define dobble?0 (dobble? dobbleSet0))
(define numCards0 (numCards dobbleSet0))
(define nthCard0 (nthCard dobbleSet0 3))
(define findTotalCards0 (findTotalCards (nthCard dobbleSet0 1)))
(define requiredElements0 (requiredElements (nthCard dobbleSet0 1)))
(define missingCards0 (missingCards dobbleSet0))

(define dobbleSet1 (cardsSet elementsSet numElementsPerCard -1))
(define game1 (game numPlayers dobbleSet1 stackMode))


; PRUEBAS PERSONALES
(define getDeck0 (getDeck dobbleSet0))
(define getElements0 (getElements dobbleSet0))
(define getNumE0 (getNumE dobbleSet0))
(define getMaxC0 (getMaxC dobbleSet0))
(define calculo1 (calculo numElementsPerCard))
;(define crear (createDeck elementsSet numElementsPerCard))
;(define obtener (getElemento elementsSet 3))
;(define crear2 (nextCards elementsSet numElementsPerCard 1 0))
(define pruebas (list (list 1 2) (list 3 4)))
;(define crear3 (lastCards elementsSet numElementsPerCard 0 0 0))
;(define dobble?1 (dobble? (list (list "A" "B" "C")(list "A" "B" "C")(list "A" "D" "E")(list "A" "F" "G")(list "B" "E" "G")(list "B" "F" "G")(list "C" "D" "G"))))