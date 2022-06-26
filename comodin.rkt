#lang racket

; FUNCION 1 (LISTA PERO FALTAN RESTRICCIONES)
(define cardsSet
  (lambda (Elements numE maxCards)
    (if (< (length (eliminaRepetidos Elements 0 0 (repetidos (detectaRepetidos Elements 0 1) 0 0 '() 1) 0)) (calculo numE))
        #f
        (if (equal? maxCards -1)
            (list (aleatorizar (createDeck (aleatorizar (eliminaRepetidos Elements 0 0 (repetidos (detectaRepetidos Elements 0 1) 0 0 '() 1) 0)) numE)) (eliminaRepetidos Elements 0 0 (repetidos (detectaRepetidos Elements 0 1) 0 0 '() 1) 0) numE maxCards)
            (list (cutDeck (aleatorizar (createDeck (aleatorizar (eliminaRepetidos Elements 0 0 (repetidos (detectaRepetidos Elements 0 1) 0 0 '() 1) 0)) numE)) maxCards) (eliminaRepetidos Elements 0 0 (repetidos (detectaRepetidos Elements 0 1) 0 0 '() 1) 0) numE maxCards)))))

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

(define detectaRepetidos
  (lambda (Elementos aux aux2)
    (if (equal? (length Elementos) aux)
        '()
        (if (equal? (length Elementos) aux2)
            (detectaRepetidos Elementos (+ aux 1) (+ aux 2))
            (if (equal? (getElemento Elementos aux) (getElemento Elementos aux2))
                (cons (getElemento Elementos aux)(detectaRepetidos Elementos aux (+ aux2 1)))
                (detectaRepetidos Elementos aux (+ aux2 1)))))))

(define repetidos
  (lambda (listaRepetidos aux aux2 listaFinal habilitador)
    (if (equal? (length listaRepetidos) aux)
        listaFinal
        (if (equal? (length listaFinal) aux2)
            (if (equal? habilitador 1)
                (repetidos listaRepetidos (+ aux 1) 0 (append listaFinal (list (getElemento listaRepetidos aux))) 1)
                (repetidos listaRepetidos (+ aux 1) 0 listaFinal 1))
            (if (equal? (getElemento listaRepetidos aux)(getElemento listaFinal aux2))
                (repetidos listaRepetidos aux (+ aux2 1) listaFinal 0)
                (repetidos listaRepetidos aux (+ aux2 1) listaFinal habilitador))))))
           
(define eliminaRepetidos
  (lambda (Elementos aux aux2 repetidos habilitador)
    (if (equal? (length Elementos) aux)
        repetidos
        (if (equal? (length repetidos) aux2)
            (if (equal? habilitador 0)
                (cons (getElemento Elementos aux)(eliminaRepetidos Elementos (+ aux 1) 0 repetidos 0))
                (eliminaRepetidos Elementos (+ aux 1) 0 repetidos 0))
            (if (equal? (getElemento Elementos aux) (getElemento repetidos aux2))
                (eliminaRepetidos Elementos aux (+ aux2 1) repetidos 1)
                (eliminaRepetidos Elementos aux (+ aux2 1) repetidos habilitador))))))

(define aleatorizar
  (lambda (Elements)
    (if (null? Elements)
        '()
        (cons (getElemento (reverse Elements) 0) (aleatorizar (cdr (reverse Elements)))))))
     
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

; FUNCIÓN 9 (AVANZANDO)
(define game
  (lambda (numPlayers cardsSet mode)
    (list numPlayers cardsSet mode "No iniciado" '() 0 '() '())))

; GETTERS
(define getNumPlayers
  (lambda (game)
    (car game)))

(define getCardsSet
  (lambda (game)
    (car (cdr game))))

(define getMode
  (lambda (game)
    (car (cdr (cdr game)))))

(define getStatus
  (lambda (game)
    (car (cdr (cdr (cdr game))))))

(define getTable
  (lambda (game)
    (car (cdr (cdr (cdr (cdr game)))))))

(define getTurn
  (lambda (game)
    (car (cdr (cdr (cdr (cdr (cdr game))))))))

(define getPlayers
  (lambda (game)
    (car (cdr (cdr (cdr (cdr (cdr (cdr game)))))))))

(define getScores
  (lambda (game)
    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr game))))))))))

; FUNCIÓN 10 (LISTA)
(define stackMode
  (lambda (cardsSet)
    (list (car (reverse cardsSet))(car (cdr (reverse cardsSet))))))

; FUNCIÓN 11 (LISTA)
(define register
  (lambda (user game)
    (if (equal? (length (getPlayers game)) (getNumPlayers game))
        game
        (if (equal? #t (verificarName user game (length (getPlayers game)) 0))
            (cons (getNumPlayers game)(cons (getCardsSet game)(cons (getMode game)(cons (getStatus game) (cons (getTable game) (cons (getTurn game)(cons (agregateName user game)(list (agregateScore game)))))))))
            game))))

(define verificarName
  (lambda (user game aux aux2)
    (if (equal? aux aux2)
        #t
        (if (equal? user (getElemento (getPlayers game) aux2))
            #f
            (verificarName user game aux (+ aux2 1))))))

(define agregateName
  (lambda (user game)
    (cons user (getPlayers game))))

(define agregateScore
  (lambda (game)
    (cons 0 (getScores game))))

; FUNCIÓN 12 (LISTA)
(define whoseTurnIsIt?
  (lambda (game)
    (getElemento (getPlayers game) (getTurn game))))

; FUNCIÓN 13 PLAY
(define play
  (lambda (game action)
    (if (equal? (length game) 9)
        #f
        (if (equal? action null)
            (cons (getNumPlayers game)(cons (getCardsSet game) (cons (getMode game) (cons "En juego" (cons ((getMode game) (car (getCardsSet game))) (cons (getTurn game) (cons (getPlayers game)(list (getScores game)))))))))
            (if (equal? action pass)
                (cons (getNumPlayers game)(cons (getCardsSet game) (cons (getMode game) (cons "En juego" (cons (getTable game) (cons (pass (getTurn game) (getNumPlayers game)) (cons (getPlayers game)(list (getScores game)))))))))
                (if (equal? action finish)
                    (cons (getNumPlayers game)(cons (getCardsSet game) (cons (getMode game) (cons "Terminado" (cons (getTable game) (cons (getTurn game) (cons (getPlayers game) (cons (getScores game) (list (finish (getScores game)(getPlayers game)))))))))))
                    (if (equal? #t (verificarComparacion ((getMode game) (car (getCardsSet game))) action (car (cdr (getCardsSet game))) 0 0))
                        (cons (getNumPlayers game)(cons (cons (retirarCartas (car (getCardsSet game))) (cdr (getCardsSet game))) (cons (getMode game)(cons "En juego"(cons '() (cons (pass (getTurn game) (getNumPlayers game))(cons (getPlayers game)(list (sumaPuntaje (getScores game)(getTurn game)(length (getScores game)) 0)))))))))
                        (cons (getNumPlayers game)(cons (cons (devolverCartas ((getMode game) (car (getCardsSet game))) (car (getCardsSet game))) (cdr (getCardsSet game))) (cons (getMode game)(cons "En juego" (cons '() (cons (pass (getTurn game) (getNumPlayers game))(cons (getPlayers game) (list (getScores game))))))))))))))))

(define spotit
  (lambda (coincidencia)
    coincidencia))

(define verificarComparacion
  (lambda (cartas coincidencia numE aux aux2)
    (if (equal? aux numE)
        #f
        (if (equal? aux2 numE)
            (verificarComparacion cartas coincidencia numE (+ aux 1) 0)
            (if (equal? (getElemento (car cartas) aux)(getElemento (car (cdr cartas)) aux2))
                (if (equal? (getElemento (car cartas) aux) coincidencia)
                    #t
                    #f)
                (verificarComparacion cartas coincidencia numE aux (+ aux2 1)))))))

(define retirarCartas
  (lambda (cardsSet)
    (reverse (cdr (cdr (reverse cardsSet))))))

(define devolverCartas
  (lambda (cartas cardsSet)
    (append cartas (reverse (cdr (cdr (reverse cardsSet)))))))
     
(define sumaPuntaje
  (lambda (puntajes turno largo aux)
    (if (equal? largo aux)
        '()
        (if (equal? aux turno)
            (cons (+ (getElemento puntajes aux) 2)(sumaPuntaje puntajes turno largo (+ aux 1)))
            (cons (getElemento puntajes aux)(sumaPuntaje puntajes turno largo (+ aux 1)))))))

(define pass
  (lambda (turno num)
    (if (equal? (+ turno 1) num)
        0
        (+ turno 1))))

(define finish
  (lambda (puntajes players)
    (obtenerGanadores players puntajes (puntajeMax puntajes 0 0) 0)))

(define puntajeMax
  (lambda (puntajes aux aux2)
    (if (equal? (length puntajes) aux2)
        aux
        (if (> (getElemento puntajes aux2) aux)
            (puntajeMax puntajes (getElemento puntajes aux2) (+ aux2 1))
            (puntajeMax puntajes aux (+ aux2 1))))))

;(define cantGanadores
;  (lambda (puntajes puntajeGanador ganadores aux)
;    (if (equal? (length puntajes) aux)
;        ganadores
;        (if (equal? (getElemento puntajes aux) puntajeGanador)
;            (cantGanadores puntajes puntajeGanador (+ ganadores 1) (+ aux 1))
;            (cantGanadores puntajes puntajeGanador ganadores (+ aux 1))))))

(define obtenerGanadores
  (lambda (players puntajes puntajeGanador aux)
    (if (equal? (length puntajes) aux)
        '()
        (if (equal? (getElemento puntajes aux) puntajeGanador)
            (cons (getElemento players aux)(obtenerGanadores players puntajes puntajeGanador (+ aux 1)))
            (obtenerGanadores players puntajes puntajeGanador (+ aux 1))))))

; FUNCIÓN 14 (LISTA)
(define status
  (lambda (game)
    (getStatus game)))

; FUNCIÓN 15 (LISTA, PERO FALTA SOLUCIÓN DE POSIBLES ERRORES (NO EXISTE USUARIO)
(define score
  (lambda (game user)
    (findPlayer (getPlayers game) (getScores game) user 0)))

(define findPlayer
  (lambda (players scores user aux)
    (if (equal? (getElemento players aux) user)
        (getElemento scores aux)
        (findPlayer players scores user (+ aux 1)))))
        
     
; FUNCIÓN 16 GAMETOSTRING



;-----------------------------------EJEMPLOS DE USO------------------------------------------------------------

;-----------------------------------DEFINICIONES INICIALES-----------------------------------------------------

; Definiciones para cardsSet 1
(define elementsSet0 (list "A" "B" "C" "D" "E" "F" "G"))
(define numPlayers0 4)
(define numElementsPerCard0 3)
(define maxCards0 5)

; Definiciones para cardsSet 2
(define elementsSet1 (list "SOL" "LUNA" "NIEVE" "LLUVIA" "CELESTE"))
(define numPlayers1 3)
(define numElementsPerCard1 3)
(define maxCards1 7)

; Definiciones para cardsSet 3
(define elementsSet2 (list "SOL" "LUNA" "NIEVE" "LLUVIA" "CELESTE" "ROJO" "AZUL" "UC" "CC" "UCH" "LAYS" "MOUSE" "PS5"))
(define numPlayers2 2)
(define numElementsPerCard2 4)
(define maxCards2 -1)

;-----------------------------------EJEMPLOS PARA LA FUNCION CARDSSET-----------------------------------------------------

; IMPORTANTE: ESTA FUNCIÓN REQUIERE DE LOS ELEMENTOS NECESARIOS PARA GENERAR EL MAZO INDEPENDIENTE DE LA CANTIDAD DE CARTAS A GENERAR
; EJEMPLO: SI SE QUIERE GENERAR 5 CARTAS CON 3 ELEMENTOS POR CADA UNA, SE DEBE INGRESAR 7 ELEMENTOS.

; Prueba con cardsSet1
; En este caso se genera un mazo incompleto, ya que, se necesitan 7 cartas para jugar
; El mazo igualmente se crea, debido a que cuenta con la cantidad de elementos necesarias para generar un mazo completo
(define dobbleSet0 (cardsSet elementsSet0 numElementsPerCard0 maxCards0))

; Prueba con cardsSet2
; En este caso el programa retorna false, debido a que no se cuenta con la cantidad necesaria de elementos para generar un set completo
(define dobbleSet1 (cardsSet elementsSet1 numElementsPerCard1 maxCards1))

; Prueba con cardsSet3
; En este caso se genera un set completo con el que se puede jugar
(define dobbleSet2 (cardsSet elementsSet2 numElementsPerCard2 maxCards2))




; PRUEBAS SCRIPTS
(define dobble?0 (dobble? dobbleSet0))
(define numCards0 (numCards dobbleSet0))
(define nthCard0 (nthCard dobbleSet0 3))
(define findTotalCards0 (findTotalCards (nthCard dobbleSet0 1)))
(define requiredElements0 (requiredElements (nthCard dobbleSet0 1)))
(define missingCards0 (missingCards dobbleSet0))

;(define game1 (game numPlayers dobbleSet1 stackMode))
;registra 3 usuarios con nombres de usuario diferentes
;(define game2 (register "user1" game1))
;(define game3 (register "user2" game2))
;(define game4 (register "user3" game3))
;intenta registrar al usuario “user3” que ya fue registrado
;(define game5 (register "user3" game4))
;registra al cuarto jugador
;(define game6 (register "user4" game5))
;intenta registrar a un quinto jugador
;(define game7 (register "user5" game6))
;retorna el nombre de usuario a quien corresponde el turno
;(define whoseTurnIsIt?0 (whoseTurnIsIt? game7))
;(define status0 (status game7))
;(define score0 (score game7 "user2"))

;(define game8 (play game7 null))
;(define game9 (play game8 pass))
;(define game10 (play game9 (spotit "C")))
;(define game11 (play game10 null))
;(define game12 (play game11 (spotit "E")))
;(define game13 (play game12 pass))
;(define game14 (play game13 pass))
;(define game15 (play game14 finish))
;(define game16 (play game15 null))

; PRUEBAS PERSONALES
;(define getDeck0 (getDeck dobbleSet0))
;(define getElements0 (getElements dobbleSet0))
;(define getNumE0 (getNumE dobbleSet0))
;(define getMaxC0 (getMaxC dobbleSet0))
;(define calculo1 (calculo numElementsPerCard))
;(define crear (createDeck elementsSet numElementsPerCard))
;(define obtener (getElemento elementsSet 3))
;(define crear2 (nextCards elementsSet numElementsPerCard 1 0))
;(define pruebas (list (list 1 2) (list 3 4)))
;(define crear3 (lastCards elementsSet numElementsPerCard 0 0 0))
;(define dobble?1 (dobble? (list (list "A" "B" "C")(list "A" "B" "C")(list "A" "D" "E")(list "A" "F" "G")(list "B" "E" "G")(list "B" "F" "G")(list "C" "D" "G"))))
