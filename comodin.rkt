#lang racket

;-------------------------------------CONSTRUCTOR-----------------------------------------

; Descripción: Función que obtiene los datos entregados para generar un mazo siempre y cuando sea posible
; Dominio: lista de elementos, numero de elementos, cantidad máxima de cartas y función de aleatorización (list x int x int x procedure)
; Recorrido: lista con los datos mencionados anteriormente con la agregación del mazo generado en su posición inicial
; Tipo de Recursión: Recursión Natural (llamado a funciones recursivas naturales internamente)
; Justificacion de Recursión: Necesaria para generar un mazo y ajustarlos a lo solicitado por el usuario
(define cardsSet
  (lambda (Elements numE maxCards randomFn)
    (if (< (length (eliminaRepetidos Elements 0 0 (repetidos (detectaRepetidos Elements 0 1) 0 0 '() 1) 0)) (calculo numE))
        #f
        (if (equal? maxCards -1)
            (list (aleatorizarMazo (createDeck (aleatorizar (eliminaRepetidos Elements 0 0 (repetidos (detectaRepetidos Elements 0 1) 0 0 '() 1) 0)) numE)) (eliminaRepetidos Elements 0 0 (repetidos (detectaRepetidos Elements 0 1) 0 0 '() 1) 0) numE maxCards randomFn)
            (list (cutDeck (aleatorizarMazo (createDeck (aleatorizar (eliminaRepetidos Elements 0 0 (repetidos (detectaRepetidos Elements 0 1) 0 0 '() 1) 0)) numE)) maxCards) (eliminaRepetidos Elements 0 0 (repetidos (detectaRepetidos Elements 0 1) 0 0 '() 1) 0) numE maxCards randomFn)))))

;---------------------------------------SELECTORES------------------------------------------

; Descripción: Función que obtiene el mazo generado
; Dominio: una lista de información de tipo cardsSet
; Recorrido: set de cartas (list)
; Tipo de Recursión: No se utiliza recursión
(define getDeck
  (lambda (cardsSet)
    (car cardsSet)))

; Descripción: Función que obtiene la lista de elementos
; Dominio: una lista de información de tipo cardsSet
; Recorrido: lista de elementos (list)
; Tipo de Recursión: No se utiliza recursión
(define getElements
  (lambda (cardsSet)
    (car (cdr cardsSet))))

; Descripción: Función que obtiene el número de elementos por carta
; Dominio: una lista de información de tipo cardsSet
; Recorrido: cantidad de elementos por carta (integer)
; Tipo de Recursión: No se utiliza recursión
(define getNumE
  (lambda (cardsSet)
    (car (cdr (cdr cardsSet)))))

; Descripción: Función que obtiene la cantidad de cartas presentes en el mazo
; Dominio: una lista de información de tipo cardsSet
; Recorrido: cantidad de cartas (integer)
; Tipo de Recursión: No se utiliza recursión
(define getMaxC
  (lambda (cardsSet)
    (car (cdr (cdr (cdr cardsSet))))))

; Descripción: Función random
; Dominio: una lista de información de tipo cardsSet
; Recorrido: función random (procedure)
; Tipo de Recursión: No se utiliza recursión
(define getRandomFn
  (lambda (cardsSet)
    (car (cdr (cdr (cdr (cdr cardsSet)))))))

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

(define aleatorizarMazo
  (lambda (cardsSet)
    (if (null? cardsSet)
        '()
        (cons (aleatorizar (getElemento cardsSet 0)) (aleatorizarMazo (cdr cardsSet))))))
     
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
           
; FUNCION 3 (LISTA)
(define numCards
  (lambda (cardsSet)
    (length (getDeck cardsSet))))
        
; FUNCION 4 (LISTA)
(define nthCard
  (lambda (cardsSet Entero)
    (if (< (- (length (getDeck cardsSet)) 1) Entero)
        #f
        (findCard (getDeck cardsSet) Entero))))

(define findCard
  (lambda (Deck Entero)
    (if (equal? Entero 0)
        (car Deck)
        (findCard (cdr Deck) (- Entero 1)))))

; FUNCION 5 (LISTA)
(define findTotalCards
  (lambda (card)
    (if (equal? card #f)
        #f
        (calculo (length card)))))

; FUNCION 6 (LISTA)
(define requiredElements
  (lambda (card)
    (if (equal? card #f)
        #f
        (calculo (length card)))))

; FUNCION 7 (LISTA)
(define missingCards
  (lambda (cardsSet)
    (compareDecks (aleatorizarMazo (createDeck (aleatorizar (eliminaRepetidos (getElements cardsSet) 0 0 (repetidos (detectaRepetidos (getElements cardsSet) 0 1) 0 0 '() 1) 0)) (getNumE cardsSet))) (getDeck cardsSet) (calculo (getNumE cardsSet)) (length (getDeck cardsSet)) 0 0)))

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
        (if (equal? total largo)
            #t
            #f)
        (if (equal? aux2 largo)
            (if (equal? comparaciones 1)
                (compareCard2 card1 card2 largo (+ aux 1) 0 0 (+ total 1))
                (compareCard2 card1 card2 largo (+ aux 1) 0 0 total))
            (if (equal? (getElemento card1 aux)(getElemento card2 aux2))
                (compareCard2 card1 card2 largo aux (+ aux2 1) (+ comparaciones 1) total)
                (compareCard2 card1 card2 largo aux (+ aux2 1) comparaciones total))))))

; FUNCION 8 (LISTA)
(define cardsSet->string
  (lambda (cardsSet)
    (creaString (getDeck cardsSet) (length (getDeck cardsSet)) 0)))

(define creaString
  (lambda (mazo largo aux)
    (if (equal? aux largo)
        "\n"
        (string-append (string-append (string-append (string-append "Carta " (number->string (+ aux 1))) ": ") (cartaString (getElemento mazo aux) 0)) (creaString mazo largo (+ aux 1))))))

(define cartaString
  (lambda (carta aux)
    (if (equal? (length carta) aux)
        "\n"
        (if (equal? (- (length carta) 1) aux)
            (string-append (getElemento carta aux) (cartaString carta (+ aux 1)))
            (string-append (string-append (getElemento carta aux) ", ") (cartaString carta (+ aux 1)))))))

; FUNCIÓN 9 (LISTA)
(define game
  (lambda (numPlayers cardsSet mode)
    (if (equal? #f (dobble? cardsSet))
        #f
        (list numPlayers cardsSet mode "No iniciado" '() 0 '() '()))))

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
    (if (equal? action finish)
        (cons (getNumPlayers game)(cons (getCardsSet game) (cons (getMode game) (cons "Terminado" (cons (getTable game) (cons (getTurn game) (cons (getPlayers game) (cons (getScores game) (list (finish (getScores game)(getPlayers game)))))))))))
        (if (equal? (length game) 9)
            #f
            (if (< (length (car (getCardsSet game))) 2)
                game
                (if (equal? action null)
                    (cons (getNumPlayers game)(cons (getCardsSet game) (cons (getMode game) (cons "En juego" (cons ((getMode game) (car (getCardsSet game))) (cons (getTurn game) (cons (getPlayers game)(list (getScores game)))))))))
                    (if (equal? action pass)
                        (cons (getNumPlayers game)(cons (cons (devolverCartas ((getMode game)(car (getCardsSet game)))(car(getCardsSet game)))(cdr (getCardsSet game)))(cons (getMode game)(cons "En juego" (cons '() (cons (pass (getTurn game) (getNumPlayers game)) (cons (getPlayers game)(list (getScores game)))))))))
                        (if (equal? #t (verificarComparacion (getTable game) action (car (cdr (cdr (getCardsSet game)))) 0 0))
                            (cons (getNumPlayers game)(cons (cons (retirarCartas (car (getCardsSet game))) (cdr (getCardsSet game))) (cons (getMode game)(cons "En juego"(cons '() (cons (pass (getTurn game) (getNumPlayers game))(cons (getPlayers game)(list (sumaPuntaje (getScores game)(getTurn game)(length (getScores game)) 0)))))))))
                            (cons (getNumPlayers game)(cons (cons (devolverCartas ((getMode game) (car (getCardsSet game))) (car (getCardsSet game))) (cdr (getCardsSet game))) (cons (getMode game)(cons "En juego" (cons '() (cons (pass (getTurn game) (getNumPlayers game))(cons (getPlayers game) (list (getScores game)))))))))))))))))

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

; FUNCIÓN 15 (LISTA)
(define score
  (lambda (game user)
    (findPlayer (getPlayers game) (getScores game) user 0)))

(define findPlayer
  (lambda (players scores user aux)
    (if (equal? (length players) aux)
        #f
        (if (equal? (getElemento players aux) user)
            (getElemento scores aux)
            (findPlayer players scores user (+ aux 1))))))
        
; FUNCIÓN 16 GAMETOSTRING

(define game->string
  (lambda (game)
    (creaGameString game 0)))

(define creaGameString
  (lambda (game aux)
    (if (equal? (length game) aux)
        "\n"
        (if (equal? 0 aux)
            (string-append (string-append (string-append "JUGADORES EN LA PARTIDA: " (number->string (getNumPlayers game)))"\n") (creaGameString game (+ aux 2)))
            (if (equal? 2 aux)
                (string-append "MODO DE JUEGO: STACK MODE\n" (creaGameString game (+ aux 1)))
                (if (equal? 3 aux)
                    (string-append (string-append (string-append "ESTADO DEL JUEGO: " (getStatus game)) "\n")(creaGameString game (+ aux 1)))
                    (if (equal? 4 aux)
                        (if (< (length (getTable game)) 2)
                            (string-append "MESA: VACÍA\n" (creaGameString game (+ aux 1)))       
                            (string-append (string-append (string-append (string-append (string-append "MESA:\n" "CARTA 1: ") (cartaString (getElemento (getTable game) 0) 0)) "") (string-append "CARTA 2: " (cartaString (getElemento (getTable game) 1) 0))) (creaGameString game (+ aux 1))))
                        (if (equal? 5 aux)
                            (string-append (string-append (string-append "TURNO ACTUAL: " (whoseTurnIsIt? game)) "\n") (creaGameString game (+ aux 1)))
                            (if (equal? 6 aux)
                                (string-append (string-append "LISTA DE JUGADORES: \n" (jugadoresString (getPlayers game) (getScores game) 0)) (creaGameString game (+ aux 2)))
                                (if (equal? 8 aux)
                                    (if (> (length (getElemento game 8)) 1)
                                        (string-append (string-append "GANADOR: \n" (jugadoresString (getPlayers game) (getScores game) 0)) (creaGameString game (+ aux 1)))
                                        (string-append (string-append "GANADORES: \n" (jugadoresGanadoresString (getPlayers game) (getScores game) (puntajeMax (getScores game) 0 0) 0 1) (creaGameString game (+ aux 1)))))
                                    '()))))))))))
                 
(define cartaString2
  (lambda (carta aux)
    (if (equal? (length carta) aux)
        ""
        (if (equal? (- (length carta) 1) aux)
            (string-append (getElemento carta aux) (cartaString2 carta (+ aux 1)))
            (string-append (string-append (getElemento carta aux) ", ") (cartaString2 carta (+ aux 1)))))))

(define jugadoresString
  (lambda (jugadores puntos aux)
    (if (equal? (length jugadores) aux)
        ""
        (string-append (string-append (string-append (string-append (string-append (getElemento jugadores aux) " tiene ") (number->string (getElemento puntos aux)) " puntos") "\n") (jugadoresString jugadores puntos (+ aux 1)))))))

(define jugadoresGanadoresString
  (lambda (jugadores puntos puntMax aux aux2)
    (if (equal? (length jugadores) aux)
        ""
        (if (equal? (getElemento puntos aux) puntMax)
            (string-append (string-append (string-append (string-append (number->string aux2) ". ") (getElemento jugadores aux)) "\n") (jugadoresGanadoresString jugadores puntos puntMax (+ aux 1) (+ aux2 1)))
            (jugadoresGanadoresString jugadores puntos puntMax (+ aux 1) aux2)))))

(define randomFn
  (lambda (xn)
    (modulo (+ (* a xn) c) m)))
;-----------------------------------EJEMPLOS DE USO------------------------------------------------------------

; IMPORTANTE: ESTA FUNCIÓN SE DECLARA PARA RESPETAR LOS EJEMPLOS DE USO PRESENTES EN EL DRIVE
; PARA LOS ASPECTOS DE ALEATORIZACIÓN SE OCUPÓ LA OPCIÓN DE CREAR UN MÉTODO PERSONAL, EN ESTE CASO SON LAS FUNCIONES "ALEATORIZAR" Y "ALEATORIZARMAZO"

; Función random
(define m 2147483647)
(define a 1103515245)
(define c 12345)

;-----------------------------------DEFINICIONES INICIALES-----------------------------------------------------

; Definiciones para cardsSet 1
(define elementsSet0 (list "A" "B" "C" "D" "E" "F" "G"))
(define numPlayers0 4)
(define numElementsPerCard0 3)
(define maxCards0 5)

; Definiciones para cardsSet 2
(define elementsSet1 (list "SOL" "LUNA" "NIEVE" "SOL" "CELESTE" "ROJO" "UC" "UC" "CC" "UCH" "LUNA" "MOUSE" "PS5"))
(define numPlayers1 3)
(define numElementsPerCard1 4)
(define maxCards1 7)

; Definiciones para cardsSet 3
(define elementsSet2 (list "USACH" "LOL" "PARADIGMAS" "ANDROID" "PS4" "XBOX" "DJ"))
(define numPlayers2 2)
(define numElementsPerCard2 3)
(define maxCards2 -1)

; Definiciones para cardsSet 4
(define elementsSet3 (list "SOL" "LUNA" "NIEVE" "LLUVIA" "CELESTE" "ROJO" "AZUL" "UC" "CC" "UCH" "LAYS" "MOUSE" "PS5"))
(define numPlayers3 3)
(define numElementsPerCard3 4)
(define maxCards3 -1)

;-----------------------------------EJEMPLOS PARA LA FUNCION CARDSSET-----------------------------------------------------

; IMPORTANTE: ESTA FUNCIÓN REQUIERE DE LOS ELEMENTOS NECESARIOS PARA GENERAR EL MAZO INDEPENDIENTE DE LA CANTIDAD DE CARTAS A GENERAR
; EJEMPLO: SI SE QUIERE GENERAR 5 CARTAS CON 3 ELEMENTOS POR CADA UNA, SE DEBE INGRESAR 7 ELEMENTOS.

; Prueba con cardsSet1
; En este caso se genera un mazo incompleto, ya que, se necesitan 7 cartas para jugar
; El mazo igualmente se crea, debido a que cuenta con la cantidad de elementos necesarias para generar un mazo completo
(define dobbleSet0 (cardsSet elementsSet0 numElementsPerCard0 maxCards0 randomFn))

; Prueba con cardsSet2
; En este caso el programa retorna false, debido a que no se cuenta con la cantidad necesaria de elementos para generar un set completo
; Posee elementos repetidos que son eliminados por el programa
(define dobbleSet1 (cardsSet elementsSet1 numElementsPerCard1 maxCards1 randomFn))

; Prueba con cardsSet3
; En este caso se genera un set completo con el que se puede jugar
(define dobbleSet2 (cardsSet elementsSet2 numElementsPerCard2 maxCards2 randomFn))

; Prueba con cardsSet4
; En este caso se genera un set completo con el que se puede jugar
(define dobbleSet3 (cardsSet elementsSet3 numElementsPerCard3 maxCards3 randomFn))

; EN LOS SIGUIENTES EJEMPLO SE OMITIRÁ EL USO DEL CARDSSET2, YA QUE, AL NO PODER FORMARSE UN SET DE CARTAS, NO ES VÁLIDO PARA OCUPAR LAS OTRAS FUNCIONES

;-----------------------------------EJEMPLOS PARA LA FUNCION DOBBLE?-----------------------------------------------------

; Prueba con cardsSet1, retorna false porque el mazo no está completo
(define dobble?0 (dobble? dobbleSet0))

; Prueba con cardsSet3, retorna true porque el mazo está completo
(define dobble?1 (dobble? dobbleSet2))

; Prueba con cardsSet4, retorna true porque el mazo está completo
(define dobble?2 (dobble? dobbleSet3))

;-----------------------------------EJEMPLOS PARA LA FUNCION NUMCARDS-----------------------------------------------------

; Prueba con cardsSet1, retorna 5
(define numCards0 (numCards dobbleSet0))

; Prueba con cardsSet3, retorna 7
(define numCards1 (numCards dobbleSet2))

; Prueba con cardsSet4, retorna 13
(define numCards2 (numCards dobbleSet3))

;-----------------------------------EJEMPLOS PARA LA FUNCION NTHCARD-----------------------------------------------------

; Prueba con cardsSet1, retorna la carta número 0
(define nthCard0 (nthCard dobbleSet0 0))

; Prueba con cardsSet3, retorna false porque la carta 9 no existe en el mazo
(define nthCard1 (nthCard dobbleSet2 9))

; Prueba con cardsSet4, retorna la carta número 7
(define nthCard2 (nthCard dobbleSet3 7))

;-----------------------------------EJEMPLOS PARA LA FUNCIÓN FINDTOTALCARDS-----------------------------------------------------

; Prueba con cardsSet1 que retorna false porque no existe la carta 8 en el mazo
(define findTotalCards0 (findTotalCards (nthCard dobbleSet0 8)))

; Prueba con cardsSet3 que usa como carta muestra a la número 5 y retorna 7
(define findTotalCards1 (findTotalCards (nthCard dobbleSet2 5)))

; Prueba con cardsSet4 que usa como carta muestra a la número 10 y retorna 13
(define findTotalCards2 (findTotalCards (nthCard dobbleSet3 10)))

;-----------------------------------EJEMPLOS PARA LA FUNCIÓN REQUIREDELEMENTS-----------------------------------------------------

; Prueba con cardsSet1 que retorna false porque no existe la carta 8 en el mazo
(define requiredElements0 (requiredElements (nthCard dobbleSet0 8)))

; Prueba con cardsSet3 que usa como carta muestra a la número 3 y retorna 7
(define requiredElements1 (requiredElements (nthCard dobbleSet2 3)))

; Prueba con cardsSet4 que usa como  carta muestra a la número 11 y retorna 13
(define requiredElements2 (requiredElements (nthCard dobbleSet3 11)))

;-----------------------------------EJEMPLOS PARA LA FUNCIÓN MISSINGCARDS-----------------------------------------------------

; Prueba con cardsSet1 que retorna las cartas faltantes, las cuales son 2
(define missingCards0 (missingCards dobbleSet0))

; Prueba con cardsSet3 que retorna una lista vacía porque el set está completo
(define missingCards1 (missingCards dobbleSet2))

; Prueba con cardsSet4 que retorna una lista vacía porque el set está completo
(define missingCards2 (missingCards dobbleSet3))

;-----------------------------------EJEMPLOS PARA LA FUNCIÓN CARDSSET->STRING-----------------------------------------------------

; IMPORTANTE: PARA HACER USO DE ESTA FUNCIÓN QUITAR ";" A LOS 3 EJEMPLOS, UNA VEZ BORRADO ESTE ELEMENTO SE LE MOSTRARÁ EN PANTALLA LOS 3 DIFERENTES MAZOS CREADOS

; Se muestra el cardsSet1
;(display (cardsSet->string dobbleSet0))

; Se muestra el cardsSet3
;(display (cardsSet->string dobbleSet2))

; Se muestra el cardsSet4
;(display (cardsSet->string dobbleSet3))

;-----------------------------------EJEMPLOS PARA LA FUNCIÓN GAME-----------------------------------------------------------------

; IMPORTANTE: EL USO DE LA FUNCIÓN RANDOM, COMO FUE EXPLICADO ANTERIORMENTE SE HIZO DE UNA CREACIÓN PERSONAL Y LA ALEATORIZACIÓN DEL MAZO SE REALIZÓ EN PROCESOS ANTERIORES
; ES POR ESTO, QUE NO SE INCLUYE LA FUNCIÓN RANDOM EN EL DOMINIO DE LA FUNCIÓN GAME

; SI SE INTENTA INICIAR UN JUEGO CON UN MAZO INVÁLIDO, SE RETORNA FALSE
(define game0 (game numPlayers0 dobbleSet0 stackMode))

; SE USARÁ EL CARDSSET4 PARA SIMULAR EL JUEGO, RECORDANDO QUE SE IMPLEMENTÓ ÚNICAMENTE EL MODO DE JUEGO "STACK"
; SE OBTIENE UNA SERIE DE DATOS EN UNA LISTA LOS CUÁLES SON (INICIANDO EN POSICIÓN 0 DE LA LISTA):
; POSICIÓN 0: NÚMERO DE JUGADORES
; POSICIÓN 1: CARDSSET (MAZO GENERADO Y LOS DATOS PREVIOS CORRESPONDIENTES)
; POSICIÓN 2: FUNCIÓN STACKMODE
; POSICIÓN 3: ESTADO DEL JUEGO, INICIALMENTE EN "NO INICIADO"
; POSICIÓN 4: MESA DEL JUEGO (AQUÍ SE PONE LAS CARTAS VOLTEADAS)
; POSICIÓN 5: TURNO (EL NÚMERO INDICA EL JUGADOR AL QUE LE CORRESPONDE EL TURNO)
; POSICIÓN 6: LISTA DE JUGADORES
; POSICIÓN 7: LISTA DE PUNNTAJES
; POSICIÓN 8: SE AÑADE ESTA POSICIÓN UNA VEZ SE TERMINA EL JUEGO, AQUÍ SE PONE LA LISTA DE GANADOR/ES

(define game1 (game numPlayers3 dobbleSet3 stackMode))

;-----------------------------------EJEMPLOS PARA LA FUNCIÓN REGISTER------------------------------------------------------------

; Se registra 2 usuarios con nombres diferentes, por ende, el registro es exitoso y se muestra los datos actualizados
(define game2 (register "user1" game1))
(define game3 (register "user2" game2))

; Se intenta registrar un 3er jugador con un nombre ya tomado, por ende, el registro no se realiza y se muestra los datos previos
(define game4 (register "user2" game3))

; Se registra el 3er usuario con nombre diferente, por ende, el registro es exitoso y se muestra los datos actualizados
(define game5 (register "user3" game4))

; Se intenta registrar un 4to jugador pero ya se registró a la cantidad máxima de jugadores, por ende, el registro no se realiza y se muestra los datos previos
(define game6 (register "user4" game5))

;-----------------------------------EJEMPLOS PARA LA FUNCIÓN STACKMODE-----------------------------------------------------------

; SU USO SE MUESTRA CUANDO EN LA FUNCIÓN PLAY SOLICITAMOS REALIZAR LA ACCIÓN NULL
; ESTA FUNCIÓN TOMA LAS DOS CARTAS QUE SE UBICAN EN LA PARTE SUPERIOR DEL MAZO Y LAS MUESTRA EN LA MESA (POSICIÓN 4 DE LA LISTA)

;-----------------------------------EJEMPLOS PARA LA FUNCIÓN PLAY----------------------------------------------------------------

; IMPORTANTE: SIEMPRE SE DEBE USAR LA ACCIÓN "NULL" ANTES DE REALIZAR OTRA ACCIÓN, CASO CONTRARIO PUEDE GENERAR ERRORES EN EL PROGRAMA

; Se usa la función stackMode y se da vuelta las 2 cartas ubicadas en la parte superior del mazo, para mostrarlas en la mesa
(define game7 (play game6 null))

; El jugador pasa de turno, por ende las cartas se ubican en la parte inferior del mazo y se limpia la mesa
(define game8 (play game7 pass))

; El jugador del nuevo turno da vuelta las 2 cartas
(define game9 (play game8 null))

; El jugador señala que la igualdad entre las 2 cartas es "ROJO", lo que lleva a sumar 2 pts a su puntaje personal, limpiar la mesa, retirar las 2 cartas del mazo y pasar de turno
(define game10 (play game9 (spotit "ROJO")))

; El jugador del nuevo turno da vuelta las 2 cartas
(define game11 (play game10 null))

; El jugador señala una igualdad incorrecta, lo que lleva a no sumar puntos, limpiar la mesa, ubicar las 2 cartas al inicio del mazo y pasar de turno
(define game12 (play game11 (spotit "VALORANT")))

; El turno vuelve al primer jugador y da vuelta las 2 cartas
(define game13 (play game12 null))

; El jugador señala que la igualdad es "SOL", lo que lleva a sumar 2 pts a su puntaje personal, limpiar la mesa, retirar las 2 cartas del mazo y pasar de turno
(define game14 (play game13 (spotit "SOL")))

; El jugador del nuevo turno da vuelta las 2 cartas
(define game15 (play game14 null))

; EN LOS SIGUIENTES EJEMPLOS SE DESARROLLARÁ EL JUEGO ENTERO, SE OMITE LA INFORMACIÓN DE CADA MOVIMIENTO PORQUE SON LOS YA EXPLICADOS ANTERIORMENTE

(define game16 (play game15 (spotit "UC")))
(define game17 (play game16 null))
(define game18 (play game17 (spotit "PS5")))
(define game19 (play game18 null))
(define game20 (play game19 (spotit "LUNA")))
(define game21 (play game20 null))
(define game22 (play game21 (spotit "CC")))

; No se puede seguir jugando debido a que no queda la cantidad de cartas mínimas en el mazo (2 cartas), por ende solo se retorna el juego previo
; Si se llega a este punto se debe finalizar el juego, teniendo en cuenta que también puede terminarlo antes
(define game23 (play game22 null))

; Se finaliza el juego y se agrega en la última posición de la lista el ganador y se cambia el estado a "Terminado"
(define game24 (play game23 finish))

; Se terminó el juego y no se puede volver a interactuar con el, por ende, retorna false
(define game25 (play game24 null))

;-----------------------------------EJEMPLOS PARA LA FUNCIÓN WHOSETURNISIT?-----------------------------------------------------

; Determina a que jugador le pertenece el turno, en este caso como se está preguntando al inicio, le corresponde el turno a "user3"
(define whoseTurnIsIt?0 (whoseTurnIsIt? game6))

(define whoseTurnIsIt?1 (whoseTurnIsIt? game15))

(define whoseTurnIsIt?2 (whoseTurnIsIt? game20))

;-----------------------------------EJEMPLOS PARA LA FUNCIÓN STATUS--------------------------------------------------------------

(define status0 (status game6))

(define status1 (status game17))

(define status2 (status game24))

;-----------------------------------EJEMPLOS PARA LA FUNCIÓN SCORE-------------------------------------------------------------

(define score0 (score game10 "user2"))

(define score1 (score game14 "user5"))

(define score2 (score game20 "user1"))

;-----------------------------------EJEMPLOS PARA LA FUNCIÓN GAME->STRING-----------------------------------------------------

(display (game->string game24))
