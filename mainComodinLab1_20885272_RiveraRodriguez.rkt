#lang racket

; COMODÍN LAB 1 DESARROLLADO POR JEAN LUCAR RIVERA

(require "TDAcardsSetComodinLab1_20885272_RiveraRodriguez.rkt")
(require "TDAgameComodinLab1_20885272_RiveraRodriguez.rkt")

;---------------------------FUNCIÓN DOBBLE?----------------------------

; Descripción: Función que permite verificar si un conjunto de cartas es válido, es decir, que en cada carta
; se listan n elementos diferentes y solo existe un elemento común entre pares de cartas.
; Dominio: Un cardsSet
; Recorrido: Si el set es válido retorna true, caso contrario false (boolean)
; Tipo de Recursión: Recursión Natural (llamado a funciones recursivas naturales internamente)
; Justificación de Recursión: Necesaria para verificar las igualdades entre pares de cartas
(define dobble?
  (lambda (cardsSet)
    (if (equal? (length (getDeck cardsSet)) (calculo (length (car (getDeck cardsSet)))))
        (if (equal? #t (reviewDeck (getDeck cardsSet) (length (getDeck cardsSet)) 0 1))
            #t
            #f)
        #f)))

;---------------------------FUNCIÓN NUMCARDS-------------------------------------

; Descripción: Función que obtiene la cantidad de cartas presentes en el mazo
; Dominio: Un cardsSet
; Recorrido: Cantidad de cartas en el mazo (integer)
; Tipo de Recursión: No se utiliza recursión
(define numCards
  (lambda (cardsSet)
    (length (getDeck cardsSet))))

;---------------------------FUNCIÓN NTHCARD-----------------------------------------

; Descripción: Función que permite obtener la n-ésima carta de un set partiendo desde la posición 0 hasta la (total - 1)
; Dominio: Un cardsSet y un entero
; Recorrido: Carta ubicada en la posición señalada (list)
; Tipo de Recursion: Recursión Natural (llamado a función recursiva natural internamente)
; Justificación de Recursión: Necesaria para ubicar la carta solicita siempre y cuando exista
(define nthCard
  (lambda (cardsSet Entero)
    (if (< (- (length (getDeck cardsSet)) 1) Entero)
        #f
        (findCard (getDeck cardsSet) Entero))))

;--------------------------FUNCIÓN FINDTOTALCARDS--------------------------------------

; Descripción: Función que obtiene la cantidad total de cartas que se debe tener para que sea un set válido, esto a partir de una carta de muestra
; Dominio: Carta de muestra (list)
; Recorrido: Cantidad de cartas necesarias (int)
; Tipo de Recursión: No se utiliza recursión
(define findTotalCards
  (lambda (card)
    (if (equal? card #f)
        #f
        (calculo (length card)))))

;--------------------------FUNCIÓN REQUIREDELEMENTS-------------------------------------------

; Descripción: Función que obtiene la cantidad total de elementos que se debe tener para lograr un set válido, esto a partir de una carta de muestra
; Dominio: Carta de muestra (list)
; Recorrido: Cantidad de elementos necesarios (int)
; Tipo de Recursión: No se utiliza recusión
(define requiredElements
  (lambda (card)
    (if (equal? card #f)
        #f
        (calculo (length card)))))

;--------------------------FUNCIÓN MISSINGCARDS--------------------------------------------

; Descripción: Función que retorna las cartas necesarias para completar el set y así lograr que sea válido
; Dominio: Un cardsSet
; Recorrido: Cartas faltantes (list)
; Tipo de Recursión: Recursión Natural (llamado a funciones recursivas naturales internamente)
; Justificación de Recursión: Necesaria para generar un mazo completo y compararlo con el generado con los datos del usuario
(define missingCards
  (lambda (cardsSet)
    (compareDecks (aleatorizarMazo (createDeck (aleatorizar (eliminaRepetidos (getElements cardsSet) 0 0 (repetidos (detectaRepetidos (getElements cardsSet) 0 1) 0 0 '() 1) 0)) (getNumE cardsSet))) (getDeck cardsSet) (calculo (getNumE cardsSet)) (length (getDeck cardsSet)) 0 0)))

;--------------------------FUNCIÓN CARDSSET->STRING--------------------------------------------

; Descripción: Función que retorna la representación del mazo en base strings
; Dominio: Un cardsSet
; Recorrido: Un string
; Tipo de Recursión: Recursión Natural (llamado a función recursiva natural internamente)
; Justificación de Recursión: Necesaria para generar la representación string del mazo
(define cardsSet->string
  (lambda (cardsSet)
    (creaString (getDeck cardsSet) (length (getDeck cardsSet)) 0)))

;--------------------------FUNCIÓN STACKMODE-----------------------------------------------
; Descripción: Función que obtiene las 2 cartas ubicadas en la parte superior del mazo
; Dominio: Un cardsSet
; Recorrido: 2 cartas (list)
; Tipo de Recursión: No se utiliza recursión
(define stackMode
  (lambda (cardsSet)
    (list (car (reverse cardsSet))(car (cdr (reverse cardsSet))))))

;--------------------------FUNCIÓN REGISTER---------------------------------------------------
; Descripción: Función que permite registrar nuevos jugadores siempre y cuando se pueda
; Dominio: Un nombre de usuario (string) y un game
; Recorrido: Un game actualizado
; Tipo de Recursión: Recursión natural (llamado a función recursiva natural internamente)
; Justificación de Recursión: Necesario para poder registrar a un usuario siempre y cuando este no exista previamente
(define register
  (lambda (user game)
    (if (equal? (length (getPlayers game)) (getNumPlayers game))
        game
        (if (equal? #t (verificarName user game (length (getPlayers game)) 0))
            (cons (getNumPlayers game)(cons (getCardsSet game)(cons (getMode game)(cons (getStatus game) (cons (getTable game) (cons (getTurn game)(cons (agregateName user game)(list (agregateScore game)))))))))
            game))))

;--------------------------FUNCIÓN WHOSETURNISIT?-----------------------------------------------

; Descripción: Función que permite obtener el nombre del jugador al que le pertenece el turno
; Dominio: Un game
; Recorrido: El nombre del usuario (string)
; Tipo de Recursión: No se utiliza recursión
(define whoseTurnIsIt?
  (lambda (game)
    (getElemento (getPlayers game) (getTurn game))))

;--------------------------FUNCIÓN PLAY-----------------------------------------------------

; Descripción: Función que permite realizar una acción en el juego, puede ser null (función stackMode), pass (pasar de turno), spotit (detectar coincidencia) o finish (terminar juego)
; Dominio: Un game y una acción (null, pass, (spotit "coincidencia") o finish)
; Recorrido: Un game actualizado
; Tipo de Recursión: Recursión Natural (llamado a diversas funciones recursivas naturales internamente)
; Justificación de Recursión: Necesario para realizar la acción que señala el usuario
(define play
  (lambda (game action)
    (if (equal? action finish)
        (setGameFinish game (list (finish (getScores game)(getPlayers game))))
        (if (equal? (length game) 9)
            #f
            (if (< (length (car (getCardsSet game))) 2)
                game
                (if (equal? action null)
                    (setTable game ((getMode game) (car (getCardsSet game))))
                    (if (equal? action pass)
                        (setGamePassTurn game (cons (devolverCartas ((getMode game)(car (getCardsSet game)))(car(getCardsSet game)))(cdr (getCardsSet game))) (pass (getTurn game) (getNumPlayers game)))
                        (if (equal? #t (verificarComparacion (getTable game) action (car (cdr (cdr (getCardsSet game)))) 0 0))
                            (setGameCorrectaCoincidencia game (cons (retirarCartas (car (getCardsSet game))) (cdr (getCardsSet game))) (pass (getTurn game) (getNumPlayers game)) (list (sumaPuntaje (getScores game)(getTurn game)(length (getScores game)) 0))) 
                            (setGameIncorrectaCoincidencia game (cons (devolverCartas ((getMode game) (car (getCardsSet game))) (car (getCardsSet game))) (cdr (getCardsSet game))) (pass (getTurn game) (getNumPlayers game)))))))))))
                           
;--------------------------FUNCIÓN STATUS------------------------------------------------------

; Descripción: Función que permite obtener el estado del juego
; Dominio: Un game
; Recorrido: Estado del juego (string)
; Tipo de Recursión: No se utiliza recursión
(define status
  (lambda (game)
    (getStatus game)))

;--------------------------FUNCIÓN SCORE------------------------------------------------------

; Descripción: Función que permite obtener el puntaje de un jugador a partir de su nombre
; Dominio: Un game y nombre del usuario (string)
; Recorrido: Puntaje del jugador (integer)
; Tipo de Recursión: Recursión natural (llamado a función recursiva natural internamente)
; Justificación de Recursión: Necesario para ver si el jugador existe y en caso de existir ver su puntaje asociado
(define score
  (lambda (game user)
    (findPlayer (getPlayers game) (getScores game) user 0)))

;--------------------------FUNCIÓN GAME->STRING-----------------------------------------------

; Descripción: Función que permite obtener la representación en base string de un game
; Dominio: Un game
; Recorrido: Un string
; Tipo de Recursión: Recursión natural (llamado a función recursiva natural internamente)
; Justificación de Recursión: Necesario para obtener la representación en base string
(define game->string
  (lambda (game)
    (creaGameString game 0)))

;-----------------------------------EJEMPLOS DE USO------------------------------------------------------------

; IMPORTANTE: LA FUNCIÓN RANDOM SE USA PARA RESPETAR LOS EJEMPLOS DE USO PRESENTES EN EL DRIVE EN RELACIÓN AL CARDSSET, SIN EMBARGO, EN EL GAME SE OMITE ESTA FUNCIÓN EN EL DOMINIO.
; PARA LOS ASPECTOS DE ALEATORIZACIÓN SE OCUPÓ LA OPCIÓN DE CREAR UN MÉTODO PERSONAL, EN ESTE CASO SON LAS FUNCIONES "ALEATORIZAR" Y "ALEATORIZARMAZO"

; TAMBIÉN SE DEBE TENER EN CUENTA QUE LA LISTA DE ELEMENTOS DADA POR EL USUARIO DEBE SER DE STRINGS, POR EJEMPLO SI QUIERE INGRESAR (1 2 3 4) DEBE HACERLO ASÍ: ("1" "2" "3" "4")
; SE SOLICITA HACER ESTE PROCESO, YA QUE, DE NO REALIZARLO LA FUNCIÓN CARDSSET->STRING PODRÍA VERSE AFECTADA. (EJEMPLO EN EL CARDSSET 3)

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
(define elementsSet2 (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12"))
(define numPlayers2 2)
(define numElementsPerCard2 3)
(define maxCards2 -1)

; Definiciones para cardsSet 4
(define elementsSet3 (list "SOL" "LUNA" "NIEVE" "LLUVIA" "CELESTE" "ROJO" "AZUL" "UC" "CC" "UCH" "LAYS" "MOUSE" "PS5"))
(define numPlayers3 3)
(define numElementsPerCard3 4)
(define maxCards3 -1)

;-----------------------------------EJEMPLOS PARA LA FUNCION CARDSSET-----------------------------------------------------

; IMPORTANTE: ESTA FUNCIÓN REQUIERE DE LOS ELEMENTOS NECESARIOS PARA GENERAR EL MAZO COMPLETO, INDEPENDIENTE DE LA CANTIDAD DE CARTAS A GENERAR
; EJEMPLO: SI SE QUIERE GENERAR 5 CARTAS CON 3 ELEMENTOS POR CADA UNA, SE DEBE INGRESAR 7 ELEMENTOS EN LA LISTA, DE LO CONTRARIO, SE RETORNA FALSE, YA QUE,
; PUEDE PRODUCIR ERRORES EN FUNCIONES COMO MISSINGCARDS, ETC.

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
; PARA EVITAR ERRORES SEGUIR LOS EJEMPLOS DADOS, DONDE EN EL DOMINIO SE OMITE LA FUNCIÓN RANDOM

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
; TAMBIÉN SE DEBE CONSIDERAR QUE EL ORDEN DE LOS TURNOS PARTE DESDE EL ÚLTIMO JUGADOR REGISTRADO, EN ESTE CASO ES "USER3" EL PRIMERO EN JUGAR.

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

; Determina a que jugador le pertenece el turno en el game15
(define whoseTurnIsIt?1 (whoseTurnIsIt? game15))

; Determina a que jugador le pertenece el turno en el game20
(define whoseTurnIsIt?2 (whoseTurnIsIt? game20))

;-----------------------------------EJEMPLOS PARA LA FUNCIÓN STATUS--------------------------------------------------------------

; Se obtiene el estado del juego antes de ser iniciado
(define status0 (status game6))

; Se obtiene el estado del juego en medio de una partida
(define status1 (status game17))

; Se obtiene el estado del juego terminado
(define status2 (status game24))

;-----------------------------------EJEMPLOS PARA LA FUNCIÓN SCORE-------------------------------------------------------------

; Se obtiene el puntaje de "user2" en el game10
(define score0 (score game10 "user2"))

; Se obtiene el puntaje de "user5" en el game14, como este jugador no existe en los registros, se retorna false
(define score1 (score game14 "user5"))

; Se obtiene el puntaje de "user1" en el game20
(define score2 (score game20 "user1"))

;-----------------------------------EJEMPLOS PARA LA FUNCIÓN GAME->STRING-----------------------------------------------------

; IMPORTANTE: PARA HACER USO DE ESTA FUNCIÓN QUITAR ";" A LOS 3 EJEMPLOS, UNA VEZ BORRADO ESTE ELEMENTO SE LE MOSTRARÁ EN PANTALLA LOS 3 DIFERENTES MAZOS CREADOS

; Se muestra el game 5, donde aún no se inicia el juego
;(display (game->string game5))

; Se muestra el game 14, donde se está jugando
;(display (game->string game15))

; Se muestra el game 24, donde el juego finalizó
;(display (game->string game24))