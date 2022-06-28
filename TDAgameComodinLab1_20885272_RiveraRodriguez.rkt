#lang racket

(require "TDAcardsSetComodinLab1_20885272_RiveraRodriguez.rkt")

;--------------------------------------TDA GAME----------------------------------------------------------------

;------------------------------------REPRESENTACIÓN------------------------------------------------------------

; Este TDA corresponde a un Game, donde se almacena los datos necesarios para la ejecución de un juego,
; estos son: número de jugadores, un cardsSet y el modo de juego
; El formato una vez se agrega los demás datos es: (int X list X procedure X string X list X int X list X list)
; A esto se le agrega una posición más cuando se termine el juego, el cuál es una lista que contiene a los ganadores o ganador

;-------------------------------------CONSTRUCTOR-------------------------------------------------------------

; Descripción: Función que obtiene los datos entregados para ejecutar un juego
; Dominio: Número de jugadores, un cardsSet y el modo de juego (int X list X procedure)
; Recorrido: lista con los datos mencionados anteriormente con la agregación de más datos explicados en los selectores
; Tipo de Recursión: No se utiliza recursión
(define game
  (lambda (numPlayers cardsSet mode)
    (if (equal? #f (verificarMazo cardsSet))
        #f
        (list numPlayers cardsSet mode "No iniciado" '() 0 '() '()))))

;---------------------------------------SELECTORES------------------------------------------------------------

; Descripción: Función que obtiene la cantidad de jugadores de la partida
; Dominio: Un game
; Recorrido: Cantidad de jugadores (integer)
; Tipo de Recursión: No se utiliza recursión
(define getNumPlayers
  (lambda (game)
    (car game)))

; Descripción: Función que obtiene el cardsSet
; Dominio: Un game
; Recorrido: Un cardsSet (list)
; Tipo de Recursión: No se utiliza recursión
(define getCardsSet
  (lambda (game)
    (car (cdr game))))

; Descripción: Función que obtiene la función stackMode
; Dominio: Un game
; Recorrido: Función stackMode (procedure)
; Tipo de Recursión: No se utiliza recursión
(define getMode
  (lambda (game)
    (car (cdr (cdr game)))))

; Descripción: Función que obtiene el estado del juego
; Dominio: Un game
; Recorrido: Estado del juego (string)
; Tipo de Recursión: No se utiliza recursión
(define getStatus
  (lambda (game)
    (car (cdr (cdr (cdr game))))))

; Descripción: Función que obtiene la mesa de juego
; Dominio: Un game
; Recorrido: Mesa del juego (list)
; Tipo de Recursión: No se utiliza recursión
(define getTable
  (lambda (game)
    (car (cdr (cdr (cdr (cdr game)))))))

; Descripción: Función que obtiene el índice de la posición del jugador al que le corresponde el turno
; Dominio: Un game
; Recorrido: Un número (integer)
; Tipo de Recursión: No se utiliza recursión
(define getTurn
  (lambda (game)
    (car (cdr (cdr (cdr (cdr (cdr game))))))))

; Descripción: Función que obtiene la lista de jugadores
; Dominio: Un game
; Recorrido: Lista de jugadores (list)
; Tipo de Recursión: No se utiliza recursión
(define getPlayers
  (lambda (game)
    (car (cdr (cdr (cdr (cdr (cdr (cdr game)))))))))

; Descripción: Función que obtiene los puntajes de los jugadores de la partida
; Dominio: Un game
; Recorrido: Puntajes de jugadores (list)
; Tipo de Recursión: No se utiliza recursión
(define getScores
  (lambda (game)
    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr game))))))))))

;-----------------------------------OTRAS FUNCIONES---------------------------------------------------------------

; Descripción: Función que permite verificar si un conjunto de cartas es válido, es decir, que en cada carta
; se listan n elementos diferentes y solo existe un elemento común entre pares de cartas.
; Dominio: Un cardsSet
; Recorrido: Si el set es válido retorna true, caso contrario false (boolean)
; Tipo de Recursión: Recursión Natural (llamado a funciones recursivas naturales internamente)
; Justificación de Recursión: Necesaria para verificar las igualdades entre pares de cartas
(define verificarMazo
  (lambda (cardsSet)
    (if (equal? (length (getDeck cardsSet)) (calculo (length (car (getDeck cardsSet)))))
        (if (equal? #t (reviewDeck (getDeck cardsSet) (length (getDeck cardsSet)) 0 1))
            #t
            #f)
        #f)))

; Descripción: Función que verifica si existe un nombre igual en la lista de jugadores
; Dominio: Un nombre de usuario (string), un game y dos auxiliares (integers)
; Recorrido: Boolean
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para verificar si el nombre existe en la lista previa al registro
(define verificarName
  (lambda (user game aux aux2)
    (if (equal? aux aux2)
        #t
        (if (equal? user (getElemento (getPlayers game) aux2))
            #f
            (verificarName user game aux (+ aux2 1))))))

; Descripción: Función que agrega el nombre del jugador/a
; Dominio: Un nombre de usuario (string) y un game
; Recorrido: Una lista de jugadores actualizada
; Tipo de Recursión: No se utiliza recusión
(define agregateName
  (lambda (user game)
    (cons user (getPlayers game))))

; Descripción: Función que agrega el puntaje inicial del jugador/a
; Dominio: Un game
; Recorrido: Una lista de puntajes actualizada
; Tipo de Recursión: No se utiliza recusión
(define agregateScore
  (lambda (game)
    (cons 0 (getScores game))))

; Descripción: Función que obtiene la coincidencia señala por el usuario
; Dominio: Un elemento
; Recorrido: El elemento
; Tipo de Recursión: No se utiliza recursión
(define spotit
  (lambda (coincidencia)
    coincidencia))

; Descripción: Función que verifica si la igualdad señalada es la correcta
; Dominio: Un par de cartas (list), una coincidencia, número de elementos por carta (integer) y dos auxiliares (integers)
; Recorrido: Boolean
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para verificar si la coincidencia señalada por el jugador es o no cierta
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

; Descripción: Función que permite retirar las cartas mostradas en la mesa del mazo
; Dominio: Un set de cartas (list)
; Recorrido: Un set de cartas actualizado (list)
; Tipo de Recursión: No se utiliza recursión
(define retirarCartas
  (lambda (cardsSet)
    (reverse (cdr (cdr (reverse cardsSet))))))

; Descripción: Función que permite devolver las cartas mostradas en la mesa a la parte inferior del mazo
; Dominio: Un par de cartas (list y un set de cartas (list)
; Recorrido: Un set de cartas actualizado (list)
; Tipo de Recursión: No se utiliza recursión
(define devolverCartas
  (lambda (cartas cardsSet)
    (append cartas (reverse (cdr (cdr (reverse cardsSet)))))))

; Descripción: Función que suma puntaje en caso de que la igualdad señalada sea la correcta
; Dominio: Puntajes de los jugadores (list), el turno actual (integer), el largo de la lista (integer) y un auxiliar (integer)
; Recorrido: Lista de puntajes actualizada (list)
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para sumar el puntaje correspondiente
(define sumaPuntaje
  (lambda (puntajes turno largo aux)
    (if (equal? largo aux)
        '()
        (if (equal? aux turno)
            (cons (+ (getElemento puntajes aux) 2)(sumaPuntaje puntajes turno largo (+ aux 1)))
            (cons (getElemento puntajes aux)(sumaPuntaje puntajes turno largo (+ aux 1)))))))

; Descripción: Función que realiza la acción de pasar de turno
; Dominio: El turno actual (integer) y el total de jugadores registrado (integer)
; Recorrido: Turno actualizado (integer)
; Tipo de Recursión: No se utiliza recursión
(define pass
  (lambda (turno num)
    (if (equal? (+ turno 1) num)
        0
        (+ turno 1))))

; Descripción: Función que finaliza el juego
; Dominio: Lista de puntajes y jugadores (ambos list)
; Recorrido: Lista de ganador/es (list)
; Tipo de Recursión: Recursión Natural (llamada a función recursiva natural internamente)
; Justificación de Recursión: Necesaria para establecer los ganadores o ganador de una partida
(define finish
  (lambda (puntajes players)
    (obtenerGanadores players puntajes (puntajeMax puntajes 0 0) 0)))

; Descripción: Función que obtiene el puntaje máximo entre los jugadores
; Dominio: Lista de puntajes (list) y dos auxiliares (integer)
; Recorrido: Puntaje máximo
; Tipo de Recursión: Recursión de Cola
; Justificación de Recursión: Necesaria para obtener el puntaje máximo de la lista
(define puntajeMax
  (lambda (puntajes aux aux2)
    (if (equal? (length puntajes) aux2)
        aux
        (if (> (getElemento puntajes aux2) aux)
            (puntajeMax puntajes (getElemento puntajes aux2) (+ aux2 1))
            (puntajeMax puntajes aux (+ aux2 1))))))

; Descripción: Función que obtiene el jugador o jugadores que obtuvieron el puntaje ganador
; Dominio: Lista de jugadores (list), lista de puntajes (list), puntaje ganador (integer) y un auxiliar (integer)
; Recorrido: Lista de jugadores ganadores (list)
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para obtener el jugador/es ganador/es
(define obtenerGanadores
  (lambda (players puntajes puntajeGanador aux)
    (if (equal? (length puntajes) aux)
        '()
        (if (equal? (getElemento puntajes aux) puntajeGanador)
            (cons (getElemento players aux)(obtenerGanadores players puntajes puntajeGanador (+ aux 1)))
            (obtenerGanadores players puntajes puntajeGanador (+ aux 1))))))

; Descripción: Función que obtiene el puntaje de un jugador registrado
; Dominio: Una lista de jugadores (list), una lista de puntajes (list), el nombre del jugador (string) y un auxiliar (integer)
; Recorrido: Puntaje del jugador (integer)
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para encontrar al jugador siempre y cuando esté registrado
(define findPlayer
  (lambda (players scores user aux)
    (if (equal? (length players) aux)
        #f
        (if (equal? (getElemento players aux) user)
            (getElemento scores aux)
            (findPlayer players scores user (+ aux 1))))))

; Descripción: Función que obtiene la representación string de un game
; Dominio: Un game y un auxiliar (integer)
; Recorrido: Un string
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para obtener el string
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
                            (string-append (string-append (string-append "TURNO ACTUAL: " (getElemento (getPlayers game) (getTurn game))) "\n") (creaGameString game (+ aux 1)))
                            (if (equal? 6 aux)
                                (string-append (string-append "LISTA DE JUGADORES: \n" (jugadoresString (getPlayers game) (getScores game) 0)) (creaGameString game (+ aux 2)))
                                (if (equal? 8 aux)
                                    (if (> (length (getElemento game 8)) 1)
                                        (string-append (string-append "GANADOR: \n" (jugadoresString (getPlayers game) (getScores game) 0)) (creaGameString game (+ aux 1)))
                                        (string-append (string-append "GANADORES: \n" (jugadoresGanadoresString (getPlayers game) (getScores game) (puntajeMax (getScores game) 0 0) 0 1) (creaGameString game (+ aux 1)))))
                                    '()))))))))))

; Descripción: Función que obtiene la representación string de una carta, similar a la original pero con modificaciones
; Dominio: Una carta (list) y un auxiliar (integer)
; Recorrido: Un string
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para obtener el string
(define cartaString2
  (lambda (carta aux)
    (if (equal? (length carta) aux)
        ""
        (if (equal? (- (length carta) 1) aux)
            (string-append (getElemento carta aux) (cartaString2 carta (+ aux 1)))
            (string-append (string-append (getElemento carta aux) ", ") (cartaString2 carta (+ aux 1)))))))

; Descripción: Función que obtiene la representación string de los jugadores con su puntaje
; Dominio: Una lista de jugadores y puntajes (ambos list) y un auxiliar (integer)
; Recorrido: Un string
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para obtener el string
(define jugadoresString
  (lambda (jugadores puntos aux)
    (if (equal? (length jugadores) aux)
        ""
        (string-append (string-append (string-append (string-append (string-append (getElemento jugadores aux) " tiene ") (number->string (getElemento puntos aux)) " puntos") "\n") (jugadoresString jugadores puntos (+ aux 1)))))))

; Descripción: Función que obtiene la representación string de el jugador/es ganador/es
; Dominio: Una lista de jugadores y puntajes (ambos list), un puntaje ganador (integer) y dos auxiliares (integers)
; Recorrido: Un string
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para obtener el string
(define jugadoresGanadoresString
  (lambda (jugadores puntos puntMax aux aux2)
    (if (equal? (length jugadores) aux)
        ""
        (if (equal? (getElemento puntos aux) puntMax)
            (string-append (string-append (string-append (string-append (number->string aux2) ". ") (getElemento jugadores aux)) "\n") (jugadoresGanadoresString jugadores puntos puntMax (+ aux 1) (+ aux2 1)))
            (jugadoresGanadoresString jugadores puntos puntMax (+ aux 1) aux2)))))

; Se utiliza provide para poder utilizar al TDA y sus funciones en otros archivos
(provide (all-defined-out))