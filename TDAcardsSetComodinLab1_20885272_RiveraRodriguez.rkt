#lang racket

;-----------------------------------TDA CARDSSET---------------------------------------------------------------

;------------------------------------REPRESENTACIÓN------------------------------------------------------------

; Este TDA corresponde a un CardsSet, donde se almacena los datos necesarios para la generación de un set de cartas,
; estos son: lista de elementos (list), número de elementos por carta (integer), número de cartas (integer) y la función random
; que en este caso está incluída para respetar el formato usado en los scripts del drive, ya que, en el ejemplo de uso se explica
; que se integró una aleatorización usando la opción de ser creada personalmente.
; El formato una vez se agrega el mazo es: (list X list X int X int X procedure)

;-------------------------------------CONSTRUCTOR-------------------------------------------------------------

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

;---------------------------------------SELECTORES------------------------------------------------------------

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

;-----------------------------------OTRAS FUNCIONES---------------------------------------------------------------

; Descripción: Función que permite detectar los elementos repetidos en la lista entregada por el usuario
; Dominio: Una lista de elementos (list) y dos auxiliares (integers)
; Recorrido: lista con todos los elementos repetidos (list)
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para ir comparando los elementos y verificar que no existen repetidos
(define detectaRepetidos
  (lambda (Elementos aux aux2)
    (if (equal? (length Elementos) aux)
        '()
        (if (equal? (length Elementos) aux2)
            (detectaRepetidos Elementos (+ aux 1) (+ aux 2))
            (if (equal? (getElemento Elementos aux) (getElemento Elementos aux2))
                (cons (getElemento Elementos aux)(detectaRepetidos Elementos aux (+ aux2 1)))
                (detectaRepetidos Elementos aux (+ aux2 1)))))))

; Descripción: Función que permite separar los elementos repetidos
; Dominio: Una lista de elementos repetidos (list), dos auxiliares (integers), una lista inicialmente vacia (list) y un habilitador (integer)
; Recorrido: lista con los elementos repetidos (list)
; Tipo de Recursión: Recursión de Cola
; Justificación de Recursión: Necesaria para ir agregando los elementos repetidos a la lista vacía
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

; Descripción: Función que permite eliminar los elementos repetidos de la lista
; Dominio: Una lista de elementos (list), dos auxiliares (integers), lista de repetidos (list) y un habilitador (integer)
; Recorrido: lista con los elementos repetidos eliminados (list)
; Tipo de Recursión: Recursión de Cola
; Justificación de Recursión: Necesaria para ir comparando los elementos de la lista con los repetidos y en caso de ser iguales eliminarlos
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

; Descripción: Función que permite aleatorizar los elementos de la lista entregada por el usuario (reemplaza a función random)
; Dominio: Una lista de elementos (list)
; Recorrido: Una lista de elementos aleatorizada (list)
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para aleatorizar la lista de elementos
(define aleatorizar
  (lambda (Elements)
    (if (null? Elements)
        '()
        (cons (getElemento (reverse Elements) 0) (aleatorizar (cdr (reverse Elements)))))))

; Descripción: Función que permite aleatorizar un mazo generado (reemplaza a función random)
; Dominio: Un set de cartas (list)
; Recorrido: Un set de cartas aleatorizado (list)
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para aleatorizar el set de cartas
(define aleatorizarMazo
  (lambda (cardsSet)
    (if (null? cardsSet)
        '()
        (cons (aleatorizar (getElemento cardsSet 0)) (aleatorizarMazo (cdr cardsSet))))))

; Descripción: Función que permite realizar un cálculo ocupado en las funciones "findTotalCards y requiredElements"
; Dominio: Número de elementos por carta (integer)
; Recorrido: Un entero (integer)
; Tipo de Recursión: No se utiliza recursión
(define calculo
  (lambda (numElementsPerCard)
    (+ (+ (sqr (- numElementsPerCard 1)) (- numElementsPerCard 1)) 1)))

; Descripción: Función que permite crear un mazo
; Dominio: Una lista de elementos (list) y número de elementos por carta (integer)
; Recorrido: Un set de cartas (list)
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para crear exitosamente el mazo
(define createDeck
  (lambda (Elements numE)
    (append (list (firstCard Elements numE 0))(append (nextCards Elements numE 1 0)(lastCards Elements numE 0 0 0)))))

; Descripción: Función que permite crear la primera carta del mazo
; Dominio: Una lista de elementos (list), número de elementos por carta (integer) y un auxiliar (integer)
; Recorrido: Una carta (list)
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para crear la primera carta recorriendo la lista de elementos
(define firstCard
  (lambda (Elements numE aux)
    (if (equal? numE aux)
        '()
        (cons (car Elements) (firstCard (cdr Elements) numE (+ aux 1))))))

; Descripción: Función que permite crear las siguiente N cartas del mazo
; Dominio: Una lista de elementos (list), número de elementos por carta (integer) y dos auxiliares (integers)
; Recorrido: Cartas (list)
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para crear las cartas recorriendo la lista de elementos
(define nextCards
  (lambda (Elements numE aux aux2)
    (if (equal? numE aux)
        '()
        (if (equal? numE aux2)
            (nextCards Elements numE (+ aux 1) 0)
            (cons (createCard Elements numE aux aux2 0) (nextCards Elements numE aux numE))))))

; Descripción: Función que permite crear las siguiente (N elevado 2) cartas del mazo
; Dominio: Una lista de elementos (list), número de elementos por carta (integer) y tres auxiliares (integers)
; Recorrido: Cartas (list)
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para crear las cartas recorriendo la lista de elementos
(define lastCards
  (lambda (Elements numE aux aux2 aux3)
    (if (equal? (- numE 1) aux)
        '()
        (if (equal? (- numE 1) aux2)
            (lastCards Elements numE (+ aux 1) 0 0)
            (if (equal? numE aux3)
                (lastCards Elements numE aux (+ aux2 1) 0)
                (cons (createLastCards Elements numE aux aux2 aux3 0) (lastCards Elements numE aux aux2 numE)))))))

; Descripción: Función que permite crear una carta
; Dominio: Una lista de elementos (list), número de elementos por carta (integer) y tres auxiliares (integers)
; Recorrido: Carta (list)
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para crear la carta recorriendo la lista de elementos
(define createCard
  (lambda (Elements numE aux aux2 aux3)
    (if (equal? (- numE 1) aux2)
        '()
        (if (equal? aux3 0)
            (cons (getElemento Elements 0) (createCard Elements numE aux aux2 1))
            (cons (getElemento Elements (+ (* (- numE 1) aux) (+ aux2 1))) (createCard Elements numE aux (+ aux2 1) aux3))))))

; Descripción: Función que permite crear las cartas de la función lastCards
; Dominio: Una lista de elementos (list), número de elementos por carta (integer) y cuatro auxiliares (integers)
; Recorrido: Carta (list)
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para crear la carta recorriendo la lista de elementos
(define createLastCards
  (lambda (Elements numE aux aux2 aux3 aux4)
    (if (equal? aux3 (- numE 1))
        '()
        (if (equal? aux4 0)
            (cons (getElemento Elements (+ aux 1)) (createLastCards Elements numE aux aux2 aux3 1))
            (cons (getElemento Elements (+ (+ (+ (- numE 1) 1) (* (- numE 1) aux3)) (remainder (+ (* aux aux3) aux2) (- numE 1)))) (createLastCards Elements numE aux aux2 (+ aux3 1) aux4))))))

; Descripción: Función que permite cortar el mazo según lo indicado por el usuario
; Dominio: Un set de cartas (list), un número de cartas (integer)
; Recorrido: Un set de cartas (list)
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para obtener la cantidad de cartas solicitadas por el usuario
(define cutDeck
  (lambda (set maxC)
    (if (equal? maxC 0)
        '()
        (cons (car set) (cutDeck (cdr set) (- maxC 1))))))

; Descripción: Función que permite obtener un elemento de una posición determinada
; Dominio: Una lista de elementos de cualquier tipo (list), posición solicitada (integer)
; Recorrido: Un elemento de la lista (puede ser cualquier tipo: list, integer, string)
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para ubicar la posición solicitada
(define getElemento
  (lambda (Elements num)
    (if (equal? num 0)
        (car Elements)
        (getElemento (cdr Elements) (- num 1)))))

; Descripción: Función que permite revisar el mazo y verificar si es válido
; Dominio: Un cardsSet, el largo del mazo (integer) y dos auxiliares (integers)
; Recorrido: Boolean
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para determinar si el set de cartas es válido o no
(define reviewDeck
  (lambda (cardsSet largo aux aux2)
    (if (equal? (- largo 1) aux)
        #t
        (if (equal? largo aux2)
            (reviewDeck cardsSet largo (+ aux 1) (+ aux 2))
            (if (equal? #t (compareCard (getElemento cardsSet aux) (getElemento cardsSet aux2) (length (car cardsSet)) 0 0 0 0))
                (reviewDeck cardsSet largo aux (+ aux2 1))
                #f)))))

; Descripción: Función que permite comparar dos cartas
; Dominio: Carta número 1 (list), carta número 2 (list), el largo de las cartas (integer), dos auxiliares (integers), comparaciones (integer) y el total de igualdades (integer)
; Recorrido: Boolean
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para verificar si entre las 2 cartas existe sólo un elemento igual
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

; Descripción: Función que permite encontrar una carta
; Dominio: Un set de cartas (list) y un entero (integer)
; Recorrido: Una carta (list)
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para encontrar la carta ubicada en la posición señala siempre y cuando exista
(define findCard
  (lambda (Deck Entero)
    (if (equal? Entero 0)
        (car Deck)
        (findCard (cdr Deck) (- Entero 1)))))

; Descripción: Función que permite comparar 2 mazos y obtener las cartas faltantes en el mazo generado por el usuario
; Dominio: Dos set de cartas (list), los largos de cada mazo (integers) y dos auxiliares (integers)
; Recorrido: Cartas faltantes (list)
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para obtener laas cartas faltantes en el mazo original
(define compareDecks
  (lambda (deck1 deck2 largo1 largo2 aux aux2)
    (if (equal? largo1 aux)
        '()
        (if (equal? largo2 aux2)
            (cons (getElemento deck1 aux)(compareDecks deck1 deck2 largo1 largo2 (+ aux 1) 0))
            (if (equal? #t (compareCard2 (getElemento deck1 aux) (getElemento deck2 aux2) (length (getElemento deck1 0)) 0 0 0 0))
                (compareDecks deck1 deck2 largo1 largo2 (+ aux 1) 0)
                (compareDecks deck1 deck2 largo1 largo2 aux (+ aux2 1)))))))

; Descripción: Función que permite comparar dos cartas, similar a la original pero con modificaciones
; Dominio: Carta número 1 (list), carta número 2 (list), el largo de las cartas (integer), dos auxiliares (integers), comparaciones (integer) y el total de igualdades (integer)
; Recorrido: Boolean
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para verificar si las 2 cartas son iguales
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

; Descripción: Función que representa en base string al mazo
; Dominio: Un set de cartas (list), el largo del mazo (integer) y un auxiliar (integer)
; Recorrido: Un string
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para crear el string
(define creaString
  (lambda (mazo largo aux)
    (if (equal? aux largo)
        "\n"
        (string-append (string-append (string-append (string-append "Carta " (number->string (+ aux 1))) ": ") (cartaString (getElemento mazo aux) 0)) (creaString mazo largo (+ aux 1))))))

; Descripción: Función que representa en base string a una carta
; Dominio: Una carta (list) y un auxiliar (integer)
; Recorrido: Un string
; Tipo de Recursión: Recursión Natural
; Justificación de Recursión: Necesaria para crear el string
(define cartaString
  (lambda (carta aux)
    (if (equal? (length carta) aux)
        "\n"
        (if (equal? (- (length carta) 1) aux)
            (if (number? (getElemento carta aux))
                (string-append (number->string (getElemento carta aux)) (cartaString carta (+ aux 1)))
                (string-append (getElemento carta aux) (cartaString carta (+ aux 1))))
            (if (number? (getElemento carta aux))
                (string-append (string-append (number->string (getElemento carta aux)) ", ") (cartaString carta (+ aux 1)))
                (string-append (string-append (getElemento carta aux) ", ") (cartaString carta (+ aux 1))))))))

; Descripción: Función random (no utilizada), se omite descripción
(define randomFn
  (lambda (xn)
    (modulo (+ (* a xn) c) m)))

; Función random
(define m 2147483647)
(define a 1103515245)
(define c 12345)

; Se utiliza provide para poder utilizar al TDA y sus funciones en otros archivos
(provide (all-defined-out))