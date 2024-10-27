#lang racket


; ---------------------------------------------------------------------
;
; (c) 2024
; EIF400 Paradigmas de Programación
; 2do ciclo 2024
; Proyecto #1
;
; 703000234-  Bayron Vega Alvarez
; 604860473 - Johan Mora Portuguez
;
;
; version 1.0.0 2024-10-19
;
; ---------------------------------------------------------------------


; --------------------------------------------------------------------------------
; ---------------------------------PARA DISPLAY-P---------------------------------
; --------------------------------------------------------------------------------
; Convierte un término del polinomio a su representación en string
(define mostrar-termino
  (lambda (coef exp)
    (cond
      [(zero? coef) ""]
      [(zero? exp) (number->string coef)]
      [(= exp 1)
       (cond
         [(= coef 1) "x"]
         [(= coef -1) "-x"]
         [else (string-append (number->string coef) "x")])]
      [else
       (cond
         [(= coef 1) (string-append "x^" (number->string exp))]
         [(= coef -1) (string-append "-x^" (number->string exp))]
         [else (string-append (number->string coef) "x^" (number->string exp))])])))

; Recorre el polinomio y acumula su representación en string
(define auxiliar
  (lambda (polinomio exp acumulador)
    (cond
      [(null? polinomio) (if (string=? acumulador "") "0" acumulador)]
      [else
       (auxiliar
        (cdr polinomio)
        (add1 exp)
        (cond
          [(string=? (mostrar-termino (car polinomio) exp) "") acumulador]
          [(string=? acumulador "") (mostrar-termino (car polinomio) exp)]
          [(positive? (car polinomio)) 
           (string-append acumulador " + " (mostrar-termino (car polinomio) exp))]
          [else
           (string-append acumulador " " (mostrar-termino (car polinomio) exp))]))])))

; Funcion principal de display-p. Muestra el polinomio en formato legible
(define display-p
  (lambda (polinomio)
    (display (auxiliar polinomio 0 ""))
    (newline)))


; --------------------------------------------------------------------------------
; ------------------------------FINAL PARA DISPLAY-P------------------------------
; --------------------------------------------------------------------------------


; --------------------------------------------------------------------------------
; ------------------------------PARA SUMA POLINOMIOS------------------------------
; --------------------------------------------------------------------------------
; Suma dos polinomios término a término
(define suma-polinomios
  (lambda (p1 p2)
    (cond
      [(null? p1) p2]
      [(null? p2) p1]
      [else (cons (+ (car p1) (car p2)) (suma-polinomios (cdr p1) (cdr p2)))])))

; Suma una lista de polinomios
(define sumar-todos-polinomios
  (lambda (polinomios)
    (cond
      [(null? (cdr polinomios)) (car polinomios)]
      [else (suma-polinomios (car polinomios) (sumar-todos-polinomios (cdr polinomios)))])))

; Elimina los ceros a la derecha del polinomio
(define simplificar
  (lambda (polinomio)
    (cond
      [(null? polinomio) '()] ; Si el polinomio está vacío, devolver lista vacía
      [(zero? (last polinomio)) (simplificar (reverse (cdr (reverse polinomio))))] ;; Si el último término es 0, lo eliminamos
      [else polinomio]))) ;; Si no hay ceros a la derecha, devolvemos el polinomio intacto

; Funcion principal de suma
(define +p
  (lambda polinomios
    (display-p
     (simplificar
      (sumar-todos-polinomios polinomios)))))

; --------------------------------------------------------------------------------
; ---------------------------FINAL PARA SUMA POLINOMIOS---------------------------
; --------------------------------------------------------------------------------

; --------------------------------------------------------------------------------
; ------------------------------PARA RESTA POLINOMIOS-----------------------------
; --------------------------------------------------------------------------------
; Resta dos polinomios término a término
(define resta-polinomios 
  (lambda (p1 p2)
    (cond
      [(null? p1) (map (lambda (coef) (- coef)) p2)] ; Si p1 es nulo se devuelven los negativos de p2
      [(null? p2) p1]                                ; Si p2 es nulo se devuelve p1
      [else (cons (- (car p1) (car p2)) (resta-polinomios (cdr p1) (cdr p2)))])))

; Resta una lista de polinomios
(define restar-todos-polinomios
  (lambda (polinomios)
    (cond
      [(null? (cdr polinomios)) (car polinomios)]    ; Si solo queda un polinomio este mismo se devuelve
      [else (restar-todos-polinomios                 ; Resta desde la izquierda progresivamente
              (cons (resta-polinomios (car polinomios) (cadr polinomios)) (cddr polinomios)))])))

; Funcion principal de resta
(define -p
  (lambda polinomios
    (display-p
     (simplificar
      (restar-todos-polinomios polinomios)))))



; --------------------------------------------------------------------------------
; ---------------------------FINAL PARA RESTA POLINOMIOS--------------------------
; --------------------------------------------------------------------------------

; --------------------------------------------------------------------------------
; -------------------------PARA MULTIPLICACION POLINOMIOS-------------------------
; --------------------------------------------------------------------------------
; Multiplica un término por un polinomio ajustando los exponentes
(define multiplicar-termino-por-polinomio
  (lambda (coef exp polinomio)
    (cond
      [(null? polinomio) '()]   ; Si el polinomio está vacío se devuelve una lista vacía
      [else (cons (* coef (car polinomio))  ;; Multiplicar coeficiente
                  (multiplicar-termino-por-polinomio coef (add1 exp) (cdr polinomio)))])))

; Multiplica dos polinomios
(define multiplicar-dos-polinomios
  (lambda (p1 p2)
    (cond
      [(null? p1) '()]   ; Si uno de los polinomios está vacío se devuelve una lista vacía
      [else (suma-polinomios  ; Suma el resultado de multiplicar el primer término de p1 por todo p2
             (multiplicar-termino-por-polinomio (car p1) 0 p2)
             (cons 0 (multiplicar-dos-polinomios (cdr p1) p2)))])))

; Multiplica una lista de polinomios
(define multiplicar-todos-polinomios
  (lambda (polinomios)
    (cond
      [(null? (cdr polinomios)) (car polinomios)]  ;; Si solo queda un polinomio, devolverlo
      [else (multiplicar-todos-polinomios          ;; Multiplicar de manera progresiva
             (cons (multiplicar-dos-polinomios (car polinomios) (cadr polinomios))
                   (cddr polinomios)))])))

; Funcion principal de multiplicación
(define *p
  (lambda polinomios
    (display-p
     (simplificar
      (multiplicar-todos-polinomios polinomios)))))



; --------------------------------------------------------------------------------
; ----------------------FINAL PARA MULTIPLICACION POLINOMIOS----------------------
; --------------------------------------------------------------------------------

; --------------------------------------------------------------------------------
; ---------------------------PARA DERIVACION POLINOMIOS---------------------------
; --------------------------------------------------------------------------------
; Deriva un solo polinomio
(define derivar-polinomio
  (lambda (polinomio)
    (derivar-polinomio-aux polinomio 0)))

; Función auxiliar para continuar la derivación
(define derivar-polinomio-aux
  (lambda (polinomio exp)
    (cond
      [(null? polinomio) '()]
      [(zero? exp) (derivar-polinomio-aux (cdr polinomio) (add1 exp))]  ; Saltamos el término constante
      [else (cons (* (car polinomio) exp)
                  (derivar-polinomio-aux (cdr polinomio) (add1 exp)))])))

; Deriva una lista de polinomios
(define derivar-todos-polinomios
  (lambda (polinomios)
    (cond
      [(null? polinomios) '()]
      [else (cons (derivar-polinomio (car polinomios))
                  (derivar-todos-polinomios (cdr polinomios)))])))

; Función principal para derivar polinomios
(define drv-p
  (lambda polinomios
    (map display-p (map simplificar (derivar-todos-polinomios polinomios)))(void)))

; --------------------------------------------------------------------------------
; ----------------------- FINAL PARA DERIVACION POLINOMIOS------------------------
; --------------------------------------------------------------------------------


; --------------------------------------------------------------------------------
; ------------------------PARA DIVISION POLINOMIOS PARTE I------------------------
; --------------------------------------------------------------------------------

; Función auxiliar para obtener el grado de un polinomio
(define grado
  (lambda (polinomio)
    (- (length polinomio) 1)))

; Función auxiliar para obtener el coeficiente líder de un polinomio
(define coef-lider
  (lambda (polinomio)
    (if (null? polinomio)
        0
        (car (reverse polinomio)))))

; Función auxiliar para multiplicar un polinomio por un monomio
(define mult-monomio
  (lambda (polinomio coef grado)
    (append (make-list grado 0)
            (map (lambda (x) (* x coef)) polinomio))))

; Función recursiva que implementa el cálculo del cociente
(define qt-p-recursivo
  (lambda (p1 p2 cociente)
    (if (< (grado p1) (grado p2))
        cociente
        (qt-p-recursivo
         (simplificar (resta-polinomios p1 (mult-monomio p2 (/ (coef-lider p1) (coef-lider p2)) (- (grado p1) (grado p2)))))
         p2
         (suma-polinomios cociente (append (make-list (- (grado p1) (grado p2)) 0) (list (/ (coef-lider p1) (coef-lider p2)))))))))

; Función principal para la división de polinomios y su despliegue
(define qt-p
  (lambda (p1 p2)
    (if (= (coef-lider p2) 0)
        (displayln "División por cero - No puedes dividir un polinomio entre 0")
        (display-p (simplificar (qt-p-recursivo p1 p2 '(0)))))))

; --------------------------------------------------------------------------------
; ---------------------FINAL PARA DIVISION POLINOMIOS PARTE I---------------------
; --------------------------------------------------------------------------------


; --------------------------------------------------------------------------------
; -----------------------PARA DIVISION POLINOMIOS PARTE II------------------------
; --------------------------------------------------------------------------------

; Función recursiva que implementa el cálculo del residuo
(define rem-p-recursivo
  (lambda (p1 p2)
    (if (< (grado p1) (grado p2))
        p1
        (rem-p-recursivo
         (simplificar 
          (resta-polinomios 
           p1 
           (mult-monomio p2 
                         (/ (coef-lider p1) (coef-lider p2)) 
                         (- (grado p1) (grado p2)))))
         p2))))

; Función principal para el cálculo del residuo
(define rem-p
  (lambda (p1 p2)
    (if (= (coef-lider p2) 0)
        (displayln "División por cero - No puedes dividir un polinomio entre 0")
        (display-p (simplificar (rem-p-recursivo p1 p2))))))

; --------------------------------------------------------------------------------
; --------------------FINAL PARA DIVISION POLINOMIOS PARTE II---------------------
; --------------------------------------------------------------------------------


; --------------------------------------------------------------------------------
; -----------------------PARA DIVISION POLINOMIOS PARTE III-----------------------
; --------------------------------------------------------------------------------
; Los polinomios se simplifican para mostrarse en /-p (se eliminan 0).
; Esta funcion asegura que se mantenga el 0 en la lista si esto fue devuelto.
(define asegurar-cero
  (lambda (polinomio)
    (if (null? polinomio)
        '(0)
        polinomio)))

; Función /-p para devolver una lista con el cociente y el residuo de la división.
; El primer elemento de la lista correspone al cociente, el segundo al residuo.
(define /-p
  (lambda (p1 p2)
    (if (= (coef-lider p2) 0)
        (displayln "División por cero - No puedes dividir un polinomio entre 0")
        (list (asegurar-cero (simplificar (qt-p-recursivo p1 p2 '(0))))  ; El cociente
              (asegurar-cero (simplificar (rem-p-recursivo p1 p2)))))))   ; El residuo

; --------------------------------------------------------------------------------
; -------------------FINAL PARA DIVISION POLINOMIOS PARTE III---------------------
; --------------------------------------------------------------------------------


; --------------------------------------------------------------------------------
; ---------------------------PARA EVALUACION CON HORNER---------------------------
; --------------------------------------------------------------------------------
; Función auxiliar para aplicar el algoritmo de Horner
(define horner
  (lambda (p x acumulador)
    (if (null? p)
        acumulador
        (horner (cdr p) x (+ (* acumulador x) (car p))))))

; Función principal eval-p que evalúa el polinomio p en el valor x
(define eval-p
  (lambda (p x)
    (horner (reverse p) x 0)))  ; Invertimos la lista de coeficientes antes de usar Horner

; --------------------------------------------------------------------------------
; ------------------------FINAL PARA EVALUACION CON HORNER------------------------
; --------------------------------------------------------------------------------


; --------------------------------------------------------------------------------
; ------------------------PARA FACTORIZACION DE POLINOMIOS------------------------
; --------------------------------------------------------------------------------
; ---------------------------------------
; --------------CUADRATICA---------------
; ---------------------------------------
; Función para resolver ecuaciones cuadráticas de la forma ax2+bx+c=0ax2+bx+c=0 utilizando la fórmula cuadrática.
(define resolver-ecuacion-cuadratica
  (lambda (a b c)
    ((lambda (discriminante) 
       (if (negative? discriminante)
           #f  ; No hay raíces reales
           ((lambda (raiz-discriminante)
              (list (/ (+ (- b) raiz-discriminante) (* 2 a))
                    (/ (- (- b) raiz-discriminante) (* 2 a))))
            (sqrt (abs discriminante)))))
     (- (* b b) (* 4 a c)))))

; Función para factorizar un polinomio cuadrático, si es posible, en términos lineales o devuelve el polinomio original si no se puede factorizar.
(define fact-polinomio-cuadratico
  (lambda (polinomio)
    ((lambda (a b c)
       (if (zero? a)
           (list polinomio)  ; No es un polinomio cuadrático
           ((lambda (raices)
              (if raices
                  ((lambda (factor1 factor2)
                     (if (= a 1)
                         (list factor1 factor2)
                         (list (list a) factor1 factor2)))
                   (crear-factor-lineal (car raices))
                   (crear-factor-lineal (cadr raices)))
                  (list polinomio))) ; No se puede factorizar con raíces reales
            (resolver-ecuacion-cuadratica a b c))))
     (if (and (pair? polinomio) (pair? (cdr polinomio)) (pair? (cddr polinomio))) (caddr polinomio) 0)
     (if (and (pair? polinomio) (pair? (cdr polinomio))) (cadr polinomio) 0)
     (if (pair? polinomio) (car polinomio) 0))))

; ---------------------------------------
; ----------------CUBICA-----------------
; ---------------------------------------
; Función para factorizar un polinomio cúbico utilizando la regla de Ruffini y buscando raíces racionales.
(define factorizar-polinomio-cubico
  (lambda (p)
    (define p-limpio (limpiar-polinomio (simplificar p)))  ;; Limpiar ceros iniciales
    (define raiz (encontrar-raiz-racional p-limpio))
    (if raiz
        (cons (crear-factor-lineal raiz) 
              (fact-p (division-sintetica p-limpio raiz)))  ;; Llamada directa a fact-p
        (list p-limpio))))  ;; Devuelve el polinomio limpio

; ---------------------------------------
; --------------AUXILIARES---------------
; ---------------------------------------
; Función para obtener una lista de posibles raíces de un polinomio basado en su término independiente.
(define obtener-posibles-raices
  (lambda (c)
    (define calcular-raices
      (lambda (divisor res)
        (cond
          [(<= divisor 1) (append res (list 1))] ; Verifica que el divisor sea mayor que 0
          [(equal? (modulo c divisor) 0) (calcular-raices (sub1 divisor) (append res (list divisor)))]
          [else (calcular-raices (sub1 divisor) res)])))
    (calcular-raices (sub1 (abs c)) (list c))))

; Función para factorizar un polinomio que no tiene término independiente, utilizando un factor común y las raíces encontradas.
(define factorizar-sin-termino-independiente
  (lambda (polinomio)
    (define factor-comun (obtener-factor-comun (cdr polinomio)))
    
    (define realizar-factorizacion
      (lambda (raices res)
        (if (null? raices)
            res
            (realizar-factorizacion (cdr raices) (append res (list (list (* (car raices) -1) 1)))))))
    
    (realizar-factorizacion (obtener-raices-ruffini (cdr polinomio)) (list factor-comun))))

; Función para aplicar la regla de Ruffini para encontrar raíces racionales de un polinomio.
(define obtener-raices-ruffini
  (lambda (polinomio)
    (define posibles-raices (reverse (obtener-posibles-raices (first polinomio))))
    
    (define realizar-evaluacion-ruffini
      (lambda (p denominador cociente divisor signo pol raices raices-restantes)
        (cond
          [(null? p)
           (cond
             [(equal? signo +)
              (if (equal? (last cociente) 0)
                  (realizar-ruffini (reverse (cdr (reverse cociente))) (append raices (list divisor)) raices-restantes -)
                  (realizar-ruffini pol raices raices-restantes -))]
             [else
              (if (equal? (last cociente) 0)
                  (realizar-ruffini (reverse (cdr (reverse cociente))) (append raices (list (* divisor -1))) (cdr raices-restantes) +)
                  (realizar-ruffini pol raices (cdr raices-restantes) +))])]
          [else
           (realizar-evaluacion-ruffini (cdr p) 
                                        (append denominador (list (* (last cociente) (if (equal? signo -) (* divisor -1) divisor)))) 
                                        (append cociente (list (+ (car p) (* (last cociente) (if (equal? signo -) (* divisor -1) divisor)))))
                                        divisor signo pol raices raices-restantes)])))
    
    (define realizar-ruffini
      (lambda (pol raices raices-restantes signo)
        (cond
          [(null? raices-restantes) raices]
          [else
           (realizar-evaluacion-ruffini (cdr pol) '() (list (car pol)) (car raices-restantes) signo pol raices raices-restantes)])))
    
    (realizar-ruffini (reverse polinomio) '() posibles-raices +)))

; Función para factorizar un polinomio a partir de sus raíces encontradas.
(define factorizar-con-raices
  (lambda (polinomio obtener-raices)
    (define realizar-factorizacion
      (lambda (raices res)
        (if (null? raices)
            res
            (realizar-factorizacion (cdr raices) (append res (list (list (* (car raices) -1) 1)))))))
    (realizar-factorizacion (obtener-raices polinomio) '())))

; Función para calcular el máximo común divisor de los coeficientes de un polinomio.
(define obtener-mcd
  (lambda (polinomio)
    (if (equal? (length polinomio) 1)
        (car polinomio)
        (gcd (car polinomio) (obtener-mcd (cdr polinomio))))))

; Función para encontrar el factor común en los coeficientes de un polinomio.
(define obtener-factor-comun
  (lambda (polinomio)
    (define factor-comun
      (lambda (comun-div p ct)
        (if (not (equal? (car p) 0))
            (append (make-list (add1 ct) 0) (list comun-div))
            (factor-comun comun-div (cdr p) (add1 ct)))))
    (factor-comun (obtener-mcd polinomio) polinomio 0)))

; Función para realizar la división sintética de un polinomio por un número real, devolviendo el cociente.
(define division-sintetica
  (lambda (p r)
    (define (sintetizar p acc)
      (if (null? (cdr p))
          (reverse (cdr acc))  ;; Devolver el cociente sin el residuo
          (sintetizar (cdr p) (cons (+ (car (cdr p)) (* r (car acc))) acc))))
    (reverse (sintetizar (reverse p) (list (car (reverse p)))))))

; Función para generar una lista de divisores de un número entero.
(define divisores
  (lambda (n)
    (filter (lambda (i) (zero? (remainder n i))) (range 1 (add1 (abs n))))))

; Función para encontrar la primera raíz racional que satisface un polinomio dado, utilizando el teorema de las raíces racionales.
(define encontrar-raiz-racional
  (lambda (p)
    (define num (abs (first p)))  ;; Término constante
    (define den (abs (last p)))   ;; Coeficiente de mayor grado

    ;; Función para generar las divisiones positivas y negativas
    (define generar-raices
      (lambda (divisores-num divisores-den)
        (append
          (generar-positivos divisores-num divisores-den)
          (generar-negativos divisores-num divisores-den))))

    ;; Generar raíces positivas
    (define generar-positivos
      (lambda (divisores-num divisores-den)
        (if (null? divisores-num)
            '()
            (append
              (map (lambda (d) (/ (car divisores-num) d)) divisores-den)
              (generar-positivos (cdr divisores-num) divisores-den)))))

    ;; Generar raíces negativas
    (define generar-negativos
      (lambda (divisores-num divisores-den)
        (if (null? divisores-num)
            '()
            (append
              (map (lambda (d) (- (/ (car divisores-num) d))) divisores-den)
              (generar-negativos (cdr divisores-num) divisores-den)))))

    ;; Buscar la primera raíz racional que satisfaga el polinomio
    (define buscar-raiz
      (lambda (raices)
        (cond
          ((null? raices) #f)
          ((zero? (eval-p p (car raices))) (car raices))
          (else (buscar-raiz (cdr raices))))))

    ;; Ejecuta la búsqueda
    (buscar-raiz (generar-raices (divisores num) (divisores den)))))


; Función para eliminar ceros a la izquierda en un polinomio, devolviendo una lista más limpia.
(define limpiar-polinomio
  (lambda (p)
    (filtrar-hasta zero? p)))

; Función para eliminar los elementos de una lista mientras un predicado dado sea verdadero.
(define filtrar-hasta
  (lambda (pred lst)
    (cond
      [(null? lst) '()]
      [(pred (car lst)) (filtrar-hasta pred (cdr lst))]
      [else lst])))

; Función para crear un factor lineal de la forma (x-r)(x-r) dado un valor de raíz r.
(define crear-factor-lineal
  (lambda (raiz)
    (list (- raiz) 1)))
 
; Función para decidir el método de factorización a aplicar a un polinomio según su grado y estructura.
(define fact-p
  (lambda (polinomio)
    (if (and (= (length polinomio) 4)  ; Verifica que el polinomio tenga 4 coeficientes
             (zero? (first polinomio)))  ; El primer coeficiente debe ser 0
        (factorizar-sin-termino-independiente polinomio)  ; Utiliza CODIGO 1
        (cond
          [(= (grado polinomio) 2)
           (fact-polinomio-cuadratico polinomio)]  ;; Grado 2
          [(= (grado polinomio) 3)
           (factorizar-polinomio-cubico polinomio)]  ;; Grado 3
          [else 
           (if (equal? (first polinomio) 0)
               (factorizar-sin-termino-independiente polinomio)  ; Usar factorización sin término independiente
               (factorizar-con-raices polinomio obtener-raices-ruffini))]))))  ; Factorización con Ruffini
; --------------------------------------------------------------------------------
; ---------------------FINAL PARA FACTORIZACION DE POLINOMIOS---------------------
; --------------------------------------------------------------------------------

; Ejemplos de uso:

; (display-p p)
(display "(display-p p)") (newline) (newline)
(display-p '(1 0 -3))
(display-p '(0 4 -5 0 2))
(display-p '(2 0 0 -6 0 0 1))

(newline)
; (+p p1 p2 ...)
(display "(+p p1 p2 ...)") (newline) (newline)
(+p '(1 2 3) '(1 2 3))
(+p '(1 0 3) '(0 2 4) '(0 0 -1 1))
(+p '(2 3 4 0 1) '(0 0 1) '(-1 -1 -1 -1 -1))

(newline)
; (-p p1 p2 ...)
(display "(-p p1 p2 ...)") (newline) (newline)
(-p '(3 4 5) '(1 2 3)) 
(-p '(3 0 1) '(1 1 1)) 
(-p '(5 0 0 2) '(1 1) '(0 2 0 1)) 

(newline)
; (*p p1 p2 ...))
(display "(*p p1 p2 ...)") (newline) (newline)
(*p '(1 2) '(1 3)) 
(*p '(1 0 -1) '(0 2)) 
(*p '(2 1 0) '(1 -1) '(0 3)) 

(newline)
; (qt-p p1 p2)
(display "(qt-p p1 p2)") (newline) (newline)
(qt-p '(1 2) '(1)) 
(qt-p '(2 4 2) '(1 1)) 
(qt-p '(3 6 2) '(1 1 1)) 

(newline)
; (rem-p p1 p2)
(display "(rem-p p1 p2)")(newline)
(display "Nota: rem-p devolverá el dividendo si la operación no es realizable.")
(newline)(newline)
(rem-p '(1 2) '(1)) 
(rem-p '(2 4 2) '(1 1)) 
(rem-p '(3 6 2) '(1 1 1)) 

(newline)
; (/-p p1 p2)
(display "(/-p p1 p2)") (newline) (newline)
(/-p '(1 2) '(1)) 
(/-p '(2 4 2) '(1 1)) 
(/-p '(3 6 2) '(1 1 1)) 

(newline)
; (drv-p p1 p2 ...)
(display "(drv-p p1 p2 ...)") (newline) (newline)
(drv-p '(1 2 3)) 
(drv-p '(0 3 0 -2)) 
(drv-p '(2 0 0 -3 1)) 

(newline)
; (eval-p p x)
(display "(eval-p p x)") (newline) (newline)
(eval-p '(1 2 3) 1) 
(eval-p '(1 0 -2 1) 2) 
(eval-p '(3 0 0 4 -1) 3) 

(newline)
; (fact-p p1)
(display "(fact-p p1)") (newline) (newline)
(fact-p '(0 3 -1 -3 1))
(fact-p '(4 0 -5 0 1))
(fact-p '(6 -1 -7 1 1))
(fact-p '(0 -10 -13 -2 1))
(fact-p '(0 2 0 -2))
(fact-p '(1 0 -1))
(fact-p '(1 -6 11 -6))
(fact-p '(-120 274 -225 85 -15 1))
(fact-p  '(720 -1764 1624 -735 175 -21 1))
(fact-p '(3628800 -10628640 12753576 -8409500 3416930 -902055 157773 -18150 1320 -55 1))
