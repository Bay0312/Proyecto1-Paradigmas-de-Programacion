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
    (display-p (simplificar (qt-p-recursivo p1 p2 '(0))))))

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
    (display-p (simplificar (rem-p-recursivo p1 p2)))))

; --------------------------------------------------------------------------------
; --------------------FINAL PARA DIVISION POLINOMIOS PARTE II---------------------
; --------------------------------------------------------------------------------


; --------------------------------------------------------------------------------
; -----------------------PARA DIVISION POLINOMIOS PARTE III------------------------
; --------------------------------------------------------------------------------
; Función /-p para devolver una lista con el cociente y el residuo de la división.
; El primer elemento de la lista correspone al cociente, el segundo al residuo.
(define /-p
  (lambda (p1 p2)
    (list (simplificar (qt-p-recursivo p1 p2 '(0)))  ; El cociente
          (simplificar (rem-p-recursivo p1 p2)))))   ; El residuo

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
; Calcula el discriminante de un polinomio de segundo grado
(define calcular-discriminante
  (lambda (a b c)
    (- (* b b) (* 4 a c))))

; Calcula la raíz de una ecuación cuadrática usando el signo y el discriminante
(define calcular-raiz-cuadratica
  (lambda (signo a b discriminante)
    (/ (signo (* b -1) (sqrt discriminante)) (* 2 a))))

; Factoriza un polinomio de segundo grado
(define factorizar-segundo-grado
  (lambda (polinomio)
    (define a (car (cdr (cdr polinomio))))
    (define b (car (cdr polinomio)))
    (define c (car polinomio))
    (define disc (calcular-discriminante a b c))

    (define obtener-raices
      (lambda (a b disc)
        (list (list (* -1 (calcular-raiz-cuadratica + a b disc)) 1)
              (list (* -1 (calcular-raiz-cuadratica - a b disc)) 1))))

    (if (>= disc 0)
        (obtener-raices a b disc)
        "Este polinomio contiene raices imaginarias...")))

(define obtener-posibles-raices
  (lambda (c)
    (define calcular-raices
      (lambda (divisor res)
        (cond
          [(<= divisor 1) (append res (list 1))] ; Verifica que el divisor sea mayor que 0
          [(equal? (modulo c divisor) 0) (calcular-raices (sub1 divisor) (append res (list divisor)))]
          [else (calcular-raices (sub1 divisor) res)])))
    (calcular-raices (sub1 (abs c)) (list c))))

; Aplica la regla de Ruffini para encontrar raíces racionales
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

; Factoriza un polinomio a partir de sus raíces
(define factorizar-con-raices
  (lambda (polinomio obtener-raices)
    (define realizar-factorizacion
      (lambda (raices res)
        (if (null? raices)
            res
            (realizar-factorizacion (cdr raices) (append res (list (list (* (car raices) -1) 1)))))))
    (realizar-factorizacion (obtener-raices polinomio) '())))

; Obtiene el máximo común divisor de los coeficientes de un polinomio
(define obtener-mcd
  (lambda (polinomio)
    (if (equal? (length polinomio) 1)
        (car polinomio)
        (gcd (car polinomio) (obtener-mcd (cdr polinomio))))))

; Encuentra el factor común de un polinomio
(define obtener-factor-comun
  (lambda (polinomio)
    (define factor-comun
      (lambda (comun-div p ct)
        (if (not (equal? (car p) 0))
            (append (make-list (add1 ct) 0) (list comun-div))
            (factor-comun comun-div (cdr p) (add1 ct)))))
    (factor-comun (obtener-mcd polinomio) polinomio 0)))

; Factoriza un polinomio cuando no tiene término independiente
(define factorizar-sin-termino-independiente
  (lambda (polinomio)
    (define factor-comun (obtener-factor-comun (cdr polinomio)))
    
    (define realizar-factorizacion
      (lambda (raices res)
        (if (null? raices)
            res
            (realizar-factorizacion (cdr raices) (append res (list (list (* (car raices) -1) 1)))))))
    
    (realizar-factorizacion (obtener-raices-ruffini (cdr polinomio)) (list factor-comun))))

; Función principal que decide el método de factorización según el polinomio
(define fact-p
  (lambda (polinomio)
    (cond
      [(<= (length polinomio) 2) polinomio] ; No hace nada si la lista tiene 0, 1 o 2 elementos
      [(and (= (length polinomio) 3) (not (equal? (third polinomio) 0)))
       (factorizar-segundo-grado polinomio)] ; Factorización de segundo grado
      [else 
       (if (equal? (first polinomio) 0)
           (factorizar-sin-termino-independiente polinomio) ; Factorización sin término independiente
           (factorizar-con-raices polinomio obtener-raices-ruffini))]))) ; Factorización con Ruffini


; --------------------------------------------------------------------------------
; ---------------------FINAL PARA FACTORIZACION DE POLINOMIOS---------------------
; --------------------------------------------------------------------------------
