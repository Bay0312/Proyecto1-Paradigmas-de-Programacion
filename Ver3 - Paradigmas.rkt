#lang racket


; --------------------------------------------------------------------------------
; ---------------------------------PARA DISPLAY-P---------------------------------
; --------------------------------------------------------------------------------
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
(define suma-polinomios
  (lambda (p1 p2)
    (cond
      [(null? p1) p2]
      [(null? p2) p1]
      [else (cons (+ (car p1) (car p2)) (suma-polinomios (cdr p1) (cdr p2)))])))

(define sumar-todos-polinomios
  (lambda (polinomios)
    (cond
      [(null? (cdr polinomios)) (car polinomios)]
      [else (suma-polinomios (car polinomios) (sumar-todos-polinomios (cdr polinomios)))])))

(define simplificar
  (lambda (polinomio)
    (cond
      [(null? polinomio) '()]  ;; Si el polinomio está vacío, devolver lista vacía
      [(zero? (last polinomio)) (simplificar (reverse (cdr (reverse polinomio))))]  ;; Si el último término es 0, lo eliminamos
      [else polinomio])))  ;; Si no hay ceros a la derecha, devolvemos el polinomio intacto


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
(define resta-polinomios
  (lambda (p1 p2)
    (cond
      [(null? p1) (map (lambda (coef) (- coef)) p2)] ;; Si p1 es nulo, devolver los negativos de p2
      [(null? p2) p1]                                ;; Si p2 es nulo, devolver p1
      [else (cons (- (car p1) (car p2)) (resta-polinomios (cdr p1) (cdr p2)))])))

(define restar-todos-polinomios
  (lambda (polinomios)
    (cond
      [(null? (cdr polinomios)) (car polinomios)]    ;; Si solo queda un polinomio, devolverlo
      [else (restar-todos-polinomios                 ;; Restar desde la izquierda progresivamente
              (cons (resta-polinomios (car polinomios) (cadr polinomios)) (cddr polinomios)))])))

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
;; Multiplica un término por un polinomio, ajustando los exponentes
(define multiplicar-termino-por-polinomio
  (lambda (coef exp polinomio)
    (cond
      [(null? polinomio) '()]   ;; Si el polinomio está vacío, devolver lista vacía
      [else (cons (* coef (car polinomio))  ;; Multiplicar coeficiente
                  (multiplicar-termino-por-polinomio coef (add1 exp) (cdr polinomio)))])))

;; Multiplica dos polinomios
(define multiplicar-dos-polinomios
  (lambda (p1 p2)
    (cond
      [(null? p1) '()]   ;; Si uno de los polinomios está vacío, devolver lista vacía
      [else (suma-polinomios  ;; Sumar el resultado de multiplicar el primer término de p1 por todo p2
             (multiplicar-termino-por-polinomio (car p1) 0 p2)
             (cons 0 (multiplicar-dos-polinomios (cdr p1) p2)))])))

;; Multiplicar todos los polinomios
(define multiplicar-todos-polinomios
  (lambda (polinomios)
    (cond
      [(null? (cdr polinomios)) (car polinomios)]  ;; Si solo queda un polinomio, devolverlo
      [else (multiplicar-todos-polinomios          ;; Multiplicar de manera progresiva
             (cons (multiplicar-dos-polinomios (car polinomios) (cadr polinomios))
                   (cddr polinomios)))])))

;; Simplificar y mostrar el polinomio resultante
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

; Deriva todos los polinomios dados
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

; Función recursiva que implementa el cálculo del cociente sin let
(define qt-p-recursivo
  (lambda (p1 p2 cociente)
    (if (< (grado p1) (grado p2))
        cociente
        (qt-p-recursivo
         (simplificar (resta-polinomios p1 (mult-monomio p2 (/ (coef-lider p1) (coef-lider p2)) (- (grado p1) (grado p2)))))
         p2
         (suma-polinomios cociente (append (make-list (- (grado p1) (grado p2)) 0) (list (/ (coef-lider p1) (coef-lider p2)))))))))

; Función principal para la división de polinomios y su despliegue sin let
(define qt-p
  (lambda (p1 p2)
    (display-p (simplificar (qt-p-recursivo p1 p2 '(0))))))

; --------------------------------------------------------------------------------
; ---------------------FINAL PARA DIVISION POLINOMIOS PARTE I---------------------
; --------------------------------------------------------------------------------


; --------------------------------------------------------------------------------
; -----------------------PARA DIVISION POLINOMIOS PARTE II------------------------
; --------------------------------------------------------------------------------

; Reutilizamos las funciones auxiliares definidas anteriormente
; (grado, coef-lider, mult-monomio)

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

; Función para mostrar el resultado
(define mostrar-rem-p
  (lambda (p1 p2)
    (display "Residuo: ")
    (rem-p p1 p2)))

; --------------------------------------------------------------------------------
; --------------------FINAL PARA DIVISION POLINOMIOS PARTE II---------------------
; --------------------------------------------------------------------------------


; --------------------------------------------------------------------------------
; -----------------------PARA DIVISION POLINOMIOS PARTE II------------------------
; --------------------------------------------------------------------------------
; Función /-p para devolver una lista con el cociente y el residuo de la división
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
    (horner (reverse p) x 0)))  ;; Invertimos la lista de coeficientes antes de usar Horner

; --------------------------------------------------------------------------------
; ------------------------FINAL PARA EVALUACION CON HORNER------------------------
; --------------------------------------------------------------------------------


; --------------------------------------------------------------------------------
; ------------------------PARA FACTORIZACION DE POLINOMIOS------------------------
; --------------------------------------------------------------------------------
; Función principal para factorizar polinomios
(define fact-p
  (lambda (pol)
    (cond
      [(= (length pol) 3) (fact-2g pol)]
      [(= (length pol) 4) (fact-3g pol)]
      [else (factorizar pol)])))

; Factorización de polinomios de grado 2
(define fact-2g
  (lambda (pol)
    (cond
      [(= (caddr pol) 0) pol]
      [(null? pol) pol]
      [else
       (list (list (cambiar-signo (formula-resta pol)) 1)
             (list (cambiar-signo (formula-suma pol)) 1))])))

; Fórmula general (parte resta)
(define formula-resta
  (lambda (pol)
    (cond
      [(= (caddr pol) 0) pol]
      [(null? pol) pol]
      [else (/ (- (cambiar-signo (cadr pol)) (raiz pol)) (* 2 (caddr pol)))])))

; Fórmula general (parte suma)
(define formula-suma
  (lambda (pol)
    (cond
      [(= (caddr pol) 0) pol]
      [(null? pol) pol]
      [else (/ (+ (cambiar-signo (cadr pol)) (raiz pol)) (* 2 (caddr pol)))])))

; Cálculo de la raíz para la fórmula general
(define raiz
  (lambda (pol)
    (cond
     [(null? pol) pol]
     [else
      (sqrt (- (expt (cadr pol) 2) (* 4 (* (caddr pol) (car pol)))))])))

; Cambiar signo de un número
(define cambiar-signo
  (lambda (num)
    (- num)))

; Factorización de polinomios de grado 3
(define fact-3g
  (lambda (pol)
    (factorizar pol)))

; Factorización de polinomios de grado superior
(define factorizar
  (lambda (pol)
    (cond
      [(= (car pol) 0) (metodo (reverse (cdr pol)) (divisores (hacer-positivo (cadr pol))) (list (list 0 1)))]
      [else
       (metodo (reverse pol) (divisores (hacer-positivo (car pol))) '())])))

; Método principal de factorización
(define metodo
  (lambda (pol divisores resultados)
    (cond
      [(null? pol) resultados]
      [(null? divisores) resultados]
      [else
       (if (verificar pol (car divisores) '())
           (metodo pol (cdr divisores) (cons (list (cambiar-signo (car divisores)) 1) resultados))
           (metodo pol (cdr divisores) resultados))])))

; Obtener divisores de un número
(define divisores
  (lambda (num)
    (append (map cambiar-signo (divisores-positivos num num '()))
            (divisores-positivos num num '()))))

; Obtener divisores positivos
(define divisores-positivos
  (lambda (num i lista)
    (cond
      [(= i 0) lista]
      [(= (modulo num i) 0) (cons i (divisores-positivos num (- i 1) lista))]
      [else (divisores-positivos num (- i 1) lista)])))

; Verificar si un divisor es válido
(define verificar
  (lambda (pol div res)
    (= (car (operacion-principal pol div res)) 0)))

; Operación principal del método de Ruffini
(define operacion-principal
  (lambda (pol div res)
    (cond
      [(null? pol) res]
      [(null? res) (operacion-principal (cdr pol) div (list (car pol)))]
      [else
       (operacion-principal (cdr pol) div 
                            (cons (desarrolla-operacion div (car res) (car pol))
                                  res))])))

; Desarrollo de la operación en el método de Ruffini
(define desarrolla-operacion
  (lambda (divisor res num-pol)
    (+ num-pol (* res divisor))))

; Hacer positivo un número
(define hacer-positivo
  (lambda (numero)
    (if (> numero 0)
        numero
        (abs numero))))


; --------------------------------------------------------------------------------
; ---------------------FINAL PARA FACTORIZACION DE POLINOMIOS---------------------
; --------------------------------------------------------------------------------



(fact-p '(4 4 1))
(fact-p '(-6 1 1))
(fact-p '(-72 -6 7 1))
(*p '(-3 1) '(4 1) '(6 1))




