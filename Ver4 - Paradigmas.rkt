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
;; Función para resolver ecuaciones cuadráticas
(define resolver-cuadratica
  (lambda (a b c)
    ((lambda (discriminante) 
       (if (negative? discriminante)
           #f  ; No hay raíces reales
           ((lambda (raiz-discriminante)
              (list (/ (+ (- b) raiz-discriminante) (* 2 a))
                    (/ (- (- b) raiz-discriminante) (* 2 a))))
            (sqrt (abs discriminante)))))
     (- (* b b) (* 4 a c)))))

;; Función para crear un factor lineal de la forma (x - r)
(define crear-factor-lineal
  (lambda (raiz)
    (list (- raiz) 1)))

;; Función principal para factorizar polinomios de grado 2
(define fact-p-cuadratico
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
            (resolver-cuadratica a b c))))
     (if (and (pair? polinomio) (pair? (cdr polinomio)) (pair? (cddr polinomio))) (caddr polinomio) 0)
     (if (and (pair? polinomio) (pair? (cdr polinomio))) (cadr polinomio) 0)
     (if (pair? polinomio) (car polinomio) 0))))

;; Función para encontrar divisores de un número
(define divisores
  (lambda (n)
    (filter (lambda (i) (zero? (remainder n i))) (range 1 (add1 (abs n))))))

;; Función para encontrar una raíz racional del polinomio
(define encontrar-raiz-racional
  (lambda (p)
    (define num (abs (first p)))  ;; Término constante
    (define den (abs (last p)))   ;; Coeficiente de mayor grado
    (define posibles-raices
      (append
       (for/list ([n (in-list (divisores num))] [d (in-list (divisores den))])
         (/ n d))
       (for/list ([n (in-list (divisores num))] [d (in-list (divisores den))])
         (- (/ n d))))) ;; Considera las raíces positivas y negativas
    (for/or ([r posibles-raices])
      (if (zero? (eval-p p r)) r #f)))) ;; Evalúa el polinomio en cada raíz

;; Función para la división sintética del polinomio
(define division-sintetica
  (lambda (p r)
    (define (helper p acc)
      (if (null? (cdr p))
          (reverse (cdr acc))  ;; Devolver el cociente sin el residuo
          (helper (cdr p) (cons (+ (car (cdr p)) (* r (car acc))) acc))))
    (reverse (helper (reverse p) (list (car (reverse p)))))))

;; Simplifica los polinomios eliminando ceros a la izquierda
(define limpiar-polinomio
  (lambda (p)
    (drop-while zero? p)))

;; Implementación propia de drop-while
(define drop-while
  (lambda (pred lst)
    (cond
      [(null? lst) '()]
      [(pred (car lst)) (drop-while pred (cdr lst))]
      [else lst])))

;; Factorización cúbica actualizada para manejar signos y ceros iniciales
(define factorizar-cubico
  (lambda (p)
    (define p-limpio (limpiar-polinomio (simplificar p)))  ;; Limpiar ceros iniciales
    (define raiz (encontrar-raiz-racional p-limpio))
    (if raiz
        (let ((coef-reducidos (division-sintetica p-limpio raiz)))
          (let ((factores-cuadraticos (fact-p coef-reducidos)))
            (cons (crear-factor-lineal raiz) factores-cuadraticos)))
        (list p-limpio))))  ;; No se puede factorizar

;; Función fact-p actualizada para manejar tanto grado 2 como grado 3
(define fact-p
  (lambda (polinomio)
    (cond
      [(= (grado polinomio) 2) (fact-p-cuadratico polinomio)]  ;; Grado 2
      [(= (grado polinomio) 3) (factorizar-cubico polinomio)]  ;; Grado 3
      [else (list polinomio)])))  ;; Si no es grado 2 o 3, devolvemos el polinomio sin factorizar


; --------------------------------------------------------------------------------
; ---------------------FINAL PARA FACTORIZACION DE POLINOMIOS---------------------
; --------------------------------------------------------------------------------






(*p '(-1 1) '(-2 1) '(2 1))
(fact-p '(-2 -5 18 45))
(fact-p '(-18 -6 3 1))
(fact-p '(0 2 0 -2))
(fact-p '(4 4 1))
(*p '(-1 1) '(-2 -2))
(fact-p '(8 4 6 3))
(fact-p '(-6 11 -6 1))
(*p '(-1 1) '(-3 1) '(-2 1))
(fact-p '(4 0 -3 1))
(*p '(1 1) '(-2 1) '(-2 1))
(fact-p '(2 6 6 2))

(fact-p '(-6 1 1))  ;'((3 1) (-2 1))
(fact-p '(-72 -6 7 1)) ;  '((-3 1) (4 1) (6 1))
(fact-p '(0 2 0 -2))
(*p '(2) '(-1 1) '(-2 -2))
(fact-p '(-5 -8 3))
(fact-p '(-5 4 1))
(fact-p '(-8 -2 1))
(fact-p '(20 -14 2))