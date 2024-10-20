# Proyecto #1 - EIF-400: Paradigmas de Programación

## Estudiantes:
- Johan Mora Portuguez - 604860473
- Bayron Vega Álvarez - 703000234

## Descripción del Proyecto
Este proyecto implementa funciones en **Racket** para la suma, resta, multiplicación, división, derivación, evaluación y factorización de polinomios de grado 2 y 3 utilizando técnicas como la **fórmula cuadrática** y la **regla de Ruffini**. 

- **Lenguaje**: Racket
- **Objetivo**: Desarrollar funciones para hacer operaciones basicas e intermedias con polinomios.

## Funcionalidades principales
- Despliegue de funciones.
- Factorización de polinomios de grados 2, 3 y superior.
- Simplificación de polinomios eliminando términos innecesarios.
- Evaluación de polinomios usando el algoritmo de Horner.
- Suma, resta, division y multiplicacion de polinomios.
- Derivación de polinomios.

## Requerimientos
- **Racket** instalado en el sistema.

## Instrucciones
1. Clonar el repositorio o descargar el archivo 'Proyecto Polinomios.rkt'
2. Ejecutar el archivo en DrRacket o desde la línea de comandos con Racket.
3. Probar las funciones con distintos polinomios.

## Nota
En el proyecto los polinomios se interpretan a partir de las listas inversamente. Es decir, el primer numero de la lista es la constante, y conforme avanzamos encontramos los coeficientes que acompañan a las x de mayor grado. Por ejemplo, la lista '(1 2 3 4) representa: 1 + 2x + 3x^2 + 4x^3.
Por otra parte, la función 'rem-p' que calcula el residuo de una división de polinomios, en caso de no ser posible resolver la división devolverá el dividendo como residuo.

## Ejemplo de Uso
```racket
(fact-p '(4 4 1))      ; Factoriza 4 + 4x + x^2
(fact-p '(0 2 0 -2))   ; Factoriza x - 2x^3
(eval-p '(1 2 3) 2)    ; Evalúa 1 + 2x + 3x^2 en x = 2
(drv-p '(18 0 9 0 6))  ; Deriva 18 + 9x^2 + 6x^4
(/-p '(8 -5 4 1) '(1 -2 1)) ;Divide 8 -5x + 4x^2 + x^3 entre 1 - 2x + x^2. Entrega listas con cociente y residuo.
(display-p '(6 1)) ;Muestra en forma de polinomio el cociente de la division anterior.
(display-p '(2 6)) ;Muestra en forma de polinomio el residuo de la division anterior.
