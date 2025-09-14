# 📐 Calculadora de Polinomios — Racket

> **Proyecto académico** para EIF-400: Paradigmas de Programación. Implementación completa de operaciones algebraicas con polinomios usando **programación funcional** en Racket.

[![Racket](https://img.shields.io/badge/Racket-9F1D20?style=flat&logo=racket&logoColor=white)](https://racket-lang.org/)
[![Academic](https://img.shields.io/badge/Proyecto-Académico-green?style=flat)](https://github.com)

---

## 👥 Equipo de Desarrollo

<div align="center">
<table>
<tr>
<th>🧑‍💻 Estudiante</th>
</tr>
<tr>
<td><b>Johan Mora Portuguez</b></td>
</tr>
<tr>
<td><b>Bayron Vega Álvarez</b></td>
</tr>
</table>
</div>

---

## 🎯 Descripción del Proyecto

Este proyecto implementa una **calculadora completa de polinomios** en Racket, aplicando principios de **programación funcional** para realizar operaciones algebraicas avanzadas con polinomios de grado 2, 3 y superior.

### 🔬 Algoritmos Implementados
- **Fórmula Cuadrática** para factorización de polinomios de grado 2
- **Regla de Ruffini** para factorización de polinomios de grado superior
- **Algoritmo de Horner** para evaluación eficiente de polinomios
- **División sintética** para operaciones de división entre polinomios

---

## ⚡ Funcionalidades Principales

<div align="center">
<table>
<tr>
<td align="center">➕<br><b>Operaciones Básicas</b><br>Suma, resta, multiplicación<br>y división de polinomios</td>
<td align="center">🧮<br><b>Factorización</b><br>Grados 2, 3 y superior<br>usando métodos algebraicos</td>
<td align="center">📊<br><b>Evaluación</b><br>Algoritmo de Horner<br>para eficiencia óptima</td>
<td align="center">📈<br><b>Derivación</b><br>Cálculo automático<br>de derivadas</td>
</tr>
</table>
</div>

### 🛠️ Características Técnicas
- **🎨 Display inteligente** de polinomios en formato matemático
- **🔧 Simplificación automática** eliminando términos innecesarios  
- **⚡ Optimización** mediante programación funcional
- **🧪 Casos de prueba** integrados para validación

---

## 📋 Requerimientos

| Componente | Versión | Descripción |
|------------|---------|-------------|
| **Racket** | 8.0+ | Entorno de desarrollo principal |
| **DrRacket** | Incluido | IDE recomendado para ejecución |
| **Sistema** | Windows/Linux/macOS | Multiplataforma |

### 📥 Instalación de Racket
```bash
# Ubuntu/Debian
sudo apt-get install racket

# macOS (Homebrew)
brew install racket

# Windows: Descargar desde https://racket-lang.org/
```

---

## 🚀 Instrucciones de Uso

### 1️⃣ **Obtener el Proyecto**
```bash
# Opción 1: Clonar repositorio
git clone [URL_DEL_REPOSITORIO]
cd proyecto-polinomios

# Opción 2: Descargar archivo directo
# Descargar 'Proyecto Polinomios.rkt'
```

### 2️⃣ **Ejecutar en DrRacket**
1. Abrir **DrRacket**
2. Cargar el archivo `Proyecto Polinomios.rkt`
3. Presionar **Run** o `Ctrl+R`

### 3️⃣ **Ejecutar desde Terminal**
```bash
racket "Proyecto Polinomios.rkt"
```

---

## 📚 Representación de Polinomios

> **⚠️ Importante:** Los polinomios se representan en **orden inverso** al estándar matemático.

### 🔢 Formato de Lista
```racket
Lista: '(a₀ a₁ a₂ a₃ ... aₙ)
Representa: a₀ + a₁x + a₂x² + a₃x³ + ... + aₙxⁿ
```

### 📝 Ejemplos de Representación
| Lista | Polinomio Matemático | Descripción |
|-------|---------------------|-------------|
| `'(1 2 3 4)` | `1 + 2x + 3x² + 4x³` | Grado 3 |
| `'(4 4 1)` | `4 + 4x + x²` | Grado 2 |
| `'(0 2 0 -2)` | `2x - 2x³` | Términos con coeficiente 0 |
| `'(5)` | `5` | Polinomio constante |

---

## 🧪 Ejemplos de Uso

<details>
<summary><b>🔍 Ver ejemplos completos</b></summary>

### ➕ **Operaciones Básicas**
```racket
;; Suma de polinomios
(sum-p '(1 2 3) '(4 5 6))    ; (1+2x+3x²) + (4+5x+6x²) = 5+7x+9x²

;; Resta de polinomios  
(sub-p '(5 7 9) '(1 2 3))    ; (5+7x+9x²) - (1+2x+3x²) = 4+5x+6x²

;; Multiplicación
(mul-p '(1 1) '(1 1))        ; (1+x)(1+x) = 1+2x+x²
```

### 🧮 **Factorización**
```racket
;; Factorización cuadrática
(fact-p '(4 4 1))            ; 4 + 4x + x² → factores
(fact-p '(-6 1 1))           ; -6 + x + x² → factores

;; Factorización cúbica
(fact-p '(0 2 0 -2))         ; 2x - 2x³ → factores
```

### 📊 **Evaluación y Derivación**
```racket
;; Evaluación usando Horner
(eval-p '(1 2 3) 2)          ; Evalúa 1+2x+3x² en x=2 → 17

;; Derivación
(drv-p '(18 0 9 0 6))        ; Deriva 18+9x²+6x⁴ → 18x+24x³
```

### ➗ **División de Polinomios**
```racket
;; División con cociente y residuo
(/-p '(8 -5 4 1) '(1 -2 1))  ; Divide (8-5x+4x²+x³)÷(1-2x+x²)

;; Mostrar resultados en formato matemático
(display-p '(6 1))           ; Muestra: 6 + x
(display-p '(2 6))           ; Muestra: 2 + 6x
```

</details>

---

## 📖 Documentación de Funciones

<div align="center">
<table>
<tr>
<th>🔧 Función</th>
<th>📝 Descripción</th>
<th>📊 Ejemplo</th>
</tr>
<tr>
<td><code>sum-p</code></td>
<td>Suma de polinomios</td>
<td><code>(sum-p '(1 2) '(3 4))</code></td>
</tr>
<tr>
<td><code>sub-p</code></td>
<td>Resta de polinomios</td>
<td><code>(sub-p '(5 6) '(1 2))</code></td>
</tr>
<tr>
<td><code>mul-p</code></td>
<td>Multiplicación de polinomios</td>
<td><code>(mul-p '(1 1) '(1 1))</code></td>
</tr>
<tr>
<td><code>/-p</code></td>
<td>División (cociente y residuo)</td>
<td><code>(/-p '(0 0 1) '(1 1))</code></td>
</tr>
<tr>
<td><code>fact-p</code></td>
<td>Factorización completa</td>
<td><code>(fact-p '(4 4 1))</code></td>
</tr>
<tr>
<td><code>eval-p</code></td>
<td>Evaluación en punto</td>
<td><code>(eval-p '(1 2 3) 5)</code></td>
</tr>
<tr>
<td><code>drv-p</code></td>
<td>Derivada del polinomio</td>
<td><code>(drv-p '(1 2 3 4))</code></td>
</tr>
<tr>
<td><code>display-p</code></td>
<td>Mostrar formato matemático</td>
<td><code>(display-p '(1 -2 1))</code></td>
</tr>
</table>
</div>

---

## ⚠️ Notas Importantes

<div align="center">
<table>
<tr>
<th>🎯 Aspecto</th>
<th>📋 Detalle</th>
</tr>
<tr>
<td><b>División por Cero</b></td>
<td>Si la división no es posible, devuelve el dividendo como residuo</td>
</tr>
<tr>
<td><b>Orden de Coeficientes</b></td>
<td>Formato inverso: constante primero, potencias mayores al final</td>
</tr>
<tr>
<td><b>Simplificación</b></td>
<td>Los términos con coeficiente 0 se eliminan automáticamente</td>
</tr>
<tr>
<td><b>Precisión</b></td>
<td>Racket maneja fracciones exactas para máxima precisión</td>
</tr>
</table>
</div>

---

## 🧪 Casos de Prueba

<details>
<summary><b>🔬 Ver casos de prueba completos</b></summary>

```racket
;; Pruebas de Factorización
(fact-p '(4 4 1))          ; Caso: x² + 4x + 4 = (x+2)²
(fact-p '(-6 1 1))         ; Caso: x² + x - 6 = (x+3)(x-2)
(fact-p '(0 2 0 -2))       ; Caso: 2x - 2x³ = 2x(1-x²)

;; Pruebas de Evaluación
(eval-p '(1 2 3) 0)        ; f(0) = 1
(eval-p '(1 2 3) 1)        ; f(1) = 6  
(eval-p '(1 2 3) -1)       ; f(-1) = 2

;; Pruebas de División
(/-p '(1 0 1) '(1 1))      ; (x²+1) ÷ (x+1)
(/-p '(8 0 0 1) '(2 1))    ; (x³+8) ÷ (x+2)
```

</details>

---

## 🎓 Contexto Académico

### 📚 **Curso:** EIF-400 - Paradigmas de Programación
### 🏫 **Institución:** UNA
### 📅 **Período:** Semestre II / 2024

### 🎯 **Objetivos de Aprendizaje**
- Aplicar **programación funcional** en problemas matemáticos
- Implementar **algoritmos algebraicos** clásicos
- Desarrollar **pensamiento recursivo** y estructuras inmutables
- Practicar **testing** y validación de funciones

---


<div align="center">

### 🎯 **Proyecto desarrollado con fines académicos**
### 💡 **Demonstración de programación funcional aplicada**

</div>
