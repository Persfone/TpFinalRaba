# Piedra, Papel o Tijeras por Secuencias (Haskell)


> **Programas Interactivos - Ejercicio g)**  
> Variante estratégica del clásico *Piedra, Papel o Tijeras*, implementada en **Haskell** con entrada interactiva y secreta.

---

## Descripción del Juego

Este es un **juego para dos jugadores** basado en el clásico *Piedra (R), Papel (P), Tijeras (S)*, pero con un giro estratégico:

- Cada jugador ingresa **secretamente una secuencia de movimientos** (ej: `RRPSPS`).
- Las secuencias se comparan **posición por posición**, usando solo la **longitud de la más corta**.
- Por cada posición:
  - **1 punto** al ganador (según reglas clásicas).
  - **0 puntos** por empate.
- Se juegan múltiples partidas.
- Al final, **gana el jugador con mayor puntaje total**.

---

## Cómo se Juega

1. **Inicio**  
   → El programa pregunta: 
        Ingrese la cantidad de partidas a jugar: Ejemplo: `3`

2. **Turno de cada jugador**  
→ Se ingresa la secuencia **letra por letra**, con:
- **1 letra por línea** (R, P o S).
- **ENTER vacío** para terminar.
- **Secreto total**: solo se ven `*` y el contador.
Secuencia actual: * * * (1 letra o ENTER para terminar): R
Secuencia actual: * * * * (1 letra o ENTER para terminar):


3. **Comparación**  
→ Ejemplo:
J1: R P S
J2: S P R

→ Resultado:  
- Pos 1: R vs S → **J1 gana** (+1)
- Pos 2: P vs P → **empate** (0)
- Pos 3: S vs R → **J2 gana** (+1)

4. **Fin del juego**  
→ Se muestra el puntaje total y se anuncia al ganador.

---

## Requisitos

- [Haskell Platform](https://www.haskell.org/downloads/) o `ghc` instalado.

---

## Modo de Ejecución

### 1. Guarda el código
Crea un archivo llamado `juego.hs` con el código fuente.

### 2. Abre la terminal en la carpeta del archivo

### 3. Compila
ghc juego.hs

### 4. Ejecuta
# Linux / macOS
./juego

# Windows
juego.exe