import System.IO (hFlush, stdout, hSetEncoding, utf8)
import Data.List (intercalate)
import Data.Char (toUpper)
import Text.Read (readMaybe)
import Data.IORef

-- Función principal
main :: IO ()
main = do
  hSetEncoding stdout utf8  -- Soporte para caracteres especiales
  historialRef <- newIORef []
  menuPrincipal historialRef

-- Menú principal
menuPrincipal :: IORef [(Int, Int, Int, Int)] -> IO ()
menuPrincipal historialRef = do
  putStrLn "\n╔════════════════════════════════════════╗"
  putStrLn "║  PIEDRA, PAPEL O TIJERAS (Secuencias)  ║"
  putStrLn "╚════════════════════════════════════════╝"
  putStrLn "1. Jugar"
  putStrLn "2. Ver historial de partidas"
  putStrLn "3. Cambiar opciones"
  putStrLn "4. Salir"
  putStr "\nSeleccione una opción: "
  hFlush stdout
  opcion <- getLine
  case opcion of
    "1" -> iniciarJuego historialRef
    "2" -> verHistorial historialRef
    "3" -> cambiarOpciones historialRef
    "4" -> putStrLn "¡Gracias por jugar!"
    _   -> do
      putStrLn "Opción inválida. Intente nuevamente."
      menuPrincipal historialRef

-- Inicia un nuevo juego
iniciarJuego :: IORef [(Int, Int, Int, Int)] -> IO ()
iniciarJuego historialRef = do
  putStrLn "\n=== Variante de Piedra, Papel o Tijeras ==="
  n <- getNumerPartidas
  playGames n n 0 0 [] historialRef

-- Obtiene el número de partidas con validación correcta
getNumerPartidas :: IO Int
getNumerPartidas = do
  putStr "Ingrese la cantidad de partidas a jugar: "
  hFlush stdout
  nStr <- getLine
  case readMaybe nStr :: Maybe Int of
    Nothing -> do
      putStrLn "Error: Debe ingresar un número válido."
      getNumerPartidas
    Just n | n > 0 -> return n
           | otherwise -> do
               putStrLn "Error: Debe ingresar un número positivo."
               getNumerPartidas

-- Juega n partidas, llevando puntaje de jugador 1 y 2
playGames :: Int -> Int -> Int -> Int -> [(Int, Int, Int)] -> IORef [(Int, Int, Int, Int)] -> IO ()
playGames 0 totalGames score1 score2 historial historialRef = do
  putStrLn "\n=== ¡Juego terminado! ==="
  putStrLn ("Puntaje final -> Jugador 1: " ++ show score1 ++ " | Jugador 2: " ++ show score2)
  if score1 > score2
    then putStrLn "¡Gana el Jugador 1!"
    else if score2 > score1
         then putStrLn "¡Gana el Jugador 2!"
         else putStrLn "¡Empate general!"
  
  -- Guardar resultado en historial global
  historialActual <- readIORef historialRef
  let nuevoRegistro = (totalGames, score1, score2, if score1 > score2 then 1 else if score2 > score1 then 2 else 0)
  writeIORef historialRef (historialActual ++ [nuevoRegistro])
  
  putStr "\n¿Desea jugar otra vez? (S/N): "
  hFlush stdout
  respuesta <- getLine
  let respuestaUpper = map toUpper respuesta
  if respuestaUpper == "S"
    then iniciarJuego historialRef
    else menuPrincipal historialRef

playGames n totalGames score1 score2 historial historialRef = do
  let partidaActual = totalGames - n + 1
  putStrLn ("\n--- Partida " ++ show partidaActual ++ " de " ++ show totalGames ++ " ---")
  seq1 <- getSecretSequence 1
  seq2 <- getSecretSequence 2
  let len = min (length seq1) (length seq2)
      (wins1, wins2) = countWins (take len seq1) (take len seq2)
      newScore1 = score1 + wins1
      newScore2 = score2 + wins2
      nuevoHistorial = historial ++ [(partidaActual, wins1, wins2)]
  
  putStrLn "\nResultado de la partida:"
  putStrLn ("Jugador 1: " ++ seq1)
  putStrLn ("Jugador 2: " ++ seq2)
  putStrLn ("Puntos -> Jugador 1: " ++ show wins1 ++ " | Jugador 2: " ++ show wins2)
  putStrLn ("Puntaje acumulado -> Jugador 1: " ++ show newScore1 ++ " | Jugador 2: " ++ show newScore2)
  
  playGames (n-1) totalGames newScore1 newScore2 nuevoHistorial historialRef

-- Obtiene una secuencia secreta válida del jugador indicado
getSecretSequence :: Int -> IO String
getSecretSequence player = do
  putStrLn ("\nJugador " ++ show player ++ ", ingrese su secuencia secreta (solo R, P, S):")
  putStrLn "(Presione una letra a la vez y ENTER vacío para terminar)"
  seq <- getSequence []
  let upperSeq = map toUpper seq
  if null upperSeq then do
    putStrLn "¡Error! Debe ingresar al menos una jugada."
    getSecretSequence player
  else if validSequence upperSeq then do
    putStrLn ("Secuencia del Jugador " ++ show player ++ " aceptada (longitud: " ++ show (length upperSeq) ++ ")")
    return upperSeq
  else do
    putStrLn "¡Secuencia inválida! Solo se permiten R, P, S."
    getSecretSequence player

-- Lee letras una por una hasta que se presione ENTER solo
getSequence :: String -> IO String
getSequence current = do
  putStr ("Secuencia actual: " ++ showCurrent current ++ " (1 letra o ENTER para terminar): ")
  hFlush stdout
  input <- getLine
  let trimmed = filter (/= ' ') input

  if null trimmed then
    return current

  else if length trimmed > 1 then do
    putStrLn "¡Error! Solo se permite 1 letra por línea."
    getSequence current

  else do
    let char = toUpper (head trimmed)
    if char `elem` ['R', 'P', 'S'] then
      getSequence (current ++ [char])
    else do
      putStrLn "¡Letra inválida! Use solo R, P o S."
      getSequence current

-- Muestra la secuencia actual ocultando las letras ya ingresadas
showCurrent :: String -> String
showCurrent [] = ""
showCurrent xs = unwords (map (const "*") xs)

-- Valida que una secuencia solo contenga R, P, S
validSequence :: String -> Bool
validSequence = all (`elem` ['R','P','S'])

-- Cuenta victorias por posición (1 punto por victoria, 0 por empate)
countWins :: String -> String -> (Int, Int)
countWins [] [] = (0, 0)
countWins (x:xs) (y:ys) = case winner x y of
                            1 -> (1 + w1, w2)
                            2 -> (w1, 1 + w2)
                            _ -> (w1, w2)
  where (w1, w2) = countWins xs ys
countWins _ _ = (0, 0)

-- Determina el ganador de una jugada individual: 1 = jugador1, 2 = jugador2, 0 = empate
winner :: Char -> Char -> Int
winner 'R' 'S' = 1
winner 'S' 'R' = 2
winner 'P' 'R' = 1
winner 'R' 'P' = 2
winner 'S' 'P' = 1
winner 'P' 'S' = 2
winner _   _   = 0

-- Ver historial
verHistorial :: IORef [(Int, Int, Int, Int)] -> IO ()
verHistorial historialRef = do
  historial <- readIORef historialRef
  putStrLn "\n+===============================================+"
  putStrLn "|          HISTORIAL DE PARTIDAS                |"
  putStrLn "+===============================================+"
  if null historial then
    putStrLn "No hay partidas registradas aún."
  else do
    putStrLn "\nJuegos completados:"
    mapM_ mostrarJuego (zip [1..] historial)
  putStrLn "\nPresione ENTER para volver al menú..."
  hFlush stdout
  _ <- getLine
  menuPrincipal historialRef

-- Muestra un juego del historial
mostrarJuego :: (Int, (Int, Int, Int, Int)) -> IO ()
mostrarJuego (num, (partidas, score1, score2, ganador)) = do
  putStrLn ("\n--- Juego #" ++ show num ++ " (" ++ show partidas ++ " partidas) ---")
  putStrLn ("Jugador 1: " ++ show score1 ++ " puntos")
  putStrLn ("Jugador 2: " ++ show score2 ++ " puntos")
  case ganador of
    1 -> putStrLn "Ganador: Jugador 1"
    2 -> putStrLn "Ganador: Jugador 2"
    _ -> putStrLn "Resultado: Empate"

-- Cambiar opciones
cambiarOpciones :: IORef [(Int, Int, Int, Int)] -> IO ()
cambiarOpciones historialRef = do
  putStrLn "\n=== OPCIONES ==="
  putStrLn "1. Mostrar reglas del juego"
  putStrLn "2. Volver al menú principal"
  putStr "\nSeleccione una opción: "
  hFlush stdout
  opcion <- getLine
  case opcion of
    "1" -> do
      mostrarReglas
      cambiarOpciones historialRef
    "2" -> menuPrincipal historialRef
    _   -> do
      putStrLn "Opción inválida."
      cambiarOpciones historialRef

-- Mostrar reglas
mostrarReglas :: IO ()
mostrarReglas = do
  putStrLn "\n+===============================================+"
  putStrLn "|              REGLAS DEL JUEGO                 |"
  putStrLn "+===============================================+"
  putStrLn "- Cada jugador ingresa una secuencia secreta"
  putStrLn "- Las jugadas válidas son:"
  putStrLn "  * R (Roca/Piedra)"
  putStrLn "  * P (Papel)"
  putStrLn "  * S (Scissors/Tijeras)"
  putStrLn "- Se comparan posición por posición"
  putStrLn "- Reglas de victoria:"
  putStrLn "  * Roca vence a Tijeras"
  putStrLn "  * Papel vence a Roca"
  putStrLn "  * Tijeras vence a Papel"
  putStrLn "- Gana quien tenga más puntos al final"
  putStrLn "\nPresione ENTER para continuar..."
  hFlush stdout
  _ <- getLine
  return ()