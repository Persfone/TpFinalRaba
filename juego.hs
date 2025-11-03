import System.IO (hFlush, stdout)

-- Funcion principal
main :: IO ()
main = menuPrincipal

-- Limpia la pantalla (lineas en blanco)
clearScreen :: IO ()
clearScreen = putStr "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

-- Menu principal
menuPrincipal :: IO ()
menuPrincipal = do
  clearScreen
  putStrLn "\n+========================================+"
  putStrLn "|  PIEDRA, PAPEL O TIJERAS (Secuencias)  |"
  putStrLn "+========================================+"
  putStrLn "1. Jugar"
  putStrLn "2. Ver reglas del juego"
  putStrLn "3. Salir"
  putStr "\nSeleccione una opcion: "
  hFlush stdout
  opcion <- getLine
  case opcion of
    "1" -> iniciarJuego
    "2" -> mostrarReglas >> volverAlMenu
    "3" -> putStrLn "Gracias por jugar!"
    _   -> do
      putStrLn "Opcion invalida. Intente nuevamente."
      esperarTecla >> menuPrincipal

-- Inicia un nuevo juego
iniciarJuego :: IO ()
iniciarJuego = do
  clearScreen
  putStrLn "\n=== Variante de Piedra, Papel o Tijeras ==="
  n <- getNumeroPartidas
  playGames n n 0 0

-- Obtiene la cantidad de partidas
getNumeroPartidas :: IO Int
getNumeroPartidas = do
  putStr "Ingrese la cantidad de partidas a jugar: "
  hFlush stdout
  input <- getLine
  if all (`elem` "0123456789") input && input /= ""
    then 
      let n = read input :: Int
      in if n > 0 then return n else reintentar
    else reintentar
  where
    reintentar = do
      putStrLn "Error: debe ingresar un numero entero positivo."
      getNumeroPartidas

-- Juega n partidas
playGames :: Int -> Int -> Int -> Int -> IO ()
playGames 0 totalGames score1 score2 = do
  clearScreen
  putStrLn "\n=== Juego terminado! ==="
  putStrLn ("Puntaje final -> Jugador 1: " ++ show score1 ++ " | Jugador 2: " ++ show score2)
  if score1 > score2
    then putStrLn "Gana el Jugador 1!"
    else if score2 > score1
         then putStrLn "Gana el Jugador 2!"
         else putStrLn "Empate general!"
  putStrLn "\nPresione ENTER para continuar..."
  esperarTecla
  menuPrincipal

playGames n totalGames score1 score2 = do
  clearScreen
  let partidaActual = totalGames - n + 1
  putStrLn ("\n--- Partida " ++ show partidaActual ++ " de " ++ show totalGames ++ " ---")
  seq1 <- getSecretSequence 1
  seq2 <- getSecretSequence 2
  let len = min (length seq1) (length seq2)
      (wins1, wins2) = countWins (take len seq1) (take len seq2)
      newScore1 = score1 + wins1
      newScore2 = score2 + wins2
  
  clearScreen
  putStrLn "\nResultado de la partida:"
  putStrLn ("Jugador 1: " ++ seq1)
  putStrLn ("Jugador 2: " ++ seq2)
  putStrLn ("Puntos -> Jugador 1: " ++ show wins1 ++ " | Jugador 2: " ++ show wins2)
  putStrLn ("Puntaje acumulado -> Jugador 1: " ++ show newScore1 ++ " | Jugador 2: " ++ show newScore2)
  
  putStrLn "\nPresione ENTER para continuar..."
  esperarTecla
  playGames (n-1) totalGames newScore1 newScore2

-- Obtiene secuencia de un jugador (solo R, P, S en mayusculas)
getSecretSequence :: Int -> IO String
getSecretSequence player = do
  clearScreen
  putStrLn ("\nJugador " ++ show player ++ ", ingrese su secuencia secreta (solo R, P, S):")
  putStrLn "(Presione una letra a la vez y ENTER vacio para terminar)"
  seq <- getSequence []
  if null seq then do
    putStrLn "Error! Debe ingresar al menos una jugada."
    esperarTecla
    getSecretSequence player
  else if validSequence seq then do
    putStrLn ("Secuencia aceptada (longitud: " ++ show (length seq) ++ ")")
    esperarTecla
    return seq
  else do
    putStrLn "Secuencia invalida! Solo se permiten R, P, S."
    esperarTecla
    getSecretSequence player

-- Lee las letras una a una hasta que se presione ENTER
getSequence :: String -> IO String
getSequence current = do
  putStr ("Secuencia actual: " ++ showCurrent current ++ " (1 letra o ENTER para terminar): ")
  hFlush stdout
  input <- getLine
  let trimmed = filter (/= ' ') input
  if trimmed == "" then
    return current
  else if length trimmed > 1 then do
    putStrLn "Error! Solo una letra por vez."
    getSequence current
  else if head trimmed `elem` ['R', 'P', 'S'] then
    getSequence (current ++ trimmed)
  else do
    putStrLn "Letra invalida! Use solo R, P o S."
    getSequence current

-- Oculta las letras ingresadas
showCurrent :: String -> String
showCurrent [] = ""
showCurrent xs = unwords (map (const "*") xs)

-- Verifica que solo haya R, P o S
validSequence :: String -> Bool
validSequence = all (`elem` ['R','P','S'])

-- Calcula el resultado de cada ronda
countWins :: String -> String -> (Int, Int)
countWins [] [] = (0, 0)
countWins (x:xs) (y:ys) = case winner x y of
                            1 -> (1 + w1, w2)
                            2 -> (w1, 1 + w2)
                            _ -> (w1, w2)
  where (w1, w2) = countWins xs ys
countWins _ _ = (0, 0)

-- Determina el ganador de una jugada
winner :: Char -> Char -> Int
winner 'R' 'S' = 1
winner 'S' 'R' = 2
winner 'P' 'R' = 1
winner 'R' 'P' = 2
winner 'S' 'P' = 1
winner 'P' 'S' = 2
winner _   _   = 0

-- Espera una tecla para continuar
esperarTecla :: IO ()
esperarTecla = do
  _ <- getLine
  return ()

-- Vuelve al menu principal
volverAlMenu :: IO ()
volverAlMenu = do
  putStrLn "\nPresione ENTER para volver al menu..."
  esperarTecla
  menuPrincipal

-- Muestra las reglas del juego
mostrarReglas :: IO ()
mostrarReglas = do
  clearScreen
  putStrLn "\n+===============================================+"
  putStrLn "|              REGLAS DEL JUEGO                 |"
  putStrLn "+===============================================+"
  putStrLn "- Cada jugador ingresa una secuencia secreta."
  putStrLn "- Las jugadas validas son:"
  putStrLn "  * R (Roca/Piedra)"
  putStrLn "  * P (Papel)"
  putStrLn "  * S (Tijeras)"
  putStrLn "- Se comparan posicion por posicion."
  putStrLn "- Reglas de victoria:"
  putStrLn "  * Roca vence a Tijeras"
  putStrLn "  * Papel vence a Roca"
  putStrLn "  * Tijeras vence a Papel"
  putStrLn "- Gana quien tenga mas puntos al final."