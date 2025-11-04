import System.IO (hFlush, stdout)
import System.Console.ANSI

main :: IO ()
main = menuPrincipal

toUpperChar :: Char -> Char
toUpperChar c
  | c == 'r' = 'R'
  | c == 'p' = 'P'
  | c == 's' = 'S'
  | otherwise = c

menuPrincipal :: IO ()
menuPrincipal = do
  clearScreen
  putStrLn "\n╔════════════════════════════════════════╗"
  putStrLn "║  PIEDRA, PAPEL O TIJERAS (Secuencias)  ║"
  putStrLn "╚════════════════════════════════════════╝"
  putStrLn "1. Jugar"
  putStrLn "2. Ver reglas del juego"
  putStrLn "3. Salir"
  putStr "\nSeleccione una opcion: "
  hFlush stdout
  opcion <- getLine
  case opcion of
    "1" -> iniciarJuego
    "2" -> mostrarReglas >> volverAlMenu
    "3" -> putStrLn "\nGracias por jugar!"
    _   -> do
      putStrLn "Opcion invalida. Intente nuevamente."
      esperarTecla >> menuPrincipal

iniciarJuego :: IO ()
iniciarJuego = do
  clearScreen
  putStrLn "\n╔════════════════════════════════════════╗"
  putStrLn "║  VARIANTE DE PIEDRA, PAPEL O TIJERAS   ║"
  putStrLn "╚════════════════════════════════════════╝"
  n <- getNumeroPartidas
  playGames n n 0 0

getNumeroPartidas :: IO Int
getNumeroPartidas = do
  putStr "Ingrese la cantidad de partidas a jugar: "
  hFlush stdout
  input <- getLine
  let trimmed = filter (/= ' ') input
  if all (`elem` "0123456789") trimmed && trimmed /= ""
    then 
      let n = read trimmed :: Int
      in if n > 0 then return n else reintentar
    else reintentar
  where
    reintentar = do
      putStrLn "Error: debe ingresar un numero entero positivo."
      getNumeroPartidas

playGames :: Int -> Int -> Int -> Int -> IO ()
playGames 0 totalGames score1 score2 = do
  clearScreen
  putStrLn "\n╔════════════════════════════════════════╗"
  putStrLn "║          JUEGO TERMINADO!              ║"
  putStrLn "╚════════════════════════════════════════╝"
  putStrLn ""
  putStrLn ("Puntaje final -> Jugador 1: " ++ show score1 ++ " | Jugador 2: " ++ show score2)
  putStrLn "----------------------------------------"
  if score1 > score2
    then putStrLn "           GANA EL JUGADOR 1!"
    else if score2 > score1
         then putStrLn "           GANA EL JUGADOR 2!"
         else putStrLn "            EMPATE GENERAL!"
  putStrLn "----------------------------------------"
  putStrLn "\nPresione ENTER para continuar..."
  esperarTecla
  menuPrincipal

playGames n totalGames score1 score2 = do
  clearScreen
  let partidaActual = totalGames - n + 1
  putStrLn "\n╔════════════════════════════════════════╗"
  putStrLn ("║           PARTIDA " ++ show partidaActual ++ " de " ++ show totalGames ++ "               ║")
  putStrLn "╚════════════════════════════════════════╝"
  seq1 <- getSecretSequence 1
  seq2 <- getSecretSequence 2
  let len = min (length seq1) (length seq2)
      (wins1, wins2) = countWins (take len seq1) (take len seq2)
      newScore1 = score1 + wins1
      newScore2 = score2 + wins2
  
  clearScreen
  putStrLn "\n╔════════════════════════════════════════╗"
  putStrLn "║       RESULTADO DE LA PARTIDA          ║"
  putStrLn "╚════════════════════════════════════════╝"
  putStrLn ""
  putStrLn "Secuencias reveladas:"
  putStrLn ("  Jugador 1: " ++ insertarEspacios seq1)
  putStrLn ("  Jugador 2: " ++ insertarEspacios seq2)
  putStrLn ""
  putStrLn "----------------------------------------"
  putStrLn ("Puntos -> Jugador 1: " ++ show wins1 ++ " | Jugador 2: " ++ show wins2)
  putStrLn ("Puntaje acumulado -> Jugador 1: " ++ show newScore1 ++ " | Jugador 2: " ++ show newScore2)
  putStrLn "----------------------------------------"
  putStrLn "\nPresione ENTER para continuar..."
  esperarTecla
  playGames (n-1) totalGames newScore1 newScore2

insertarEspacios :: String -> String
insertarEspacios [] = []
insertarEspacios [x] = [x]
insertarEspacios (x:xs) = x : ' ' : insertarEspacios xs

getSecretSequence :: Int -> IO String
getSecretSequence player = do
  clearScreen
  putStrLn "\n╔════════════════════════════════════════╗"
  putStrLn ("║       JUGADOR " ++ show player ++ " - INGRESO SECRETO      ║")
  putStrLn "╚════════════════════════════════════════╝"
  putStrLn ""
  putStrLn "(Presione una letra a la vez y ENTER vacio para terminar)"
  seq <- getSequence []
  if null seq then do
    putStrLn "\nError! Debe ingresar al menos una jugada."
    esperarTecla
    getSecretSequence player
  else if validSequence seq then do
    putStrLn ("\nSecuencia aceptada (longitud: " ++ show (length seq) ++ ")")
    esperarTecla
    return seq
  else do
    putStrLn "\nSecuencia invalida! Solo se permiten R, P, S."
    esperarTecla
    getSecretSequence player

getSequence :: String -> IO String
getSequence current = do
  putStr ("Secuencia actual: " ++ showCurrent current ++ " > ")
  hFlush stdout
  input <- getLine
  let trimmed = filter (/= ' ') input
  if trimmed == "" then
    return current
  else if length trimmed > 1 then do
    putStrLn "Error! Solo una letra por vez."
    getSequence current
  else if toUpperChar (head trimmed) `elem` ['R', 'P', 'S'] then
    getSequence (current ++ [toUpperChar (head trimmed)])
  else do
    putStrLn "Letra invalida! Use solo R, P o S."
    getSequence current

showCurrent :: String -> String
showCurrent [] = ""
showCurrent xs = unwords (map (const "*") xs)

validSequence :: String -> Bool
validSequence = all (`elem` ['R','P','S'])

countWins :: String -> String -> (Int, Int)
countWins [] [] = (0, 0)
countWins (x:xs) (y:ys) = case winner x y of
                            1 -> (1 + w1, w2)
                            2 -> (w1, 1 + w2)
                            _ -> (w1, w2)
  where (w1, w2) = countWins xs ys
countWins _ _ = (0, 0)

winner :: Char -> Char -> Int
winner 'R' 'S' = 1
winner 'S' 'R' = 2
winner 'P' 'R' = 1
winner 'R' 'P' = 2
winner 'S' 'P' = 1
winner 'P' 'S' = 2
winner _   _   = 0

esperarTecla :: IO ()
esperarTecla = do
  _ <- getLine
  return ()

volverAlMenu :: IO ()
volverAlMenu = do
  putStrLn "\nPresione ENTER para volver al menu..."
  esperarTecla
  menuPrincipal

mostrarReglas :: IO ()
mostrarReglas = do
  clearScreen
  putStrLn "\n╔════════════════════════════════════════╗"
  putStrLn "║           REGLAS DEL JUEGO             ║"
  putStrLn "╚════════════════════════════════════════╝"
  putStrLn ""
  putStrLn "COMO SE JUEGA:"
  putStrLn "  - Cada jugador ingresa una secuencia secreta"
  putStrLn "  - Las secuencias se comparan posicion por posicion"
  putStrLn "  - Se usa la longitud de la secuencia mas corta"
  putStrLn ""
  putStrLn "MOVIMIENTOS VALIDOS:"
  putStrLn "  - R (Roca/Piedra)"
  putStrLn "  - P (Papel)"
  putStrLn "  - S (Tijeras)"
  putStrLn ""
  putStrLn "REGLAS DE VICTORIA:"
  putStrLn "  - Roca vence a Tijeras"
  putStrLn "  - Papel vence a Roca"
  putStrLn "  - Tijeras vence a Papel"
  putStrLn ""
  putStrLn "PUNTUACION:"
  putStrLn "  - Por cada victoria: +1 punto"
  putStrLn "  - Gana quien tenga mas puntos al final"