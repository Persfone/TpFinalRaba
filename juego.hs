import System.IO (hFlush, stdout)
import Data.List (intercalate)
import Data.Char (toUpper)

-- Función principal
main :: IO ()
main = do
  putStrLn "=== Variante de Piedra, Papel o Tijeras ==="
  putStr "Ingrese la cantidad de partidas a jugar: "
  hFlush stdout
  nStr <- getLine
  let n = read nStr :: Int
      Just n' | n > 0 = Just n
              | otherwise = error "Debe ingresar un número positivo."
  playGames n 0 0

-- Juega n partidas, llevando puntaje de jugador 1 y 2
playGames :: Int -> Int -> Int -> IO ()
playGames 0 score1 score2 = do
  putStrLn "\n=== ¡Juego terminado! ==="
  putStrLn $ "Puntaje final -> Jugador 1: " ++ show score1 ++ " | Jugador 2: " ++ show score2
  if score1 > score2
    then putStrLn "¡Gana el Jugador 1!"
    else if score2 > score1
         then putStrLn "¡Gana el Jugador 2!"
         else putStrLn "¡Empate general!"

playGames n score1 score2 = do
  putStrLn $ "\n--- Partida " ++ show (totalGames - n + 1) ++ " de " ++ show totalGames ++ " ---"
  seq1 <- getSecretSequence 1
  seq2 <- getSecretSequence 2
  let len = min (length seq1) (length seq2)
      (wins1, wins2) = countWins (take len seq1) (take len seq2)
      newScore1 = score1 + wins1
      newScore2 = score2 + wins2
  putStrLn $ "Puntos de esta partida -> Jugador 1: " ++ show wins1 ++ " | Jugador 2: " ++ show wins2
  playGames (n-1) newScore1 newScore2
  where totalGames = n

-- Obtiene una secuencia secreta válida del jugador indicado
getSecretSequence :: Int -> IO String
getSecretSequence player = do
  putStrLn $ "Jugador " ++ show player ++ ", ingrese su secuencia secreta (solo R, P, S):"
  putStrLn "(Presione una letra a la vez y ENTER al finalizar)"
  seq <- getSequence []
  let upperSeq = map toUpper seq
  if validSequence upperSeq
    then do
      putStrLn $ "Secuencia del Jugador " ++ show player ++ " aceptada (longitud: " ++ show (length upperSeq) ++ ")"
      return upperSeq
    else do
      putStrLn "¡Secuencia inválida! Solo se permiten R, P, S."
      getSecretSequence player

-- Lee letras una por una hasta que se presione ENTER solo
-- Lee letras una por una: solo 1 carácter por línea
getSequence :: String -> IO String
getSequence current = do
  putStr $ "Secuencia actual: " ++ showCurrent current ++ " (1 letra o ENTER para terminar): "
  hFlush stdout
  input <- getLine
  let trimmed = filter (/= ' ') input  -- quita espacios

  if null trimmed then
    return current  -- ENTER vacío → termina

  else if length trimmed > 1 then do
    putStrLn "¡Error! Solo se permite 1 letra por línea."
    getSequence current  -- repite

  else do
    let char = toUpper (head trimmed)
    if char `elem` ['R', 'P', 'S'] then
      getSequence (current ++ [char])
    else do
      putStrLn "¡Letra inválida! Use solo R, P o S."
      getSequence current

-- Muestra la secuencia actual ocultando las letras ya ingresadas
showCurrent :: String -> String
showCurrent = intercalate " " . map (const "*")

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

-- Determina el ganador de una jugada individual: 1 = jugador1, 2 = jugador2, 0 = empate
winner :: Char -> Char -> Int
winner 'R' 'S' = 1
winner 'S' 'R' = 2
winner 'P' 'R' = 1
winner 'R' 'P' = 2
winner 'S' 'P' = 1
winner 'P' 'S' = 2
winner _   _   = 0  -- empate si son iguales o cualquier otro caso