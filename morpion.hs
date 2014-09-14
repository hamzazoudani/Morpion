-- Morpion en Haskell




import Data.Char ( digitToInt )
import Data.List ( intercalate, intersperse )
import Data.List.Split ( splitOn )
import Data.Maybe ( fromJust, fromMaybe )
import Data.Sequence ( Seq(..), update, fromList, index )




data Jeu = Jeu {joueur :: Joueur, plateau :: Plateau} deriving (Eq, Show)

data Joueur = Circle | Cross deriving (Eq, Show)

type Plateau = Seq Case

data Case = Vide | O | X deriving (Eq, Show)


initJeu :: Jeu
initJeu = Jeu {joueur = Cross, plateau = initPlateau}


initPlateau :: Plateau
initPlateau = fromList $ take 9 $ repeat Vide




play :: Jeu -> Int -> Jeu
play j n = Jeu {joueur = jB, plateau = plato}
	where
		jA = joueur j
		jB = otherJoueur jA
		kase = joueur2Case jA
		plato = update n kase $ plateau j




otherJoueur :: Joueur -> Joueur
otherJoueur Cross = Circle
otherJoueur Circle = Cross


joueur2Case :: Joueur -> Case
joueur2Case Cross = X
joueur2Case Circle = O


canPlay :: Jeu -> Int -> Bool
canPlay _ (-1) = False
canPlay j n = index (plateau j) n == Vide


str2Coord :: String -> Int
str2Coord (xc:yc:xs) = if ok then (y-1)*3 + x else -1
	where
		x = if xc `elem` "abc" then digitToInt xc - 10 else -1
		y = digitToInt yc
		a = null xs
		b = x `elem` [0..2]
		c = y `elem` [1..3]
		ok = a && b && c
str2Coord (x:xs) = -1
str2Coord [] = -1


showCase :: Plateau -> Int -> Char
showCase p n = c $ index p n
	where
		c X = 'X'
		c O = 'O'
		c _ = '.'


showPlateau :: Jeu -> [String]
showPlateau j = ["",a,b,c,d,e,""]
	where
		p = plateau j
		a = "  . | a b c"
		b = "  --+------"
		c = intersperse ' ' $ " 1|" ++ [(showCase p 0),(showCase p 1),(showCase p 2)]
		d = intersperse ' ' $ " 2|" ++ [(showCase p 3),(showCase p 4),(showCase p 5)]
		e = intersperse ' ' $ " 3|" ++ [(showCase p 6),(showCase p 7),(showCase p 8)]


displayPlateau :: Jeu -> IO ()
displayPlateau j = do
	mapM putStrLn $ showPlateau j
	return ()


turn :: Jeu -> IO Jeu
turn j = do
	displayPlateau j
	putStrLn $ (show $ joueur j) ++ " plays on :"
	sc <- getLine
	let coords = str2Coord sc
	if canPlay j coords
		then do
			turn $ play j coords
		else do
			putStrLn "Error in the given coordinates."
			putStrLn "Must be in the format 'xy' eq. 'a2'."
			turn j


main :: IO ()
main = do
	--displayPlateau $ play initJeu $ fromJust $ str2Coord "b2"
	j <- turn initJeu
	return ()