module Board(mkBoard,mkPlayer,mkOpponent,size,row,column,mark,isEmpty,isMarked,isMarkedBy,marker,isFull,isWonBy,isDraw,isGameOver,boardToStr,playerToChar,charToStr) where


 {-Return an empty nxn board, where n is a positive number. A 1-based 
     pair of indices (x,y) will be used to access a specific place of
     the board, where x and y are column and row indices, respectively.
     However, it's up to you how to represent an omok board concretely. -}
mkBoard :: Int -> [Char]
mkBoard n = mkList(n*n)

mkList :: Int -> [Char]
mkList 1 = [' ']
mkList n = (' ':mkList(n-1))

{-Create and return the first player. It's up to you how to
     represent an omok player concretely. E.g., mkPlayer = 1. The
     idea is to call this function whenever you need to refer to the
     first play, rather than using a magic number like 1.-}
mkPlayer :: Char
mkPlayer = 'X'

{-Create and return the second player, i.e., the opponent. E.g.,
     mkOpponent = 2-}
mkOpponent :: Char
mkOpponent = 'O'

{-size bd
     Return the size of a board bd, n for an nxn board.-}
size :: [Char] -> Int
size bd = (floor . sqrt . fromIntegral . length) bd

row :: Int -> [Char] -> [Char]
row _ []                        =   []
row y bd    |   y <= 0          =   []
            |   y > (size bd)   =   []
            |   otherwise       =   rowCut ((y-1) * (size bd)) (((y-1) * (size bd)) + (size bd) - 1) bd

rowCut :: Int -> Int -> [a] -> [a]
rowCut 0    0    (n:l)  = [n]
rowCut 0    tail (n:l)  = (n:(rowCut 0 (tail-1) l))
rowCut head tail (n:l)  = rowCut (head-1) (tail-1) l

{-
column x bd
     Return a column x of a board bd, where x is a 1-based index. It
     returns a list of size n, where n is the size of bd.
-}
column :: Int -> [Char] -> [Char]
column _ []                         =   []
column x bd     |   x <= 0          =   []
                |   x > (size bd)   =   []
                |   otherwise       =   columnCut (size bd) (drop (x - 1) bd)

columnCut :: Int -> [a] -> [a]
columnCut _ [] = []
columnCut n l  = ((head l):(columnCut n (drop n l)))

{-
++ operator will concatenate the half lists
mark x y bd p
     Mark a place (x,y) in a board bd by a player p, where x and y 
     are 1-based column and row indices. The specified place is assumed
     to be empty (see below).
-}
mark :: Int -> Int -> [Char] -> Char -> [Char]
mark x y bd p  = firstHalf ++ [p] ++ secondHalf
                where (firstHalf, (_:secondHalf)) = splitAt ((((y - 1) * (size bd)) + (x - 1))) bd 

{- This function reuses the marker function to test if space is empty, leading to more code reusage and less clutter
    isEmpty x y bd
     Is a place (x,y) of a board bd unmarked or a stone not placed? 
     The x and y are 1-based column and row indices.
-}
isEmpty :: Int -> Int -> [Char] -> Bool
isEmpty x y bd = ((marker x y bd) == ' ')

{-
isMarked x y bd
     Does a place (x,y) of a board bd have a stone placed? The x and y 
     are 1-based column and row indices.
-}
isMarked :: Int -> Int -> [Char] -> Bool
isMarked x y bd = not (isEmpty x y bd)

{-
isMarkedBy x y bd p
     Does a place (x,y) of a board bd have a stone placed by a player p?
     The x and y are 1-based column and row indices.
-}
isMarkedBy :: Int -> Int -> [Char] -> Char -> Bool
isMarkedBy x y bd pl = ((marker x y bd) == pl)             

{-
marker x y board 
     Return the player of the stone placed on a place (x,y) of a board 
     bd. The x and y are 1-based column and row indices.
-}
marker :: Int -> Int -> [Char] -> Char
marker x y bd   |  (((y - 1) * (size bd)) + (x - 1)) < 0            = ' '
                |  (((y - 1) * (size bd)) + (x - 1)) >= (length bd) = ' ' 
                |  otherwise                                        = head (drop ((((y - 1) * (size bd)) + (x - 1))) bd)

{-
isFull bd
     Are all places of board bd marked, i.e., there is no empty place?
-}
isFull :: [Char] -> Bool
isFull []        = True
isFull (' ':r) = False 
isFull (_:r)   = isFull r

{- Hardcode-ish algorithm to check for a win
isWonBy bd p
      Is the game played on a board bd won by a player p? That is, does 
      the board bd has a winning row for the player p?
-}
isWonBy :: [Char] -> Char -> Bool
isWonBy bd p = isWin bd p ((size bd) * (size bd))


isWin :: [Char] -> Char -> Int -> Bool
isWin bd pl 0   = False
isWin bd pl cur = (isWinRow bd pl ((mod (cur - 1)  (size bd)) + 1) ((div (cur - 1)  (size bd)) + 1 ) 1 0 5) || (isWinRow bd pl ((mod (cur - 1)  (size bd)) + 1) ((div (cur - 1)  (size bd)) + 1 ) 0 1 5) || (isWinRow bd pl ((mod (cur - 1)  (size bd)) + 1) ((div (cur - 1)  (size bd)) + 1 ) 1 1 5) || (isWinRow bd pl ((mod (cur - 1)  (size bd)) + 1) ((div (cur - 1)  (size bd)) + 1 ) 1 (-1) 5) || (isWin bd pl (cur - 1))


isWinRow :: [Char] -> Char -> Int -> Int -> Int -> Int -> Int -> Bool
isWinRow bd p x y dx dy 0   = True
isWinRow bd p x y dx dy rem = (isMarkedBy x y bd p) && (isWinRow bd p (x + dx) (y + dy) dx dy (rem - 1))

{-
isDraw bd
      Is the game played on a board bd ended in a draw?
-}
isDraw :: [Char] -> Bool
isDraw bd = (isFull bd) && (not (isWonBy bd 'X')) && (not (isWonBy bd 'O'))

{-
isGameOver bd
      Is the game played on a board bd over?
-}
isGameOver :: [Char] -> Bool
isGameOver bd = (isDraw bd) || (isWonBy bd 'X') || (isWonBy bd 'O')

{-
 boardToStr playerToChar bd

      Return a string representation of a board bd. This is a
      higher-order function, and playerToChar is a function that
      converts a player to a character representation, e.g., 'O' and
      'X' (see Part II below). A formatted sample return value is
      shown below; it is produced with a playerToChar function that
      maps the first player to 'O', the second player to 'X', and
      others to '.'.
-}
boardToStr :: (Char -> Char) -> [Char] -> [Char]
boardToStr playerToChar bd = allRowsToStrings playerToChar bd (size bd)

{-
playerToChar p
     Return a character representation of a player p. It returns a Char
     value. This function may be used to print the current state of
     a board (see the boardToStr function in Part I).
-}
playerToChar:: Char -> Char
playerToChar p = if p ==        'X'
                 then           'X'
                 else if p ==   'O'
                 then           'O'
                 else           '.'


charToStr :: Char -> String
charToStr c = [c]


allRowsToStrings :: (Char -> Char) -> [Char] -> Int -> [Char]
allRowsToStrings playerToChar bd 0       = []
allRowsToStrings playerToChar bd x = (allRowsToStrings playerToChar bd (x - 1)) ++ (rowToStr playerToChar (row x bd)) ++ ['\n'] 


rowToStr :: (Char -> Char) -> [Char] -> [Char]
rowToStr _            []      = []
rowToStr playerToChar (n:r) = [(playerToChar n)] ++ [' '] ++ (rowToStr playerToChar r)