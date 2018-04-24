{-|
Module :: Vi
Description: [Neo]Vi[m] user-interface API. 

Provides 
* 'CursorPosn' : row and column number posn (vs the 'Int' indices used internally).
* 'ViSel' : a 'CursorPosn'-based selection that respects newlines.
* 'locate' : to fetch the key of the content in which a cursor is.
-}
module Vi where


