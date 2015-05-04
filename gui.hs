import Graphics.UI.Gtk
import qualified NQueens as NQ
import qualified Puzzle as P
import qualified Cups as C
import qualified BlackAndWhite as BW

main :: IO ()
main = do
  initGUI
  window <- windowNew
  mainBox <- vBoxNew False 0
  set window [windowDefaultHeight := 500, windowDefaultWidth := 500, containerBorderWidth := 10,
              containerChild := mainBox, windowTitle := "MI Solver"]
  noteBook <- notebookNew
  queensBox <- boxNewNQueen
  notebookAppendPage noteBook queensBox "N Queens"
  cupsBox <- boxNewCups
  notebookAppendPage noteBook cupsBox "Filling"
  puzzleBox <- boxNewPuzzle
  notebookAppendPage noteBook puzzleBox "Puzzle"
  blackAndWhiteBox <- boxNewBW
  notebookAppendPage noteBook blackAndWhiteBox "Black & White"
  boxPackStart mainBox noteBook PackGrow 0
  --quitButton <- buttonNewWithLabel "Quit"
  --boxPackStart mainBox quitButton PackRepel 0
  --onClicked quitButton mainQuit
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

boxNewNQueen :: IO VBox
boxNewNQueen = do
  box <- vBoxNew False 0
  label <- labelNew (Just "N:")
  miscSetAlignment label 0.0 0.5
  boxPackStart box label PackNatural 0
  spinButton <- spinButtonNewWithRange 1.0 10.0 1.0
  boxPackStart box spinButton PackNatural 0
  set spinButton [spinButtonValue := 8.0]
  (outbuf, frame) <- makeOutput
  textBufferSetText outbuf (NQ.solveForOutput 8)
  boxPackStart box frame PackGrow 0
  onValueSpinned spinButton $ do n <- get spinButton spinButtonValue
                                 textBufferSetText outbuf (NQ.solveForOutput (round n))
  return box

boxNewPuzzle :: IO VBox
boxNewPuzzle = do
  box <- vBoxNew False 0
  label <- labelNew (Just "Configuration:")
  miscSetAlignment label 0.0 0.5
  boxPackStart box label PackNatural 0
  inputFrame <- frameNew
  inbuf <- textBufferNew Nothing
  textBufferSetText inbuf "1 4 2\n7 0 8\n3 5 6"
  inText <- textViewNewWithBuffer inbuf
  font <- fontDescriptionFromString "Courier"
  widgetModifyFont inText (Just font)
  inputWindow <- scrolledWindowNew Nothing Nothing
  containerAdd inputWindow inText
  inputBox <- hBoxNew False 0
  boxPackStart inputBox inputWindow PackGrow 0
  inputButton <- buttonNewWithLabel "Enter"
  boxPackStart inputBox inputButton PackNatural 0
  containerAdd inputFrame inputBox
  frameSetShadowType inputFrame ShadowOut
  --spinButton <- spinButtonNewWithRange 1.0 10.0 1.0
  boxPackStart box inputFrame PackGrow 0
  (outbuf, frame) <- makeOutput
  textBufferSetText outbuf (P.solveForOutput $ P.parseInput "1 4 2\n7 0 8\n3 5 6")
  boxPackStart box frame PackGrow 0
  inputButton `on` buttonActivated $ do input <- get inbuf textBufferText
                                        textBufferSetText outbuf (P.solveForOutput (P.parseInput input))
  return box

boxNewCups :: IO VBox
boxNewCups = do
  box <- vBoxNew False 0
  label <- labelNew (Just "List of cups:")
  miscSetAlignment label 0.0 0.5
  boxPackStart box label PackNatural 0
  inputBox <- hBoxNew False 0
  spinButton <- spinButtonNewWithRange 1.0 100.0 1.0
  boxPackStart inputBox spinButton PackNatural 0
  set spinButton [spinButtonValue := 8.0]
  inputFrame <- frameNew
  txtfield <- entryNew
  entrySetText txtfield "5 7 9"
  boxPackStart inputBox txtfield PackGrow 0
  containerAdd inputFrame inputBox
  frameSetShadowType inputFrame ShadowOut
  boxPackStart box inputFrame PackNatural 0
  (outbuf, frame) <- makeOutput
  textBufferSetText outbuf (C.solveForOutput 8 (C.parseInput "5 7 9"))
  boxPackStart box frame PackGrow 0
  onEntryActivate txtfield $ do input <- entryGetText txtfield
                                n <- get spinButton spinButtonValue
                                textBufferSetText outbuf (C.solveForOutput (round n) (C.parseInput input))
  onValueSpinned spinButton $ do n <- get spinButton spinButtonValue
                                 input <- entryGetText txtfield
                                 textBufferSetText outbuf (C.solveForOutput (round n) (C.parseInput input))
  return box

boxNewBW :: IO VBox
boxNewBW = do
  box <- vBoxNew False 0
  label <- labelNew (Just "Number of black and white tiles:")
  miscSetAlignment label 0.0 0.5
  boxPackStart box label PackNatural 0
  inputBox <- hBoxNew True 0
  spinButtonBlack <- spinButtonNewWithRange 1.0 10.0 1.0
  spinButtonWhite <- spinButtonNewWithRange 1.0 10.0 1.0
  boxPackStart inputBox spinButtonBlack PackNatural 0
  boxPackStart inputBox spinButtonWhite PackNatural 0
  boxPackStart box inputBox PackNatural 0
  set spinButtonBlack [spinButtonValue := 3.0]
  set spinButtonWhite [spinButtonValue := 3.0]
  (outbuf, frame) <- makeOutput
  textBufferSetText outbuf (BW.solveForOutput 3 3)
  boxPackStart box frame PackGrow 0
  onValueSpinned spinButtonBlack $ do b <- get spinButtonBlack spinButtonValue
                                      w <- get spinButtonWhite spinButtonValue
                                      textBufferSetText outbuf (BW.solveForOutput (round b) (round w))
  onValueSpinned spinButtonWhite $ do b <- get spinButtonBlack spinButtonValue
                                      w <- get spinButtonWhite spinButtonValue
                                      textBufferSetText outbuf (BW.solveForOutput (round b) (round w))
  return box

makeOutput :: IO (TextBuffer, Frame)
makeOutput = do
  frame <- frameNew
  outbuf <- textBufferNew Nothing
  out <- textViewNewWithBuffer outbuf
  font <- fontDescriptionFromString "Courier"
  widgetModifyFont out (Just font)
  scroll <- scrolledWindowNew Nothing Nothing
  containerAdd scroll out
  containerAdd frame scroll
  frameSetShadowType frame ShadowOut
  return (outbuf, frame)

