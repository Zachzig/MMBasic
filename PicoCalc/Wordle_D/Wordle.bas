' Wordle for PicoCalc
' Rev 1.0.0 William M Leue 9-Jan-2022
' Rev D1.0.0 Peter Doerwald - german and adaption to PicoCalc

Option default integer
Option base 1

' Constants
Const WLEN = 5
Const TLEN = WLEN+2
Const NUMROWS = 6
Const NDWRDS = 1711
Const NALPHA = 26
Const dict$ = "WordleOK_PD.txt"
'Const dict$ = "wordle_D5.txt"

' Layout params
Const FWIDTH = 24
Const FHEIGHT = 32
Const FBGAP = 3
Const GAP =   3
Const BWIDTH = FBGAP+FWIDTH+FBGAP
Const BHEIGHT = FBGAP+FHEIGHT+FBGAP
Const PSWIDTH = WLEN*FWIDTH + (WLEN-1)*GAP
Const PSHEIGHT = NUMROWS*FHEIGHT + (NUMROWS-1)*GAP
Const AWIDTH = (NALPHA\2)*FWIDTH + (NALPHA\2-1)*GAP
Const AHEIGHT = 2*FHEIGHT + GAP
Const CTRX = MM.HRes\2
Const PSX = CTRX - PSWIDTH\2
Const PSY = 10
Const ALX = CTRX - AWIDTH\2
Const ALY = 257 '400
Const FNUM = 5

' Keystroke values
Const ENTER  = 13
Const ESC    = 27
Const LCA    = 97
Const LCZ    = 122
Const CAPA   = 65
Const CAPZ   = 90
Const BACK   = 8
Const TAB    = 9

' Globals
theWord$ = ""
Dim rowContent(WLEN, NUMROWS)
Dim alphaStat(NALPHA)
Dim guess$ = ""
Dim nguesses = 0
Dim ngpl = 0
Dim colors(2, 5)
Dim istate = 0
Dim ngreens = 0
Dim running = 0

' Main Program
'open "debug.txt" for output as #1
ReadColors
Reset
ChooseWord
DrawIntroPage
HandleInput
End

' Read the colors for letter status (text, bg)
' 1: not yet guessed
' 2: guessed, word status still not known
' 3: guessed, not in word
' 4: guessed, in word but wrong position
' 5: guessed, in word and correct position
Sub ReadColors
  Local i, j
  For i = 1 To 5
    For j = 1 To 2
      Read colors(j, i)
    Next j
  Next i
End Sub

' Reset the puzzle
Sub Reset
  Local i, j
  For i = 1 To NUMROWS
    For j = 1 To WLEN
      rowContent(j, i) = 1*1000
    Next j
  Next i
  For i = 1 To NALPHA
    alphaStat(i) = 1
  Next i
  nguesses = 0
  guess$ = ""
  ngpl = 0
  ngreens = 0
  running = 1
End Sub

' Choose a random 5-letter word from the dictionary
Sub ChooseWord
  Local wx, fp
  Local buf$
  wx = RandomIntegerInRange(1, NDWRDS)
  fp = (wx-1)*TLEN + 1
  Open dict$ For random As #2
  Seek #2, fp
  print fp

  Line Input #2, buf$
  Close #2
  theWord$ = UCase$(buf$)
End Sub

' See if proffered word is a known word
' returns 1 if yes, 0 no
Function IsAWord(g$)
  Local wx, fp, ok
  Local buf$, w$
  ok = 0
  Open dict$ For random As #2
  For wx = 1 To NDWRDS
    fp = (wx-1)*TLEN + 1
    Seek #2, fp
    Line Input #2, buf$
    w$ = UCase$(Left$(buf$, WLEN))
    If w$ = g$ Then
      ok = 1
      Exit For
    End If
  Next wx
  Close #2
  IsAWord = ok
End Function

' Draw the puzzle
Sub DrawPuzzle
  DrawCells
  DrawAlphabet
End Sub

' Draw the array of cells at the top of the puzzle
Sub DrawCells
  Local i, j, x, y, rc, a, st, c, tc
  y = PSY
  For i = 1 To NUMROWS
    x = PSX-20
    For j = 1 To WLEN
      rc = rowContent(j, i)
      st = rc\1000
      a = rc - 1000*st
      tc = colors(1, st)
      c = colors(2, st)
      box x, y, BWIDTH, BHEIGHT,, rgb(white), c
      If i <= nguesses Then
        If a >= Asc("A") Then
          tc = colors(1, st)
          c = colors(2, st)
          text x+FBGAP+2, y+FBGAP+2, Chr$(a), "LT", FNUM,, tc, -1
        End If
      End If
      inc x, BWIDTH + GAP
    Next j
    inc y, BHEIGHT + GAP
  Next i
End Sub

' Draw the alphabet at the bottom of the puzzle
Sub DrawAlphabet
  Local x, y, row, col, ax, st, c, tc
  if running <> 0 Then
    y = ALY
    ax = Asc("A")
    For row = 1 To 2
      x = ALX+27
      For col = 1 To 13
        st = WasGuessed(Chr$(ax))
        tc = colors(1, st)
        c = colors(2, st)
        box x, y, BWIDTH-10, BHEIGHT-10,, rgb(white), c
        text x+FBGAP, y+FBGAP+2, Chr$(ax), "LT", 3,, tc, -1
        inc ax
        inc x, BWIDTH+GAP-10
      Next col
    inc y, BHEIGHT+GAP-10
  Next row
  End If
End Sub

' Handle user keystrokes
Sub HandleInput
  Local z$, g$
  Local cmd, ucmd, i, ax
  Do
    z$ = Inkey$
    Do
      z$ = Inkey$
    Loop Until z$ <> ""
    ShowStatus ""
    cmd = Asc(UCase$(z$))
    Select Case cmd
      Case ENTER
        If running = 1 Then
          If nguesses < numrows And  ngpl <> WLEN Then
            ShowStatus "zu kurz"
          Else
            g$ = ""
            For i = 1 To WLEN
              st = rowContent(i, nguesses)\1000
              ax = rowContent(i, nguesses) - 1000*st
              g$ = g$ + Chr$(ax)
            Next i
            k = IsAWord(g$)
            If k = 0 Then
              ShowStatus "Kein Wort"
            Else
              UpdateGuessStatus
              If nguesses < numrows Then inc nguesses
              ngpl = 0
              DrawPuzzle
            End If
          End If
        End If
      Case ESC
        Cls
        End
      Case CAPA To CAPZ
        If running = 1 Then
          If ngpl < WLEN Then
            If ngpl = 0 And nguesses = 0 Then inc nguesses
            inc ngpl
            guess$ = guess$ + Chr$(cmd)
            rowContent(ngpl, nguesses) = 2000 + cmd
            DrawPuzzle
          End If
        End If
      Case BACK
        If running = 1 Then
          If ngpl > 0 Then
            rowContent(ngpl, nguesses) = 1000
            inc ngpl, -1
            DrawPuzzle
          End If
        End If
      Case TAB
	    CLS
        Reset
        ChooseWord
        Cls
        DrawPuzzle
      Case Else
    End Select
  Loop
End Sub

' input is a letter and its position in the current guess row.
' returns 1 if the letter has not been guessed yet
' returns 2 if the letter has been guessed but word status still not known
' returns 3 if letter has been guessed but is not in word
' returns 4 if the letter has been guessed and was in word but out of place
' returns 5 if the letter has been guessed and was in the word in the correct place
Function WasGuessed(a$)
  Local ua = Asc(UCase$(a$))
  Local ix = ua - Asc("A") + 1
  WasGuessed = alphastat(ix)
End Function

' Compare the list of guessed letters to theWord$ letters and mark both the alphastat array
' and the rowContent array values to reflect the letter status:
'   1: letter not yet guessed
'   2: letter guessed but word status not yet known
'   3: letter guessed but not in word
'   4: letter guessed and in word but out of place
'   5: letter guessed in in word and in place
' The default letter status is 3.
' If a letter exactly matches one in the word, the status is 5
' Otherwise, we count how many of that letter are in the word.
' If one or more, then we see which of the prior guess letters
' are the same and if so, if they were already have status 4.
' If we haven't already accounted for all the instances of the
' letter in the word, then we set status 4. (More complicated
' than first expected.)
Sub UpdateGuessStatus
  Local i, j, rc, a, s, n, ax, st, gp, ngreens, nyellows, nc
  Local k, krc, ks, ka
  Local cp(WLEN)
  Local c$, kc$
  Local m(WLEN)
  ngreens = 0
  For i = 1 To WLEN
    m(i) = 0
    st = 3
    rc = rowContent(i, nguesses)
    s = rc\1000
    a = rc - 1000*s
    c$ = Chr$(a)
    ax = a - Asc("A") + 1
    w$ = Mid$(theWord$, i, 1)
    n = 0
    If c$ = w$ Then
      st = 5
      m(i) = 1
      inc ngreens
    End If
    rowContent(i, nguesses) = st*1000 + a
    alphaStat(ax) = st
  Next i
  For i = 1 To WLEN
    rc = rowContent(i, nguesses)
    s = rc\1000
    a = rc - 1000*s
    c$ = Chr$(a)
    ax = a - Asc("A") + 1
    nc = 0
    For j = 1 To WLEN
      If Mid$(theWord$, j, 1) = c$ Then inc nc
    Next j
    If s < 4 Then
      For j = 1 To WLEN
        w$ = Mid$(theWord$, j, 1)
        nkc = 0
        For k = 1 To j-1
          krc = rowContent(k, nguesses)
          ks = krc\1000
          ka = krc - 1000*ks
          kc$ = Chr$(ka)
          If kc$ = c$ And ks = 4 Then inc nkc
        Next k
        If c$ = w$ And m(j) = 0 And nkc < nc Then
          st = 4
          rowContent(i, nguesses) = st*1000 + a
          alphaStat(ax) = st
        End If
      Next j
    End If
  Next i
  CheckEndGame ngreens
End Sub

' See if game is over
Sub CheckEndGame ngreens
  
  If ngreens = WLEN Then
    running = 0
	box 0, ALY, MM.HRes, 320-ALY,, rgb(white), rgb(black)
	text MM.HRes/2, ALY+10, "Gewonnen!", "CT", 1,, rgb(white), -1
    'ShowStatus "Gewonnen!"
  Else If nguesses = numrows Then
    running = 0
	box 0, ALY, MM.HRes, 320-ALY,, rgb(white), rgb(black)
    If ngreens = WLEN Then
	  text MM.HRes/2, ALY+10, "Gewonnen!", "CT", 1,, rgb(white), -1
      'ShowStatus "Gewonnen!"
    Else
	  text MM.HRes/2, ALY+10, "Verloren: das Wort war '" + theWord$ + "'", "CT", 1,, rgb(white), -1
      'ShowStatus "Sorry, verloren. Das Wort war '" + theWord$ + "'"
    End If
  End If
  If running = 0 Then
    text MM.HRes/2, ALY+30, "Druecke Tab fuer ein neues Spiel", "CT", 1,, rgb(white), -1
  End If
End Sub

' Introduction Page
Sub DrawIntroPage
  Local x, y, w
  Local z$
  Cls
  w = MM.HRes-20
  x = MM.HRes\2
  text x, 20, "WORDLE!", "CT", 5,, rgb(green)
  x = x - w\2
  y = 60 ': w = 400
  box x, y, w, 180,, rgb(white), rgb(white)
  inc x, 20
  inc y, 10
  text x, y, "Rate das Wort!", "LT", 1,, rgb(black), -1
  inc y, 15
  text x, y, "Schreibe ein 5er Wort + Enter.", "LT", 1,, rgb(black), -1
  inc y, 30
  text x, y, "Gruen = richtiger Ort.", "LT", 1,, rgb(black), rgb(green)
  inc y, 15
  text x, y, "Gelb = falscher Ort.", "LT", 1,, rgb(black), rgb(yellow)
  inc y, 15
  text x, y, "Schwarz = nicht im Wort.", "LT", 1,, rgb(white), rgb(black)
  inc y, 30
  text x, y, "Druecke Tab fuer ein neues Wort.", "LT", 1,, rgb(black), -1
  inc y, 15
  text x, y, "ESC zum Beenden.", "LT", 1,, rgb(black), -1
  inc y, 30
  text x, y, "Taste druecken.", "LT", 1,, rgb(black), -1
  z$ = Inkey$
  Do
    z$ = Inkey$
  Loop Until z$ <> ""
  Cls
  DrawPuzzle
End Sub

' print a status message at the bottom of the puzzle.
' The message persists until the next keystroke.
Sub ShowStatus msg$
  text MM.HRes-40, 26, Space$(10), "CT"
  text MM.HRes-40, 26, msg$, "CT", 1,, rgb(red), -1
End Sub

' Generate a uniformly distributed random integer in the closed range a to b
' (gets around issues with non-uniform distribution in rnd() at some
' expense in performance.)
Function RandomIntegerInRange(a As integer, b As integer) As integer
  Local v, c
  c = b - a + 1
  Do
    v = a + (b-a+2)*Rnd()
    If v > 1 And v-a <= c Then Exit Do
  Loop
  RandomIntegerInRange = v-1
End Function

' Colors for various letter statuses (text, background)
Data rgb(black), rgb(gray)
Data rgb(white), rgb(gray)
Data rgb(white), rgb(black)
Data rgb(black), rgb(255,255,0)
Data rgb(black), rgb(green)
