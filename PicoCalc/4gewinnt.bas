' 4 GEWINNT GRAFIK-EDITION fuer PicoCalc
' Getestet mit V6.00.03
' 01/2026 P.Doerwald and Gemini  
' V1.0
' ------------------------------------
Option Explicit
Option Default Integer

' Konstanten fuer das Layout
Const BOX_SIZE = 30
Const OFFSET_X = 54
Const OFFSET_Y = 40

Dim board(6, 5)
Dim player, column, row, moves, win, gameMode, i, x, y
Dim aiCol, aiFound
Dim winX(3), winY(3) ' Speichert die Koordinaten der 4 Gewinnsteine

' Grafik initialisieren
'MODE 1 ' Standard Grafikmodus
Do  
    GoSub Init
	GoSub ShowMenu

	

	' Hauptschleife
	Do
	  GoSub DrawBoard

	  If player = 1 Or gameMode = 1 Then
		' Eingabe ueber Tasten 0-6
		Print @(OFFSET_X-12, 5) "Spieler " + Str$(player) + ": Waehle Spalte (0-6)"
		Do
		  column = Asc(Inkey$) - 48
		Loop Until column >= 0 And column <= 6
	  Else
		Print @(OFFSET_X-12, 5) "Computer denkt nach...          "
		Pause 600
		GoSub CalculateAIMove
		column = aiCol
	  End If

	  ' Stein fallen lassen
	  row = -1
	  For i = 5 To 0 Step -1
		If board(column, i) = 0 Then
		  board(column, i) = player
		  row = i
		  Exit For
		End If
	  Next i

	  If row <> -1 Then
		moves = moves + 1
		GoSub CheckWin
		If win = 1 Then
		  GoSub DrawBoard
		  GoSub AnimateWin ' <--- NEU: Blink-Animation
		   Box 0, 0, MM.HRES,15, 1, RGB(BLACK), RGB(BLACK)
		  Text MM.HRES/2, 10, "SIEGER: SPIELER " + Str$(player), "CM", 1, 2, RGB(YELLOW)
		  Pause 3000
		  Exit Do
		ElseIf moves = 42 Then
		  GoSub DrawBoard
		  Box 0, 0, MM.HRES,15, 1, RGB(BLACK), RGB(BLACK)
		  Text MM.HRES/2, 10, "UNENTSCHIEDEN!", "CM", 1, 2, RGB(WHITE)
		  Pause 3000
		  Exit Do
		End If
		player = 3 - player
	  End If
	Loop
Loop

' --- GRAFIK UNTERPROGRAMME ---

ShowMenu:
  CLS
  Local i, animX
  
  Text MM.HRES/2, 6, "** Zachzig presents **", "CM", 7, 1, RGB(BLUE)
  Text MM.HRES/2, 50, "4 GEWINNT", "CM", 1, 3, RGB(CYAN)
  Text MM.HRES/2, 100, "1: Mensch vs Mensch", "CM", 1, 1, RGB(WHITE)
  Text MM.HRES/2, 130, "2: Mensch vs Computer", "CM", 1, 1, RGB(WHITE)
  ' Animation: Steine rollen herein
  For i = 0 To 80
    ' Alter Zustand loeschen (schwarz)
    Circle MM.HRES/2-120+i-2, 160, 12, 0, 1, RGB(BLACK), RGB(BLACK)
    Circle MM.HRES/2+120-i+2, 160, 12, 0, 1, RGB(BLACK), RGB(BLACK)
    ' Neuer Zustand zeichnen
    Circle MM.HRES/2-120+i, 160, 12, 1, 1, RGB(YELLOW), RGB(YELLOW) ' Spieler 1 rollt von links
    Circle MM.HRES/2+120-i, 160, 12, 1, 1, RGB(RED), RGB(RED)    ' Spieler 2 rollt von rechts
    Pause 15
  Next i
  Do
    gameMode = Asc(Inkey$) - 48
  Loop Until gameMode = 1 Or gameMode = 2
  CLS
Return

DrawBoard:
  ' Zeichne das blaue Brett
  Local c
  Box OFFSET_X-5, OFFSET_Y-5, 7*BOX_SIZE+10, 6*BOX_SIZE+10, 1, RGB(BLUE), RGB(BLUE)
  For x=0 To 6
   Print @(OFFSET_X+2 + x*BOX_SIZE,OFFSET_Y + 6*BOX_SIZE + BOX_SIZE/2) x
  Next x
  For y = 0 To 5
    For x = 0 To 6
      c = RGB(BLACK) ' Leer
      If board(x, y) = 1 Then c = RGB(YELLOW)
      If board(x, y) = 2 Then c = RGB(RED)

      ' Zeichne die Loecher/Steine
      Circle OFFSET_X + x*BOX_SIZE + BOX_SIZE/2, OFFSET_Y + y*BOX_SIZE + BOX_SIZE/2, BOX_SIZE/2-2, 1, 1, c, c
    Next x
  Next y
Return

' --- VERBESSERTE KI LOGIK GEGEN WAAGERECHTE FALLEN ---

CalculateAIMove:
  aiFound = 0
  Local i, weight, maxWeight = -100
  
  ' 1. SOFORTIGER EIGENER GEWINN (Prioritaet 1) [cite: 5]
  For i = 0 To 6
    If IsWinningMove(i, 2) Then 
      aiCol = i : aiFound = 1 : Return 
    End If
  Next i

  ' 2. SPIELER 1 BLOCKIEREN (DIREKTE GEFAHR) (Prioritaet 2) [cite: 5, 6]
  For i = 0 To 6
    If IsWinningMove(i, 1) Then 
      aiCol = i : aiFound = 1 : Return 
    End If
  Next i

  ' 3. SPEZIAL-REAKTION: MITTEN-ERÖFFNUNG BLOCKEN
  ' Wenn Spieler 1 in Spalte 3 liegt und die KI noch keinen Stein daneben hat
  If board(3, 5) = 1 And moves < 3 Then
    ' Besetze sofort eines der angrenzenden Felder (2 oder 4), 
    ' um die waagerechte Ausbreitung zu stoppen.
    If board(2, 5) = 0 Then aiCol = 2 : aiFound = 1 : Return
    If board(4, 5) = 0 Then aiCol = 4 : aiFound = 1 : Return
  End If

  ' 4. STRATEGISCHE BEWERTUNG 
  aiCol = 3 ' Standardzug Mitte
  For i = 0 To 6
    If board(i, 0) = 0 Then
      ' Grundgewichtung: Mitte ist am staerksten 
      weight = 3 - Abs(3 - i)
      
      ' Waagerechte Flaechen-Kontrolle:
      ' Erhoehe das Gewicht, wenn der Zug eine 2er oder 3er Kette des Gegners unterbricht,
      ' auch wenn es noch kein direkter 4er Siegzug ist.
      weight = weight + CheckHorizontalThreat(i)

      ' VERHINDERT VORLAGEN [cite: 6, 8]
      If WouldEnableOpponent(i) Then 
        weight = weight - 20 
      End If
      
      If weight > maxWeight Then
        maxWeight = weight : aiCol = i
      ElseIf weight = maxWeight And Rnd > 0.5 Then
        aiCol = i
      End If
    End If
  Next i
Return

' Hilfsfunktion zur Erkennung waagerechter Bedrohungen
Function CheckHorizontalThreat(c)
  Local r, r_f = -1, score = 0
  ' Finde Reihe
  For r = 5 To 0 Step -1: If board(c, r) = 0 Then r_f = r : Exit For : Next r
  If r_f = -1 Then CheckHorizontalThreat = 0 : Exit Function
  
  ' Pruefe direkte Nachbarn (waagerecht)
  ' Wenn links und rechts von der aktuellen Position Steine von Spieler 1 sind,
  ' ist dies eine hochprioritaere Block-Stelle.
  If c > 0 Then If board(c-1, r_f) = 1 Then score = score + 5
  If c < 6 Then If board(c+1, r_f) = 1 Then score = score + 5
  
  CheckHorizontalThreat = score
End Function

' Hilfsfunktion: Prüft, ob ein Stein für Spieler p zum Sieg führt
Function IsWinningMove(c, p)
  Local r, r_found = -1, old_p = player
  IsWinningMove = 0
  ' Finde die tiefste freie Reihe in der Spalte
  For r = 5 To 0 Step -1
    If board(c, r) = 0 Then r_found = r : Exit For 
  Next r
  
  ' Simuliere den Zug
  If r_found <> -1 Then
    board(c, r_found) = p
    ' Nutze das bestehende CheckWin Unterprogramm
    player = p : GoSub CheckWin
    If win = 1 Then IsWinningMove = 1
    ' Setze den Zustand für die Simulation zurück
    win = 0 : player = old_p : board(c, r_found) = 0
  End If
End Function

' Hilfsfunktion: Verhindert, dass die KI dem Gegner einen Sieg "auflegt"
Function WouldEnableOpponent(c)
  Local r, r_f = -1
  WouldEnableOpponent = 0
  ' Finde die Reihe, in der der KI-Stein landen würde
  For r = 5 To 0 Step -1
    If board(c, r) = 0 Then r_f = r : Exit For
  Next r
  
  ' Wenn darüber noch Platz ist, simuliere, ob der Spieler dort gewinnen könnte
  If r_f > 0 Then
    board(c, r_f) = 2 ' KI setzt Test-Stein
    If IsWinningMove(c, 1) Then WouldEnableOpponent = 1 ' Spieler könnte direkt darüber gewinnen
    board(c, r_f) = 0 ' Zurücksetzen
  End If
End Function

CheckWin:
  ' Horizontal [cite: 9]
  For y = 0 To 5
    For x = 0 To 3
      If board(x,y)=player And board(x+1,y)=player And board(x+2,y)=player And board(x+3,y)=player Then
        win=1: For i=0 To 3: winX(i)=x+i: winY(i)=y: Next i: Return
      End If
    Next x
  Next y
  ' Vertikal [cite: 9]
  For x = 0 To 6
    For y = 0 To 2
      If board(x,y)=player And board(x,y+1)=player And board(x,y+2)=player And board(x,y+3)=player Then
        win=1: For i=0 To 3: winX(i)=x: winY(i)=y+i: Next i: Return
      End If
    Next y
  Next x
  ' Diagonal 
  For x = 0 To 3
    For y = 0 To 2
      If board(x,y)=player And board(x+1,y+1)=player And board(x+2,y+2)=player And board(x+3,y+3)=player Then
        win=1: For i=0 To 3: winX(i)=x+i: winY(i)=y+i: Next i: Return
      End If
    Next y
    For y = 3 To 5
      If board(x,y)=player And board(x+1,y-1)=player And board(x+2,y-2)=player And board(x+3,y-3)=player Then
        win=1: For i=0 To 3: winX(i)=x+i: winY(i)=y-i: Next i: Return
      End If
    Next y
  Next x
Return

AnimateWin:
  Local blink, j
  Local c_win = RGB(WHITE)
  If player = 1 Then c_win = RGB(YELLOW) Else c_win = RGB(RED)
  
  ' 10-mal blinken lassen
  For blink = 1 To 10
    ' Linie zeichnen
    Line OFFSET_X + winX(0)*BOX_SIZE + BOX_SIZE/2, OFFSET_Y + winY(0)*BOX_SIZE + BOX_SIZE/2,OFFSET_X + winX(3)*BOX_SIZE + BOX_SIZE/2, OFFSET_Y + winY(3)*BOX_SIZE + BOX_SIZE/2, 3, RGB(WHITE)
    Pause 200
    ' Linie loeschen (mit Spielfeldfarbe uebermalen)
    Line OFFSET_X + winX(0)*BOX_SIZE + BOX_SIZE/2, OFFSET_Y + winY(0)*BOX_SIZE + BOX_SIZE/2,OFFSET_X + winX(3)*BOX_SIZE + BOX_SIZE/2, OFFSET_Y + winY(3)*BOX_SIZE + BOX_SIZE/2, 3, RGB(BLUE)
    Pause 200
  Next blink
  ' Am Ende Linie dauerhaft stehen lassen
  Line OFFSET_X + winX(0)*BOX_SIZE + BOX_SIZE/2, OFFSET_Y + winY(0)*BOX_SIZE + BOX_SIZE/2,OFFSET_X + winX(3)*BOX_SIZE + BOX_SIZE/2, OFFSET_Y + winY(3)*BOX_SIZE + BOX_SIZE/2, 3, RGB(WHITE)
Return

Init:
  For y = 0 To 5
	For x = 0 To 6
	  board(x, y) = 0 
	Next x
  Next y
  player = 1
  moves = 0
  win = 0
Return  
  

