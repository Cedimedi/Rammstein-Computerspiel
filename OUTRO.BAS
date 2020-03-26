' OUTRO.FRM
Option Explicit

Sub btnSave_MouseDown (Button As Integer, Shift As Integer, X As Single, Y As Single)
  imgKnopf.Visible = True
End Sub

Sub btnSave_MouseUp (Button As Integer, Shift As Integer, X As Single, Y As Single)
Dim l0030 As String
Dim l0032 As Integer
Dim l0034 As String
Dim l0036 As String
Dim l0038 As String
Dim l003A
  l0036 = "0000000000"
  l0034 = Trim(Str$(gv01C6 + (gv01C2 * 1000) - (gv01CA * 10)))
  l0038 = Left(l0036, 10 - Len(l0034)) & l0034
    imgKnopf.Visible = False
    On Error GoTo LC068
    l0032 = FreeFile
    l0030 = "C:\HISCORE.TXT"
    Open l0030 For Output As l0032
    Print #l0032, " "
    Print #l0032, " "
    Print #l0032, " "
    Print #l0032, " "
    Print #l0032, " "
    Print #l0032, " "
    Print #l0032, "      Rammstein-Spielergebnis"
    Print #l0032, " "
    Print #l0032, " "
    Print #l0032, " "
    l0036 = ""
    For l003A = 10 To 1 Step -1
      l0036 = l0036 & Str$(Int(10 * Rnd))
      l0036 = l0036 & " " & Mid$(l0038, l003A, 1)
    Next l003A
    Print #l0032, "      Dein verschlüsseltes Highscore-Ergebnis: " & l0036
    Print #l0032, " "
    Print #l0032, " "
    l0036 = ""
    For l003A = 1 To 10
      l0036 = l0036 & Str$(Int(10 * Rnd))
      l0036 = l0036 & Str$((Val(Mid$(l0038, l003A, 1)) + l003A) Mod 10)
    Next l003A
    Print #l0032, "      Kontrollziffer:                          " & l0036
    Close
    MsgBox "Das Ergebnis wurde in Datei C:\HISCORE.TXT gesichert." & Chr(13) & "Zum Beenden des Spiels Esc betätigen.", 0, "Rammstein"
    Exit Sub

LC068:
    MsgBox "Fehler beim Sichern des Ergebnises in Datei C:\HISCORE.TXT" & Chr(13) & "Zum Beenden des Spiels Esc betätigen.", 0, "Rammstein"
    Exit Sub
End Sub

Sub Form_Activate ()
    gv01CA = gv01CA / (1000 / gv01D0)
    lblOut(0).Caption = Str$(gv01C6)
    lblOut(1).Caption = Str$(gv01C2 * 1000)
    lblOut(2).Caption = Str$(gv01CA * -10)
    lblOut(3).Caption = Str$(gv01C6 + (gv01C2 * 1000) - (gv01CA * 10))
    frmLoading.Refresh
End Sub

Sub Form_KeyPress (KeyAscii As Integer)
  If  (KeyAscii = 27) Then
    End
  End If
End Sub

Sub Form_Load ()
  Call sub0204(frmOutro)
  frmOutro.Move 0, 0, Screen.Width, Screen.Height
  picBack.Left = (frmOutro.Width - picBack.ScaleWidth) / 2
  picBack.Top = (frmOutro.Height - picBack.ScaleHeight) / 2
End Sub
