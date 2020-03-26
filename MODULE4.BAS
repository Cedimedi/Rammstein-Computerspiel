' Module4
Option Explicit

Sub sub0204 (p0022 As Form)
Dim l0024 As Integer
Dim l0026 As Integer
Dim l0028 As Single
Dim l002A As Single
Dim l002C
  l0024% = 15
  l0026% = 15
  l0028 = Screen.TwipsPerPixelX / l0024%
  l002A = Screen.TwipsPerPixelY / l0026%
  p0022.Move (Screen.Width - (p0022.Width * l0028)) / 2 - 1, (Screen.Height - (p0022.Height * l002A)) / 2 - 1, p0022.Width * l0028, p0022.Height * l002A
  If  l0028 = 1 And l002A = 1 Then Exit Sub
  For l002C = 0 To p0022.Controls.Count - 1
    If  TypeOf p0022.Controls(l002C) Is Timer Then
    ElseIf TypeOf p0022.Controls(l002C) Is DriveListBox Then
      p0022.Controls(l002C).Move p0022.Controls(l002C).Left * l0028, p0022.Controls(l002C).Top, p0022.Controls(l002C).Width * l0028
    ElseIf TypeOf p0022.Controls(l002C) Is ComboBox Then
      If  p0022.Controls(l002C).Style <> 1 Then
        p0022.Controls(l002C).Move p0022.Controls(l002C).Left * l0028, p0022.Controls(l002C).Top * l002A, p0022.Controls(l002C).Width * l0028
      End If
    ElseIf TypeOf p0022.Controls(l002C) Is PictureBox Then
      If  p0022.Controls(l002C).AutoSize = True Then
        p0022.Controls(l002C).Move p0022.Controls(l002C).Left * l0028, p0022.Controls(l002C).Top * l002A
      Else
        p0022.Controls(l002C).Move p0022.Controls(l002C).Left * l0028, p0022.Controls(l002C).Top * l002A, p0022.Controls(l002C).Width * l0028, p0022.Controls(l002C).Height * l002A
      End If
    ElseIf TypeOf p0022.Controls(l002C) Is Line Then
      p0022.Controls(l002C).X1 = p0022.Controls(l002C).X1 * l0028
      p0022.Controls(l002C).Y1 = p0022.Controls(l002C).Y1 * l002A
      p0022.Controls(l002C).X2 = p0022.Controls(l002C).X2 * l0028
      p0022.Controls(l002C).Y2 = p0022.Controls(l002C).Y2 * l002A
    Else
      p0022.Controls(l002C).Move p0022.Controls(l002C).Left * l0028, p0022.Controls(l002C).Top * l002A, p0022.Controls(l002C).Width * l0028, p0022.Controls(l002C).Height * l002A
      If  TypeOf p0022.Controls(l002C) Is TextBox Then
        p0022.Controls(l002C).FontSize = p0022.Controls(l002C).FontSize * l0028
      ElseIf TypeOf p0022.Controls(l002C) Is Label Then
        p0022.Controls(l002C).FontSize = p0022.Controls(l002C).FontSize * l0028
      End If
    End If
  Next l002C
End Sub

Sub Main ()
Dim l0032 As Integer
Dim l0034 As Integer
Dim l0036 As String
  If  App.PrevInstance Then
    End
  End If
  gv01BE = 1
  gv01C2 = 30
  gv01C6 = 0
  gv01CA = 0
  gv01CE = False
  gv01D0 = 80
  gv01D2 = False
  l0034 = Len(Command$)
  If  (l0034 > 0) Then
    If  (InStr(Command$, "/GODMODE") > 0) Then
      gv01C2 = 100
      gv01CE = True
      gv01D2 = True
      l0036 = "Godmode aktiviert" & Chr(13)
    End If
    l0032 = InStr(Command$, "/T")
    If  (l0032 > 0) Then
      gv01D0 = Int(Mid(Command$, l0032 + 2, l0034 - (l0032 + 1)))
      l0036 = l0036 & "Timer-Interval=" & Str$(gv01D0)
    End If
    If  (Len(l0036) > 0) Then
      MsgBox l0036, 0, "Rammstein"
    End If
  End If
  frmStart.Show
End Sub
