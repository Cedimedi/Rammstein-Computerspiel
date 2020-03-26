' LOADING.FRM
Option Explicit
Const mc0026 = 1 ' &H1%
Const mc002C = 2 ' &H2%
Const mc0032 = 3 ' &H3%

Sub Form_Activate ()
    lblText.Caption = "Lade Level" & Str$(gv01B2)
    frmLoading.Refresh
    If  (gv01B2 = mc0026) Then
      Load frmScreen1
      frmScreen1.Show
      Unload frmLoading
      Exit Sub
    ElseIf (gv01B2 = mc002C) Then
      Load frmScreen2
      frmScreen2.Show
      Unload frmLoading
      Exit Sub
    ElseIf (gv01B2 = mc0032) Then
      Load frmScreen3
      frmScreen3.Show
      Unload frmLoading
      Exit Sub
    End If
End Sub

Sub Form_KeyPress (KeyAscii As Integer)
  If  (KeyAscii = 27) Then
    End
  End If
End Sub

Sub Form_Load ()
  frmLoading.Move 0, 0, Screen.Width, Screen.Height
  lblText.Left = (frmLoading.Width - lblText.Width) / 2
  lblText.Top = (frmLoading.Height - lblText.Height) / 2
End Sub
