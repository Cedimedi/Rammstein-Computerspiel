' GAMEOVER.FRM
Option Explicit

Sub Form_KeyPress (KeyAscii As Integer)
  End
End Sub

Sub Form_Load ()
  frmGameOver.Move 0, 0, Screen.Width, Screen.Height
  picBack.Left = (frmGameOver.Width - picBack.ScaleWidth) / 2
  picBack.Top = (frmGameOver.Height - picBack.ScaleHeight) / 2
End Sub
