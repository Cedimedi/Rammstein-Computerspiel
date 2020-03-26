' START.FRM
Option Explicit

Sub Form_KeyPress (KeyAscii As Integer)
  frmScreen1.Show
  Unload frmStart
End Sub

Sub Form_Load ()
  frmStart.Move 0, 0, Screen.Width, Screen.Height
  picBack.Left = (frmStart.Width - picBack.ScaleWidth) / 2
  picBack.Top = (frmStart.Height - picBack.ScaleHeight) / 2
  Load frmScreen1
End Sub

Sub picBack_Click ()
  frmScreen1.Show
  Unload frmStart
End Sub
