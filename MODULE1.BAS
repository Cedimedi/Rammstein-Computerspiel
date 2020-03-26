' Module1
Option Explicit
Declare Sub extsub0083 Lib "User" Alias "GetKeyboardState" (ByVal p1$)
Declare Sub extsub009F Lib "User" Alias "SetKeyboardState" ()

Sub sub00BB ()
  gv00B2 = Space$(256)
  Call extsub0083(gv00B2)
End Sub
