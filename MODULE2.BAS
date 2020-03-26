' Module2
Option Explicit
Declare Function extfn00F0 Lib "MMSystem" Alias "sndPlaySound" (ByVal p1$, ByVal p2%) As Integer
Dim m0008 As Integer
Const mc000A = 1 ' &H1%
Const mc0010 = 8 ' &H8%

Sub sub0100 (ByVal p0014 As String)
Dim l0016 As Integer
  l0016 = extfn00F0(App.Path & "\WAV\" & p0014, mc000A)
End Sub

Sub sub010D (ByVal p001C As String)
Dim l001E As Integer
  l001E = extfn00F0(App.Path & "\WAV\" & p001C, mc0010 + mc000A)
End Sub

Sub sub011E (p0020 As String)
Dim l0022 As Integer
  l0022 = extfn00F0(App.Path & "\WAV\" & p0020, m0008)
End Sub
