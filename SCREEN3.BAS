' SCREEN3.FRM
Option Explicit
Dim m001C As Integer
Const mc001E = 264 ' &H108%
Const mc0020 = 8 ' &H8%
Dim m0022(264) As Integer
Dim m0038(264) As Integer
Dim m004E As Integer
Dim m0050
Dim m0052 As Integer
Dim m0054 As Integer
Dim m0056 As Integer
Const mc0058 = 1 ' &H1%
Const mc005A = 2 ' &H2%
Dim m005C As Integer
Const mc005E = 1 ' &H1%
Const mc0060 =-1 ' &HFFFF%
Dim m0062 As Integer
Dim m0064 As Integer
Dim m0066 As Integer
Dim m0068 As Integer
Dim m006A As Integer
Dim m006C
Dim m006E
Dim m0070 As Integer
Dim m0072 As Integer
Dim m0074 As Integer
Dim m0076 As Integer
Dim m0078 As Integer
Dim m007A As Integer
Dim m007C As Integer
Dim m007E As Integer
Dim m0080(10) As Integer
Dim m0096(10) As Integer
Dim m00AC(10) As Integer
Dim m00C2 As Integer
Dim m00C4 As Integer
Dim m00C6 As Integer
Dim m00C8 As Integer
Dim m00CA As Integer
Dim m00CC As Integer
Dim m00CE As Integer
Dim m00D0 As Integer
Dim m00D2 As Integer
Dim m00D4 As Integer
Dim m00D6 As Integer
Dim m00D8 As Integer
Dim m00DA As Integer
Dim m00DC As Integer
Dim m00DE As Integer
Const mc00E0 = 8 ' &H8%
Dim m00E4(8) As T0534
Dim m00FA As Integer
Dim m00FC As Integer
Const mc0102 = 2 ' &H2%
Const mc0104 = 3 ' &H3%
Const mc0106 = 4 ' &H4%
Const mc0108 = 5 ' &H5%
Const mc010A = 6 ' &H6%
Const mc010C = 7 ' &H7%
Const mc010E = 8 ' &H8%
Const mc012E = 1 ' &H1%
Const m0158 = 0 ' &H0%
Const mc018C = 2 ' &H2%

Function fn0240 (p0112 As Variant, p0116 As Variant, p011A As Variant) As Integer
Dim l011E
Dim l0120 As Integer
Dim l0122 As Integer
  If  (p0112 < p0116) Then
    l0120 = p0112
    l0122 = p0116
  Else
    l0120 = p0116
    l0122 = p0112
  End If
  fn0240 = -1
  For l011E = 0 To mc00E0
    If  (m00E4(l011E).M054A) Then
      If  (m00E4(l011E).M0543 <= l0122 And m00E4(l011E).M0543 >= l0120) Then
        If  (p011A > 0) Then
          If  (m00E4(l011E).M060B - 30 <= p011A And m00E4(l011E).M060B + 30 >= p011A) Then
            fn0240 = l011E
          End If
        Else
          fn0240 = l011E
        End If
      End If
    End If
  Next l011E
End Function

Sub sub0259 ()
Dim l0124
Dim l0126
Dim l0128 As Integer
Dim l012A As Integer
Dim l012C As Integer
' Const mc012E = 1 ' &H1%
    l0128 = fn0240(m006C, (m006C + (m0070 * 4)), m006E)
    If  (l0128 > -1) Then
      If  (m00E4(l0128).M0553 = mc012E) Then
        If  (m0054 = mc005A) Then
          m0052 = m0052 + 1
          m004E = m004E + m005C
        End If
        gv01C6 = gv01C6 + m00E4(l0128).M0627
        lblPunkte.Caption = Str$(gv01C6)
        If  (m00E4(l0128).M0627 > 0) Then
          If  (l0128 = mc0106 Or l0128 = mc0108) Then
            Call sub0100("TOD1.WAV")
          Else
            Call sub0100("P" & Trim(Str$(m00E4(l0128).M0627)) & ".WAV")
          End If
        Else
          If  (l0128 = mc010C) Then
            Call sub0100("P2000.WAV")
          End If
        End If
        If  (l0128 = mc010E) Then
          l012A = m00D8
          l012C = 12
        Else
          If  (m004E > 176) Then
            l012A = m00D6
          Else
            l012A = m00DA
          End If
          l012C = 9
        End If
        For l0124 = 0 To l012C
          For l0126 = 0 To mc00E0
            If  (m00E4(l0126).M05F1 = True) Then
              m001C = extfn01A1(m00E4(l0126).M0568, m00E4(l0126).M0573, m00E4(l0126).M0581, m00E4(l0126).M058F, 0)
            End If
          Next l0126
          If  (m0054 = mc005A) Then
            m001C = extfn01A1(m00D4, m0022(m004E), m0038(m004E) + m00AC(m0052), m0052, 1)
          Else
            m001C = extfn01A1(m00D2, m0022(m004E) + (m0080(m0052) * m005C), m0038(m004E), m0052, 1)
          End If
          m001C = extfn01A1(l012A, m00E4(l0128).M0573 + m00E4(l0128).M0617, m00E4(l0128).M060B - 30, l0124, 0)
          m001C = extfn01B5(picScreen.hWnd)
        Next l0124
        m006A = 0
        m0062 = 0
        If  (l0128 = m00FA) Then
          m00FA = -1
          m00E4(mc0106).M05DC = False
          m00E4(mc0108).M05DC = False
          timerMovingFire.Enabled = False
        End If
        m00E4(l0128).M05DC = False
        m00E4(l0128).M054A = False
        m00E4(l0128).M05F1 = False
      End If
      If  (m00E4(l0128).M05FA > -1) Then
        m00E4(m00E4(m006C).M05FA).M05F1 = True
      End If
    Else
      If  (m006A > 6) Then
        m0062 = m0062 - 1
      End If
      m006A = m006A - 1
      m006C = m006C + (m0070 * 4)
      If  (m006C >= mc001E Or m006C < 0) Then
        m006A = 0
        m0062 = 0
      End If
    End If
End Sub

Sub sub0277 ()
  gv01BE = gv01BE - 1
  m004E = 10
  If  (gv01D2) Then gv01BE = 1
  If  (gv01BE = 0) Then
    frmGameOver.Show
    Unload Me
    Exit Sub
  End If
End Sub

Sub sub0285 ()
Dim l0150
Dim l0152
Dim l0154 As Integer
Dim l0156 As Integer
' Const m0158 = 0 ' &H0%
    l0154 = fn0240(m0050, m004E, 0)
    If  (l0154 > -1) Then
      If  (l0154 = mc0102 Or l0154 = mc0104) Then
        If  (m0054 = mc005A And m0052 >= 2 And m0052 < 5) Then
          Exit Sub
        End If
      End If
      If  (m00E4(l0154).M055E = m0158) Then
        m004E = m0050
        m0054 = m0056
      End If
      If  (m00E4(l0154).M055E = mc012E) Then
        Call sub0100("TOD1.WAV")
        If  (m004E > 176) Then
          l0156 = m00D8
        Else
          l0156 = m00DC
        End If
        For l0150 = 0 To 12
          For l0152 = 0 To mc00E0
            If  (m00E4(l0152).M05F1 = True) Then
              m001C = extfn01A1(m00E4(l0152).M0568, m00E4(l0152).M0573, m00E4(l0152).M0581, m00E4(l0152).M058F, 0)
            End If
          Next l0152
          m001C = extfn01A1(l0156, m0022(m004E) - 30, m0038(m004E) - 30, l0150, 0)
          m001C = extfn01B5(picScreen.hWnd)
        Next l0150
        m00E4(l0154).M054A = False
        m00E4(l0154).M05F1 = False
        If  (l0154 = mc010E) Then
          m00E4(mc010E).M059F = 2
          m00E4(mc010E).M0543 = 220
          m00E4(mc010E).M0573 = 216
        End If
        Call sub0277
      End If
    End If
End Sub


Sub sub03FC ()
Dim l015A
Dim l015C
Dim l015E As Integer
Dim l0160 As Integer
    If  (m007A > 0 And m007A < 88) Then
      If  (m007E = -1) Then
        l015E = m007A - 4
        l0160 = m007A
      Else
        l015E = m007A
        l0160 = m007A + 4
      End If
      If  (l0160 >= m004E And l015E <= m004E) Then
          Call sub0100("TOD1.WAV")
          For l015A = 0 To 12
            For l015C = 0 To mc00E0
              If  (m00E4(l015C).M05F1 = True) Then
                m001C = extfn01A1(m00E4(l015C).M0568, m00E4(l015C).M0573, m00E4(l015C).M0581, m00E4(l015C).M058F, 0)
              End If
            Next l015C
            If  (m00FA > -1) Then
              m001C = extfn01A1(m00E4(m00FA).M0568, m00E4(m00FA).M0573, m00E4(m00FA).M0581, m00E4(m00FA).M058F, 0)
            End If
            If  (l015A < 6) Then
              m001C = extfn01A1(m00D2, m0022(m004E) + (m0080(m0052) * m005C), m0038(m004E), m0052, 1)
            End If
            m001C = extfn01A1(m00DC, m0022(m004E) - 30, m0038(m004E) - 30, l015A, 0)
            m001C = extfn01B5(picScreen.hWnd)
          Next l015A
          m0078 = 0
          m00E4(mc0106).M05BE = 10
          m00E4(mc0108).M05B2 = 10
          Call sub0277
      Else
        m007A = m007A + (4 * m007E)
        m0078 = m0078 - 1
      End If
    Else
      m0078 = 0
    End If
End Sub

Sub Form_Activate ()
  Call sub0100("ENTER.WAV")
End Sub

Sub Form_KeyDown (KeyCode As Integer, Shift As Integer)
  If  (KeyCode = 32) Then
    If  (gv01C2 > 0 And m006A = 0) Then
      m0062 = 2
      m006A = 8
      m0070 = m005C
      If  (m0070 = mc0060) Then
        m006C = m0050 - 4
        If  (m004E > 176) Then
          m0068 = m0066
        Else
          m0068 = m0072
        End If
      Else
        m006C = m0050 + 1
        If  (m004E > 176) Then
          m0068 = m0064
        Else
          m0068 = m0074
        End If
      End If
      If  (m006C >= mc001E Or m006C < 0) Then
        m006A = 0
        m0062 = 0
      Else
        m006E = m0038(m0050) + 31
        If  (m0054 = mc005A) Then m006E = m006E + m00AC(m0052 + 1)
        Call sub0100("FIRE.WAV")
        gv01C2 = gv01C2 - 1
        lblMunition.Width = gv01C2 * 100
      End If
    End If
  End If
End Sub

Sub Form_Load ()
  Call sub0204(frmScreen3)
  frmScreen3.Move 0, 0, Screen.Width, Screen.Height
  picScreen.Left = (frmScreen3.Width - picScreen.ScaleWidth) / 2
  picScreen.Top = (frmScreen3.Height - picScreen.ScaleHeight) / 2
  timerMain.Interval = gv01D0
  timerMovingFire.Enabled = False
  sub045E
End Sub

Sub Form_Unload (Cancel As Integer)
  timerMain.Enabled = False
  timerMovingFire.Enabled = False
End Sub

Sub sub045E ()
Dim l0180
' Const mc018C = 2 ' &H2%
  lblPunkte.Caption = Str$(gv01C6)
  m001C = extfn0156(picScreen.Picture)
  If  (m001C = 0) Then
    End
  End If
  m00E4(0).M0543 = 74
  m00E4(0).M054A = True
  m00E4(0).M0553 = mc012E
  m00E4(0).M055E = m0158
  m00E4(0).M0568 = extfn01C7(imgKopf5.Picture)
  m00E4(0).M0573 = 24
  m00E4(0).M0581 = 350
  m00E4(0).M058F = 0
  m00E4(0).M059F = 0
  m00E4(0).M05F1 = True
  m00E4(0).M05FA = -1
  m00E4(0).M060B = 407
  m00E4(0).M0617 = 0
  m00E4(0).M0627 = 10000
  m00E4(1).M0543 = 245
  m00E4(1).M054A = True
  m00E4(1).M0553 = mc012E
  m00E4(1).M055E = mc018C
  m00E4(1).M0568 = extfn01C7(imgKopf6.Picture)
  m00E4(1).M0573 = 67
  m00E4(1).M0581 = 1
  m00E4(1).M058F = 0
  m00E4(1).M059F = 0
  m00E4(1).M05F1 = True
  m00E4(1).M05FA = -1
  m00E4(1).M060B = 60
  m00E4(1).M0617 = 3
  m00E4(1).M0627 = 10000
  m00E4(2).M0543 = 112
  m00E4(2).M054A = True
  m00E4(2).M0553 = mc018C
  m00E4(2).M055E = mc012E
  m00E4(2).M0568 = extfn017B(imgIgel.Picture, &HFF0000&, 105, 57)
  m00E4(2).M0573 = 428
  m00E4(2).M0581 = 252
  m00E4(2).M058F = 1
  m00E4(2).M059F = 3
  m00E4(2).M05B2 = 0
  m00E4(2).M05BE = 0
  m00E4(2).M05DC = True
  m00E4(2).M05F1 = True
  m00E4(2).M05FA = -1
  m00E4(2).M0617 = 50
  m00E4(2).M0627 = 0
  m00E4(3).M0543 = 143
  m00E4(3).M054A = True
  m00E4(3).M0553 = mc018C
  m00E4(3).M055E = mc012E
  m00E4(3).M0568 = m00E4(2).M0568
  m00E4(3).M0573 = 179
  m00E4(3).M0581 = 252
  m00E4(3).M058F = 0
  m00E4(3).M059F = 3
  m00E4(3).M05B2 = 0
  m00E4(3).M05BE = 0
  m00E4(3).M05DC = True
  m00E4(3).M05F1 = True
  m00E4(3).M05FA = -1
  m00E4(3).M0617 = 50
  m00E4(3).M0627 = 0
  m00C2 = extfn017B(imgManStepR.Picture, &HFF0000&, 57, 82)
  m00C4 = extfn017B(imgManJumpR.Picture, &HFF0000&, 61, 91)
  m00C6 = extfn017B(imgManStepL.Picture, &HFF0000&, 57, 82)
  m00C8 = extfn017B(imgManJumpL.Picture, &HFF0000&, 61, 91)
  m00E4(4).M0543 = 48
  m00E4(4).M054A = False
  m00E4(4).M0553 = mc012E
  m00E4(4).M055E = mc018C
  m00E4(4).M0568 = m00C2
  m00E4(4).M0573 = 234
  m00E4(4).M0581 = 383
  m00E4(4).M058F = 0
  m00E4(4).M059F = 10
  m00E4(4).M05B2 = 60
  m00E4(4).M05BE = 40
  m00E4(4).M05C8 = -1
  m00E4(4).M05DC = True
  m00E4(4).M05F1 = False
  m00E4(4).M05FA = -1
  m00E4(4).M060B = 400
  m00E4(4).M0617 = 26
  m00E4(4).M0627 = -5000
  m00E4(5).M0543 = 10
  m00E4(5).M054A = False
  m00E4(5).M0553 = mc012E
  m00E4(5).M055E = mc018C
  m00E4(5).M0568 = m00C6
  m00E4(5).M0573 = 232
  m00E4(5).M0581 = 383
  m00E4(5).M058F = 0
  m00E4(5).M059F = 10
  m00E4(5).M05B2 = 40
  m00E4(5).M05BE = 60
  m00E4(5).M05C8 = 1
  m00E4(5).M05DC = True
  m00E4(5).M05F1 = False
  m00E4(5).M05FA = -1
  m00E4(5).M060B = 400
  m00E4(5).M0617 = 26
  m00E4(5).M0627 = 5000
  m00E4(6).M0543 = 45
  m00E4(6).M054A = False
  m00E4(6).M0553 = mc018C
  m00E4(6).M055E = mc018C
  m00E4(6).M0568 = extfn017B(imgEgo.Picture, &HFF0000&, 53, 79)
  m00E4(6).M0573 = 232
  m00E4(6).M0581 = 383
  m00E4(6).M058F = 0
  m00E4(6).M059F = 3
  m00E4(6).M05B2 = 0
  m00E4(6).M05BE = 0
  m00E4(6).M05DC = False
  m00E4(6).M05F1 = False
  m00E4(6).M05FA = -1
  m00E4(6).M0627 = 0
  m00E4(7).M0543 = 138
  m00E4(7).M054A = False
  m00E4(7).M0553 = mc012E
  m00E4(7).M055E = mc012E
  m00E4(7).M0568 = extfn017B(imgFass.Picture, &HFF0000&, 60, 44)
  m00E4(7).M0573 = 300
  m00E4(7).M0581 = 149
  m00E4(7).M058F = 0
  m00E4(7).M059F = 4
  m00E4(7).M05B2 = 0
  m00E4(7).M05BE = 0
  m00E4(7).M05DC = True
  m00E4(7).M05F1 = False
  m00E4(7).M05FA = -1
  m00E4(7).M0617 = 10
  m00E4(7).M0627 = 0
  m00E4(8).M0543 = 220
  m00E4(8).M054A = False
  m00E4(8).M0553 = mc012E
  m00E4(8).M055E = mc012E
  m00E4(8).M0568 = extfn017B(imgMonster.Picture, &HFF0000&, 150, 107)
  m00E4(8).M0573 = 216
  m00E4(8).M0581 = 40
  m00E4(8).M058F = 0
  m00E4(8).M059F = 2
  m00E4(8).M05B2 = 0
  m00E4(8).M05BE = 0
  m00E4(8).M05DC = True
  m00E4(8).M05F1 = False
  m00E4(8).M05FA = -1
  m00E4(8).M060B = 70
  m00E4(8).M0617 = 25
  m00E4(8).M0627 = 5000
  m0066 = extfn0169(imgFireR.Picture, &HFF0000&)
  m0064 = extfn0169(imgFireL.Picture, &HFF0000&)
  m00D6 = extfn017B(imgExplosionKlein.Picture, &HFF0000&, 59, 65)
  m00D8 = extfn017B(imgExplosionGross.Picture, &HFF0000&, 116, 107)
  m00DA = extfn017B(imgImplosionKlein.Picture, &HFF0000&, 59, 65)
  m00DC = extfn017B(imgImplosion.Picture, &HFF0000&, 116, 107)
  m00CA = extfn017B(imgManStepRO.Picture, &HFF0000&, 57, 82)
  m00CC = extfn017B(imgManJumpRO.Picture, &HFF0000&, 61, 91)
  m00CE = extfn017B(imgManStepLO.Picture, &HFF0000&, 57, 82)
  m00D0 = extfn017B(imgManJumpLO.Picture, &HFF0000&, 61, 91)
  m0074 = extfn0169(imgFireLB.Picture, &HFF0000&)
  m0072 = extfn0169(imgFireRB.Picture, &HFF0000&)
  m0076 = m0072
  m00DE = extfn017B(imgLeben.Picture, &HFF0000&, 57, 21)
  For l0180 = 0 To 87
    m0022(l0180) = 640 - l0180 * mc0020
    m0022(l0180 + 88) = 640 - l0180 * mc0020
    m0022(l0180 + 176) = 640 - l0180 * mc0020
  Next l0180
  For l0180 = 0 To 87
    m0038(l0180) = 383
  Next l0180
  For l0180 = 88 To 175
    m0038(l0180) = 225
  Next l0180
  For l0180 = 176 To 263
    m0038(l0180) = 62
  Next l0180
  m0080(0) = 0: m0080(1) = 0: m0080(2) = 0: m0080(3) = 10: m0080(4) = 10
  m0080(5) = 0: m0080(6) = 0: m0080(7) = 0: m0080(8) = 10: m0080(9) = 10
  m00AC(0) = -20: m00AC(1) = -30: m00AC(2) = -40: m00AC(3) = -50
  m00AC(4) = -40: m00AC(5) = -30: m00AC(6) = -20: m00AC(7) = -9
  m004E = 10
  m0052 = 10
  m005C = mc005E
  m0054 = m0056
  If  (gv01CE) Then
    m00D2 = m00C6
    m00D4 = m00C8
  Else
    m00D2 = m00CE
    m00D4 = m00D0
  End If
  m0062 = 0
  m006A = 0
  m0070 = mc005E
  m0068 = m0074
  m00FA = -1
  lblMunition.Width = gv01C2 * 100
End Sub

Sub sub030C ()
  m001C = extfn01A1(m00D4, m0022(m004E), m0038(m004E) + m00AC(m0052), m0052, m0062)
  m0052 = m0052 + 1
End Sub

Sub sub0317 ()
  m0052 = (m0052 + 1) Mod 10
  m001C = extfn01A1(m00D2, m0022(m004E) + (m0080(m0052) * m005C), m0038(m004E), m0052, m0062)
End Sub


Sub sub0322 ()
      m00E4(m00FA).M0543 = m00E4(m00FA).M0543 + m00E4(m00FA).M05C8
      m00E4(m00FA).M058F = m00E4(m00FA).M058F + 1
      If  (m00E4(m00FA).M058F > m00E4(m00FA).M059F) Then
        m00E4(m00FA).M058F = 0
      End If
      If  (m00FA = mc0106) Then
        If  (m00E4(m00FA).M0543 = m00E4(m00FA).M05BE) Then
          m00E4(m00FA).M0543 = m00E4(m00FA).M0543 - m00E4(m00FA).M05C8
          m00FA = mc0108
          m00E4(m00FA).M0543 = m00E4(mc0106).M0543
          m00E4(mc0106).M054A = False
          m00E4(mc0108).M054A = True
        End If
      Else
        If  (m00E4(m00FA).M0543 = m00E4(m00FA).M05BE) Then
          m00E4(m00FA).M0543 = m00E4(m00FA).M0543 - m00E4(m00FA).M05C8
          m00FA = mc0106
          m00E4(m00FA).M0543 = m00E4(mc0108).M0543
          m00E4(mc0106).M054A = True
          m00E4(mc0108).M054A = False
        End If
      End If
      m00E4(m00FA).M0573 = m0022(m00E4(m00FA).M0543) - m00E4(m00FA).M0617
      If  (m00E4(m00FA).M0543 <> m00E4(m00FA).M05BE) Then
        m001C = extfn01A1(m00E4(m00FA).M0568, m00E4(m00FA).M0573 + (m0080(m00E4(m00FA).M058F) * m00E4(m00FA).M05C8), m00E4(m00FA).M0581, m00E4(m00FA).M058F, 0)
      Else
        m00E4(m00FA).M0543 = m00E4(m00FA).M05B2
        m00E4(m00FA).M054A = False
        m00FA = -1
      End If
End Sub

Sub sub0337 ()
Dim l021E
    For l021E = 0 To mc00E0
      If  (m00E4(l021E).M05F1 = True) Then
        If  (m00E4(l021E).M05DC) Then
          If  (l021E = mc0102 Or l021E = mc0104) Then
            If  (m00FC = 0) Then
              m00E4(l021E).M058F = (m00E4(l021E).M058F + 1) Mod (m00E4(l021E).M059F + 1)
            End If
          ElseIf (l021E = mc010E) Then
            If  (m00E4(l021E).M059F = 10) Then
              If  (m00E4(l021E).M058F < 10) Then
                m00E4(l021E).M058F = (m00E4(l021E).M058F + 1)
              Else
                m00E4(mc010E).M0543 = m00E4(mc010E).M0543 - 3
                m00E4(mc010E).M0573 = m00E4(mc010E).M0573 + 24
              End If
            Else
              m00E4(l021E).M058F = (m00E4(l021E).M058F + 1) Mod (m00E4(l021E).M059F + 1)
            End If
          Else
            m00E4(l021E).M058F = (m00E4(l021E).M058F + 1) Mod (m00E4(l021E).M059F + 1)
          End If
        End If
        m001C = extfn01A1(m00E4(l021E).M0568, m00E4(l021E).M0573, m00E4(l021E).M0581, m00E4(l021E).M058F, 0)
      End If
    Next l021E
    If  (m0054 = m0056) Then
      m001C = extfn01A1(m00D2, m0022(m004E) + (m0080(m0052) * m005C), m0038(m004E), m0052, m0062)
    ElseIf (m0054 = mc0058) Then
      Call sub0317
    ElseIf (m0054 = mc005A) Then
      Call sub030C
    End If
    If  (m006A > 1) Then
      m001C = extfn0192(m0068, m0022(m006C), m006E)
    End If
    If  (m00FA > -1) Then
      Call sub0322
    End If
    If  (m0078 > 0) Then
      m001C = extfn0192(m0076, m0022(m007A), m007C)
    End If
    m001C = extfn01A1(m00DE, 583, 459, gv01BE - 1, 0)
    m001C = extfn01B5(picScreen.hWnd)
End Sub


Sub timerMain_Timer ()
Dim l0224
Dim l0226
  gv01CA = gv01CA + 1
  m00FC = (m00FC + 1) Mod 3
  m0050 = m004E
  Call sub00BB
  If  (m0054 = mc005A And m0052 <= 7) Then
    m004E = m004E + m005C
    If  (Asc(Mid(gv00B2, 40, 1)) > 2) Then
      m004E = m004E - 1
    End If
    If  (Asc(Mid(gv00B2, 38, 1)) > 2) Then
      m004E = m004E + 1
    End If
  Else
    If  (m0054 = mc005A) Then
      m0052 = 0
    End If
    m0054 = m0056
    If  (Asc(Mid(gv00B2, 40, 1)) > 2) Then
      m0054 = mc0058
      m005C = mc0060
      m004E = m004E - 1
      If  (gv01CE And m004E < 175) Then
        m00D2 = m00C2
        m00D4 = m00C4
      Else
        m00D2 = m00CA
        m00D4 = m00CC
      End If
    End If
    If  (Asc(Mid(gv00B2, 38, 1)) > 2) Then
      m0054 = mc0058
      m005C = mc005E
      m004E = m004E + 1
      If  (gv01CE And m004E < 175) Then
        m00D2 = m00C6
        m00D4 = m00C8
      Else
        m00D2 = m00CE
        m00D4 = m00D0
      End If
    End If
    If  (Asc(Mid(gv00B2, 39, 1)) > 2) Then
      m0054 = mc005A
      m0052 = 0
    End If
  End If
  If  (m0054 > m0056) Then
    Call sub0285
    If  (m004E >= mc001E) Then
      m004E = 0
      frmOutro.Show
      Unload Me
      Exit Sub
    End If
    If  (m004E < 0) Then
      m0054 = m0056
      m004E = m0050
    End If
    If  (m004E > 40 And gv01CE = False) Then
        Call sub0100("TOD1.WAV")
        For l0224 = 0 To 12
          For l0226 = 0 To mc00E0
            If  (m00E4(l0226).M05F1 = True) Then
              m001C = extfn01A1(m00E4(l0226).M0568, m00E4(l0226).M0573, m00E4(l0226).M0581, m00E4(l0226).M058F, 0)
            End If
          Next l0226
          m001C = extfn01A1(m00DC, m0022(m004E) - 30, m0038(m004E) - 30, l0224, 0)
          m001C = extfn01B5(picScreen.hWnd)
        Next l0224
        Call sub0277
    End If
    If  (m00E4(mc0106).M05DC) Then
      If  (m004E > 58 And m00FA = -1) Then
        m00E4(mc010A).M05F1 = True
        m00E4(mc010A).M05DC = True
      End If
    End If
    If  (m004E > 100 And m004E < 165) Then
      If  (m00E4(mc010C).M054A = False) Then
        m00E4(mc010C).M05DC = True
        m00E4(mc010C).M0581 = 149
        m00E4(mc010C).M0573 = m0022(m004E + 10 * m005C)
        m00E4(mc010C).M0543 = m004E + (8 * m005C)
        m00E4(mc010C).M05BE = m00E4(mc010C).M0543 - 88
        m00E4(mc010C).M054A = True
        m00E4(mc010C).M05F1 = True
      End If
    End If
    If  (m004E > 170) Then
      If  (m00E4(mc010E).M05DC) Then
        m00E4(mc010E).M05F1 = True
        m00E4(mc010E).M054A = True
      End If
    End If
    If  (m004E > 210) Then
      If  (m00E4(mc010E).M05F1) Then
        Call sub0100("MONSTER.WAV")
        m00E4(mc010E).M059F = 10
      End If
    End If
    m0050 = m004E
  End If
  If  (m00E4(mc010A).M05F1 = True) Then
    If  (m00FA = -1) Then
      If  (m00E4(mc010A).M058F = m00E4(mc010A).M059F) Then
        m00FA = mc0106
        m00E4(mc0106).M054A = True
        timerMovingFire.Enabled = True
        m00E4(mc010A).M05F1 = False
        m00E4(mc010A).M05DC = False
      End If
    End If
  End If
  If  (m00E4(mc010C).M054A = True) Then
    m00E4(mc010C).M0581 = m00E4(mc010C).M0581 + 10
    If  (m00E4(mc010C).M0581 > 280) Then
      m00E4(mc010C).M0543 = m00E4(mc010C).M05BE
    End If
    If  (m00E4(mc010C).M0581 > 480) Then
      m00E4(mc010C).M054A = False
      m00E4(mc010C).M05F1 = False
    End If
    m00E4(mc010C).M060B = m00E4(mc010C).M0581 + 20
  End If
  If  (m00E4(mc010E).M058F = 10) Then
    If  (m00E4(mc010E).M05F1) Then
      m0050 = m004E + 6
      Call sub0285
      m0050 = m004E
    End If
  End If
  If  (m006A > 0) Then
    Call sub0259
  End If
  If  (m0078 > 0) Then
    Call sub03FC
  End If
  Call sub0337
End Sub


Sub timerMovingFire_Timer ()
  If  (m0078 = 0) Then
    m007C = m00E4(m00FA).M0581 + 32
    m007E = m00E4(m00FA).M05C8
    m007A = m00E4(m00FA).M0543 + (m007E * 3)
    If  (m007E = mc0060) Then
      m0076 = m0072
    Else
      m0076 = m0074
    End If
    m0078 = 8
  End If
End Sub
