' SCREEN2.FRM
Option Explicit
Dim m001C As Integer
Const mc001E = 264 ' &H108%
Const mc0020 = 8 ' &H8%
Dim m0022(264) As Integer
Dim m0038(264) As Integer
Dim m004E As Integer
Dim m0050 As Integer
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
Dim m006C As Integer
Dim m006E As Integer
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
Const mc00D4 = 8 ' &H8%
Dim m00D8(8) As T0534
Dim m00EE As Integer
Dim m00F0 As Integer
Dim m00F2 As Integer
Dim m00F4 As Integer
Const mc00FA = 2 ' &H2%
Const mc00FE = 4 ' &H4%
Const mc0102 = 6 ' &H6%
Const mc0104 = 7 ' &H7%
Const mc0106 = 8 ' &H8%
Const mc0120 = 1 ' &H1%
Const mc014A = 4 ' &H4%
Const mc014C = 6 ' &H6%
Const mc017C = 2 ' &H2%
Const mc020C = 3 ' &H3%

Function fn0240 (ByVal pv010A As Integer, ByVal pv010C As Integer, ByVal pv010E As Integer) As Integer
Dim l0110
Dim l0112 As Integer
Dim l0114 As Integer
  If  (pv010A < pv010C) Then
    l0112 = pv010A
    l0114 = pv010C
  Else
    l0112 = pv010C
    l0114 = pv010A
  End If
  fn0240 = -1
  For l0110 = 0 To mc00D4
    If  (m00D8(l0110).M054A) Then
      If  (m00D8(l0110).M0543 >= l0112 And m00D8(l0110).M0543 <= l0114) Then
        If  (pv010E > 0) Then
          If  (m00D8(l0110).M060B - 30 <= pv010E And m00D8(l0110).M060B + 30 >= pv010E) Then
            fn0240 = l0110
            Exit For
          End If
        Else
          fn0240 = l0110
          Exit For
        End If
      End If
    End If
  Next l0110
End Function

Sub sub0259 ()
Dim l0116
Dim l0118
Dim l011A As Integer
Dim l011C As Integer
Dim l011E As Integer
' Const mc0120 = 1 ' &H1%
    l011A = fn0240(m006C, (m006C + (m0070 * 5)), m006E)
    If  (l011A > -1) Then
      If  (m00D8(l011A).M0553 = mc0120) Then
        If  (m0054 = mc005A) Then
          m0052 = m0052 + 1
          m004E = m004E + m005C
        End If
        If  (l011A = mc0102 Or l011A = mc0104 Or l011A = mc00FA Or l011A = mc0106) Then
          l011C = m00D0
          l011E = 12
        Else
          l011C = m00CE
          l011E = 9
        End If
        gv01C6 = gv01C6 + m00D8(l011A).M0627
        lblPunkte.Caption = Str$(gv01C6)
        If  (m00D8(l011A).M0627 > 0) Then
          If  (l011A = mc0102 Or l011A = mc0104) Then
            Call sub0100("GUMMI.WAV")
          Else
            Call sub0100("P" & Trim(Str$(m00D8(l011A).M0627)) & ".WAV")
          End If
        End If
        For l0116 = 0 To l011E
          For l0118 = 0 To mc00D4
            If  (m00D8(l0118).M05F1 = True) Then
              m001C = extfn01A1(m00D8(l0118).M0568, m00D8(l0118).M0573, m00D8(l0118).M0581, m00D8(l0118).M058F, 0)
            End If
          Next l0118
          If  (m0054 = mc005A) Then
            m001C = extfn01A1(m00CC, m0022(m004E), m0038(m004E) + m00AC(m0052), m0052, 1)
          Else
            m001C = extfn01A1(m00CA, m0022(m004E) + (m0080(m0052) * m005C), m0038(m004E), m0052, 1)
          End If
          m001C = extfn01A1(l011C, m00D8(l011A).M0573 + m00D8(l011A).M0617, m00D8(l011A).M060B - 30, l0116, 0)
          m001C = extfn01B5(picScreen.hWnd)
        Next l0116
        m006A = 0
        m0062 = 0
        If  (l011A = m00EE) Then
          m00EE = -1
          timerWaffeFire.Enabled = False
        End If
        m00D8(l011A).M054A = False
        m00D8(l011A).M05F1 = False
        m00D8(l011A).M05DC = False
      End If
      If  (m00D8(l011A).M05FA > -1) Then
        m00D8(m00D8(l011A).M05FA).M05F1 = True
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
Dim l0142
Dim l0144
Dim l0146 As Integer
Static m0148 As Integer
' Const mc014A = 4 ' &H4%
' Const mc014C = 6 ' &H6%
    l0146 = fn0240(m0050, m004E, 0)
    If  (l0146 > -1) Then
      If  (m00D8(l0146).M055E = m0148) Then
        m004E = m0050
        m0054 = m0056
      ElseIf (m00D8(l0146).M055E = mc0120) Then
        Call sub0100("TOD1.WAV")
        For l0142 = 0 To 12
          For l0144 = 0 To mc00D4
            If  (m00D8(l0144).M05F1 = True) Then
              m001C = extfn01A1(m00D8(l0144).M0568, m00D8(l0144).M0573, m00D8(l0144).M0581, m00D8(l0144).M058F, 0)
            End If
          Next l0144
          If  (l0142 < 6) Then
            m001C = extfn01A1(m00CA, m0022(m004E) + (m0080(m0052) * m005C), m0038(m004E), m0052, 1)
          End If
          m001C = extfn01A1(m00D0, m0022(m004E) - 30, m0038(m004E) - 30, l0142, 0)
          m001C = extfn01B5(picScreen.hWnd)
        Next l0142
        m00D8(l0146).M054A = False
        m00D8(l0146).M05F1 = False
        Call sub0277
      ElseIf (m00D8(l0146).M055E = mc014A) Then
        Call sub0100("CHORAL.WAV")
        For l0142 = 0 To 2
          m00D8(l0146).M058F = l0144
          For l0144 = 0 To mc00D4
            If  (m00D8(l0144).M05F1 = True) Then
              m001C = extfn01A1(m00D8(l0144).M0568, m00D8(l0144).M0573, m00D8(l0144).M0581, m00D8(l0144).M058F, 0)
            End If
          Next l0144
          m001C = extfn01A1(m00CA, m0022(m004E) + (m0080(m0052) * m005C), m0038(m004E), m0052, 1)
          m001C = extfn01B5(picScreen.hWnd)
        Next l0142
        gv01BE = gv01BE + 1
        m00D8(l0146).M054A = False
        m00D8(l0146).M05F1 = False
      ElseIf (m00D8(l0146).M055E = mc014C) Then
        If  (m0054 = mc005A And m0052 > 2 And m0052 < 5) Then
          Call sub0100("CHORAL.WAV")
          gv01CE = True
          m00D8(l0146).M054A = False
          m00D8(l0146).M05F1 = False
        End If
      End If
    End If
End Sub


Sub sub03E6 (p0150 As Integer)
Dim l0152
Dim l0154
  If  (m00D8(p0150).M054A) Then
    If  (m00D8(p0150).M0543 >= m004E And m00D8(p0150).M0543 - 4 <= m004E) Then
      Call sub0100("TOD1.WAV")
      For l0152 = 0 To 12
        For l0154 = 0 To mc00D4
          If  (m00D8(l0154).M05F1 = True) Then
            m001C = extfn01A1(m00D8(l0154).M0568, m00D8(l0154).M0573, m00D8(l0154).M0581, m00D8(l0154).M058F, 0)
          End If
        Next l0154
        If  (l0152 < 6) Then
          m001C = extfn01A1(m00CA, m0022(m004E) + (m0080(m0052) * m005C), m0038(m004E), m0052, 1)
        End If
        m001C = extfn01A1(m00D0, m0022(m004E) - 30, m0038(m004E) - 30, l0152, 0)
        m001C = extfn01B5(picScreen.hWnd)
      Next l0152
      m00D8(p0150).M054A = False
      m00D8(p0150).M05F1 = False
      Call sub0277
    End If
  End If
End Sub

Sub sub03FC ()
Dim l0156
Dim l0158
    If  (m007A > 88 And m007A < 174) Then
      If  (m007A >= m004E And (m007A - 3) <= m004E) Then
        If  (m0078 > 4 Or m0054 <> mc005A) Then
          Call sub0100("TOD1.WAV")
          For l0156 = 0 To 12
            For l0158 = 0 To mc00D4
              If  (m00D8(l0158).M05F1 = True) Then
                m001C = extfn01A1(m00D8(l0158).M0568, m00D8(l0158).M0573, m00D8(l0158).M0581, m00D8(l0158).M058F, 0)
              End If
            Next l0158
            If  (m00EE > -1) Then
              m001C = extfn01A1(m00D8(m00EE).M0568, m00D8(m00EE).M0573, m00D8(m00EE).M0581, m00D8(m00EE).M058F, 0)
            End If
            If  (l0156 < 6) Then
              m001C = extfn01A1(m00CA, m0022(m004E) + (m0080(m0052) * m005C), m0038(m004E), m0052, 1)
            End If
            m001C = extfn01A1(m00D0, m0022(m004E) - 30, m0038(m004E) - 30, l0156, 0)
            m001C = extfn01B5(picScreen.hWnd)
          Next l0156
          m0078 = 0
          Call sub0277
        End If
      ElseIf (m0078 = 1) Then
        Call sub0100("BOOOM1.WAV")
        m001C = extfn01A1(m00CE, m0022(m007A), m007C - 25, 3, 0)
        m0078 = 0
      Else
        m007A = m007A + (3 * m007E)
        m007C = m007C + ((11 - m0078) * 2)
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
        m0068 = m0066
      Else
        m006C = m0050 + 1
        m0068 = m0064
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
  Call sub0204(frmScreen2)
  frmScreen2.Move 0, 0, Screen.Width, Screen.Height
  picScreen.Left = (frmScreen2.Width - picScreen.ScaleWidth) / 2
  picScreen.Top = (frmScreen2.Height - picScreen.ScaleHeight) / 2
  timerMain.Interval = gv01D0
  sub040F
End Sub

Sub Form_Unload (Cancel As Integer)
  timerMain.Enabled = False
  timerWaffeFire.Enabled = False
End Sub

Sub sub040F ()
Dim l0178
' Const mc017C = 2 ' &H2%
  lblPunkte.Caption = Str$(gv01C6)
  m001C = extfn0156(picScreen.Picture)
  If  (m001C = 0) Then
    End
  End If
  m00D8(0).M0543 = 76
  m00D8(0).M054A = True
  m00D8(0).M0553 = mc0120
  m00D8(0).M055E = mc017C
  m00D8(0).M0568 = extfn01C7(imgKopf3.Picture)
  m00D8(0).M0573 = 33
  m00D8(0).M0581 = 348
  m00D8(0).M058F = 0
  m00D8(0).M059F = 0
  m00D8(0).M05F1 = True
  m00D8(0).M05FA = -1
  m00D8(0).M060B = 407
  m00D8(0).M0617 = 0
  m00D8(0).M0627 = 10000
  m00D8(1).M0543 = 185
  m00D8(1).M054A = True
  m00D8(1).M0553 = mc0120
  m00D8(1).M055E = mc017C
  m00D8(1).M0568 = extfn01C7(imgKopf4.Picture)
  m00D8(1).M0573 = 532
  m00D8(1).M0581 = 21
  m00D8(1).M058F = 0
  m00D8(1).M059F = 0
  m00D8(1).M05F1 = True
  m00D8(1).M05FA = -1
  m00D8(1).M060B = 82
  m00D8(1).M0617 = 3
  m00D8(1).M0627 = 10000
  m00D8(2).M0543 = 80
  m00D8(2).M054A = False
  m00D8(2).M0553 = mc0120
  m00D8(2).M055E = mc0120
  m00D8(2).M0568 = extfn017B(imgSchabe.Picture, &HFF0000&, 153, 85)
  m00D8(2).M0573 = 0
  m00D8(2).M0581 = 380
  m00D8(2).M058F = 0
  m00D8(2).M059F = 3
  m00D8(2).M05B2 = 80
  m00D8(2).M05BE = 0
  m00D8(2).M05C8 = -1
  m00D8(2).M05DC = True
  m00D8(2).M05F1 = False
  m00D8(2).M05FA = -1
  m00D8(2).M060B = 400
  m00D8(2).M0617 = 44
  m00D8(2).M0627 = 5000
  m00D8(3).M0543 = 20
  m00D8(3).M054A = True
  m00D8(3).M0553 = mc017C
  m00D8(3).M055E = mc014C
  m00D8(3).M0568 = extfn01C7(imgTauch.Picture)
  m00D8(3).M0573 = 481
  m00D8(3).M0581 = 353
  m00D8(3).M058F = 0
  m00D8(3).M059F = 0
  m00D8(3).M05F1 = True
  m00D8(3).M05FA = -1
  m00D8(3).M060B = 0
  m00D8(3).M0617 = 0
  m00D8(3).M0627 = 0
  m00D8(4).M0543 = 159
  m00D8(4).M054A = False
  m00D8(4).M0553 = mc0120
  m00D8(4).M055E = mc017C
  m00D8(4).M0568 = extfn017B(imgWaffe.Picture, &HFF0000&, 88, 33)
  m00D8(4).M0573 = 0
  m00D8(4).M0581 = 180
  m00D8(4).M058F = 0
  m00D8(4).M059F = 1
  m00D8(4).M05B2 = 160
  m00D8(4).M05BE = 100
  m00D8(4).M05C8 = -1
  m00D8(4).M05DC = True
  m00D8(4).M05F1 = True
  m00D8(4).M05FA = -1
  m00D8(4).M060B = 200
  m00D8(4).M0617 = 44
  m00D8(4).M0627 = 5000
  m00F0 = 0
  m00D8(5).M0543 = 150
  m00D8(5).M054A = True
  m00D8(5).M0553 = mc017C
  m00D8(5).M055E = mc014A
  m00D8(5).M0568 = extfn017B(imgBoot.Picture, &HFF0000&, 116, 30)
  m00D8(5).M0573 = 125
  m00D8(5).M0581 = 276
  m00D8(5).M058F = 0
  m00D8(5).M059F = 2
  m00D8(5).M05F1 = True
  m00D8(5).M05FA = -1
  m00D8(5).M060B = 0
  m00D8(5).M0617 = 0
  m00D8(5).M0627 = 0
  m00D8(6).M0543 = 250
  m00D8(6).M054A = True
  m00D8(6).M0553 = mc0120
  m00D8(6).M055E = mc0120
  m00D8(6).M0568 = extfn0169(imgPuppe1.Picture, &HFF0000&)
  m00D8(6).M0573 = 24
  m00D8(6).M0581 = 6
  m00D8(6).M058F = 0
  m00D8(6).M059F = 0
  m00D8(6).M05F1 = True
  m00D8(6).M05FA = -1
  m00D8(6).M060B = 40
  m00D8(6).M0617 = 0
  m00D8(6).M0627 = 2000
  m00D8(7).M0543 = 190
  m00D8(7).M054A = True
  m00D8(7).M0553 = mc0120
  m00D8(7).M055E = mc0120
  m00D8(7).M0568 = extfn0169(imgPuppe2.Picture, &HFF0000&)
  m00D8(7).M0573 = 504
  m00D8(7).M0581 = 16
  m00D8(7).M058F = 0
  m00D8(7).M059F = 0
  m00D8(7).M05F1 = True
  m00D8(7).M05FA = -1
  m00D8(7).M060B = 40
  m00D8(7).M0617 = 0
  m00D8(7).M0627 = 2000
  m00D8(8).M0543 = 80
  m00D8(8).M054A = False
  m00D8(8).M0553 = mc0120
  m00D8(8).M055E = mc0120
  m00D8(8).M0568 = m00D8(mc00FA).M0568
  m00D8(8).M0573 = 0
  m00D8(8).M0581 = 380
  m00D8(8).M058F = 0
  m00D8(8).M059F = 3
  m00D8(8).M05B2 = 80
  m00D8(8).M05BE = 0
  m00D8(8).M05C8 = -1
  m00D8(8).M05DC = True
  m00D8(8).M05F1 = False
  m00D8(8).M05FA = -1
  m00D8(8).M060B = 400
  m00D8(8).M0617 = 44
  m00D8(8).M0627 = 5000
  m00C2 = extfn017B(imgManStepR.Picture, &HFF0000&, 57, 82)
  m00C4 = extfn017B(imgManJumpR.Picture, &HFF0000&, 61, 91)
  m00C6 = extfn017B(imgManStepL.Picture, &HFF0000&, 57, 82)
  m00C8 = extfn017B(imgManJumpL.Picture, &HFF0000&, 61, 91)
  m0066 = extfn0169(imgFireR.Picture, &HFF0000&)
  m0064 = extfn0169(imgFireL.Picture, &HFF0000&)
  m00CE = extfn017B(imgExplosionKlein.Picture, &HFF0000&, 59, 65)
  m00D0 = extfn017B(imgExplosionGross.Picture, &HFF0000&, 116, 107)
  m0074 = extfn017B(imgPfeilL.Picture, &HFF0000&, 45, 11)
  m0072 = extfn017B(imgPfeilR.Picture, &HFF0000&, 45, 11)
  m0076 = m0074
  m00D2 = extfn017B(imgLeben.Picture, &HFF0000&, 57, 21)
  For l0178 = 0 To 87
    m0022(l0178) = 640 - l0178 * mc0020
    m0022(l0178 + 88) = 640 - l0178 * mc0020
    m0022(l0178 + 176) = 640 - l0178 * mc0020
  Next l0178
  For l0178 = 0 To 87
    m0038(l0178) = 382
  Next l0178
  For l0178 = 88 To 175
    m0038(l0178) = 225
  Next l0178
  For l0178 = 176 To 263
    m0038(l0178) = 67
  Next l0178
  m0080(0) = 0: m0080(1) = 0: m0080(2) = 0: m0080(3) = 10: m0080(4) = 10
  m0080(5) = 0: m0080(6) = 0: m0080(7) = 0: m0080(8) = 10: m0080(9) = 10
  m00AC(0) = -20: m00AC(1) = -30: m00AC(2) = -40: m00AC(3) = -50
  m00AC(4) = -40: m00AC(5) = -30: m00AC(6) = -20: m00AC(7) = -9
  m004E = 10
  m0052 = 10
  m005C = mc005E
  m0054 = m0056
  m00CA = m00C6
  m00CC = m00C8
  m0062 = 0
  m006A = 0
  m0070 = mc005E
  m0068 = m0064
  m00EE = -1
  lblMunition.Width = gv01C2 * 100
End Sub

Sub sub030C ()
  m001C = extfn01A1(m00CC, m0022(m004E), m0038(m004E) + m00AC(m0052), m0052, m0062)
  m0052 = m0052 + 1
End Sub


Sub sub0317 ()
  m0052 = (m0052 + 1) Mod 10
  m001C = extfn01A1(m00CA, m0022(m004E) + (m0080(m0052) * m005C), m0038(m004E), m0052, m0062)
End Sub

Sub sub0322 ()
      m00D8(m00EE).M0543 = m00D8(m00EE).M0543 + m00D8(m00EE).M05C8
      m00D8(m00EE).M0573 = m0022(m00D8(m00EE).M0543) - m00D8(m00EE).M0617
      m00D8(m00EE).M058F = m00D8(m00EE).M058F + 1
      If  (m00D8(m00EE).M058F > m00D8(m00EE).M059F) Then
        m00D8(m00EE).M058F = 0
      End If
      If  (m00D8(m00EE).M0543 = m00D8(m00EE).M05BE) Then
        m00F0 = m00F0 + 1
        If  (m00F0 = 4) Then
          Call sub010D("WAFFE.WAV")
          m00D8(m00EE).M05C8 = m00D8(m00EE).M05C8 * -1
          m007E = m00D8(m00EE).M05C8
          m00D8(m00EE).M0543 = m00D8(m00EE).M0543 + m00D8(m00EE).M05C8
        Else
          m00D8(m00EE).M0543 = m00D8(m00EE).M0543 - m00D8(m00EE).M05C8
        End If
      End If
      If  (m00D8(m00EE).M0543 = m00D8(m00EE).M05B2 Or m00F2) Then
        m00F0 = m00F0 - 1
        If  (m00F0 = 0) Then
          Call sub010D("WAFFE.WAV")
          m00F2 = False
          m00D8(m00EE).M05C8 = m00D8(m00EE).M05C8 * -1
          m007E = m00D8(m00EE).M05C8
          m00D8(m00EE).M0543 = m00D8(m00EE).M0543 + m00D8(m00EE).M05C8
        Else
          m00D8(m00EE).M0543 = m00D8(m00EE).M0543 - m00D8(m00EE).M05C8
        End If
      End If
      If  (m00F4 = 0) Then
        m00D8(m00EE).M058F = 1
      Else
        m00D8(m00EE).M058F = 0
      End If
      If  (m00D8(m00EE).M0543 <> m00D8(m00EE).M05BE) Then
        m001C = extfn01A1(m00D8(m00EE).M0568, m00D8(m00EE).M0573, m00D8(m00EE).M0581, m00D8(m00EE).M058F, m00F0)
      Else
        m00D8(m00EE).M0543 = m00D8(m00EE).M05B2
        m00D8(m00EE).M054A = False
        m00EE = -1
      End If
End Sub

Sub sub0337 ()
Dim l01FC
' Const mc020C = 3 ' &H3%
    For l01FC = 0 To mc00D4
      If  (m00D8(l01FC).M05F1 = True) Then
        If  (m00D8(l01FC).M05DC And l01FC <> mc00FE) Then
          m00D8(l01FC).M058F = (m00D8(l01FC).M058F + 1) Mod (m00D8(l01FC).M059F + 1)
        End If
        m001C = extfn01A1(m00D8(l01FC).M0568, m00D8(l01FC).M0573, m00D8(l01FC).M0581, m00D8(l01FC).M058F, 0)
      End If
    Next l01FC
    If  (m0054 = m0056) Then
      m001C = extfn01A1(m00CA, m0022(m004E) + (m0080(m0052) * m005C), m0038(m004E), m0052, m0062)
    ElseIf (m0054 = mc0058) Then
      Call sub0317
    ElseIf (m0054 = mc005A) Then
      Call sub030C
    End If
    If  (m006A > 1) Then
      m001C = extfn0192(m0068, m0022(m006C), m006E)
    End If
    If  (m00EE > -1) Then
      Call sub0322
    End If
    If  (m0078 > 0) Then
      m001C = extfn01A1(m0076, m0022(m007A), m007C, m0078 Mod 3, 0)
    End If
    m001C = extfn01A1(m00D2, 583, 459, gv01BE - 1, 0)
    m001C = extfn01B5(picScreen.hWnd)
End Sub


Sub timerMain_Timer ()
  gv01CA = gv01CA + 1
  m00F4 = (m00F4 + 1) Mod 8
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
      m00CA = m00C2
      m00CC = m00C4
    End If
    If  (Asc(Mid(gv00B2, 38, 1)) > 2) Then
      m0054 = mc0058
      m005C = mc005E
      m004E = m004E + 1
      m00CA = m00C6
      m00CC = m00C8
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
      gv01B2 = mc020C
      frmLoading.Show
      Unload Me
      Exit Sub
    End If
    If  (m004E < 0) Then
      m0054 = m0056
      m004E = m0050
    End If
    If  (m004E > 88 And m00D8(mc00FE).M05DC) Then
      If  (m00EE <> mc00FE) Then
        Call sub010D("WAFFE.WAV")
        m00EE = mc00FE
        m00D8(mc00FE).M054A = True
        m00D8(mc00FE).M05F1 = False
        timerWaffeFire.Enabled = True
        m00F2 = False
      Else
        If  (m0050 <= 88) Then
          If  (m00D8(mc00FE).M05C8 = 1) Then
            m00F2 = True
          End If
        End If
      End If
    End If
    If  (m004E > 15 And m00D8(mc00FA).M05DC And m00D8(mc00FA).M05F1 = False) Then
      Call sub0100("SCHABE.WAV")
      m00D8(mc00FA).M054A = True
      m00D8(mc00FA).M05F1 = True
      m00D8(mc00FA).M0543 = m00D8(mc00FA).M05B2
    End If
    m0050 = m004E
  End If
  If  (m00D8(mc00FA).M05F1) Then
    m00D8(mc00FA).M0543 = m00D8(mc00FA).M0543 - 2
    If  (m00D8(mc00FA).M0543 < 0) Then
      m00D8(mc00FA).M054A = False
      m00D8(mc00FA).M05F1 = False
    Else
      m00D8(mc00FA).M0573 = m0022(m00D8(mc00FA).M0543) - 50
      Call sub03E6(mc00FA)
    End If
  End If
  If  (m00D8(mc00FA).M0543 < 55 And m00D8(mc0106).M05DC And m00D8(mc0106).M05F1 = False) Then
    Call sub0100("SCHABE.WAV")
    m00D8(mc0106).M054A = True
    m00D8(mc0106).M05F1 = True
    m00D8(mc0106).M0543 = m00D8(mc0106).M05B2
  End If
  If  (m00D8(mc0106).M05F1) Then
    m00D8(mc0106).M0543 = m00D8(mc0106).M0543 - 3
    If  (m00D8(mc0106).M0543 < 0) Then
      m00D8(mc0106).M054A = False
      m00D8(mc0106).M05F1 = False
    Else
      m00D8(mc0106).M0573 = m0022(m00D8(mc0106).M0543) - 50
      Call sub03E6(mc0106)
    End If
  End If
  If  (m006A > 0) Then
    Call sub0259
  End If
  If  (m0078 > 0) Then
    Call sub03FC
  End If
  m00D8(mc0102).M0573 = m00D8(mc0102).M0573 + m005C
  If  (m00D8(mc0102).M0573 > 530) Then
    m00D8(mc0102).M0573 = 530
  End If
  If  (m00D8(mc0102).M0573 < 1) Then m00D8(mc0102).M0573 = 1
  m00D8(mc0102).M0543 = 254 - (m00D8(mc0102).M0573 / 8)
  m00D8(mc0104).M0573 = m00D8(mc0104).M0573 - (2 * m005C)
  If  (m00D8(mc0104).M0573 > 530) Then
    m00D8(mc0104).M0573 = 530
  End If
  If  (m00D8(mc0104).M0573 < 1) Then m00D8(mc0104).M0573 = 1
  m00D8(mc0104).M0543 = 254 - (m00D8(mc0104).M0573 / 8)
  Call sub0337
End Sub


Sub timerWaffeFire_Timer ()
  If  (m0078 = 0 And (m00F0 = 0 Or m00F0 = 4)) Then
    m007C = m00D8(mc00FE).M0581 + 17
    m007E = m00D8(m00EE).M05C8
    m007A = m00D8(mc00FE).M0543 + 3 + (7 * m007E)
    If  (m007E = mc0060) Then
      m0076 = m0072
    Else
      m0076 = m0074
    End If
    m0078 = 11
  End If
End Sub
