' SCREEN1.FRM
Option Explicit
Dim m0020 As Integer
Const mc0022 = 264 ' &H108%
Const mc0024 = 8 ' &H8%
Dim m0026(264) As Integer
Dim m003C(264) As Integer
Dim m0052 As Integer
Dim m0054 As Integer
Dim m0056 As Integer
Dim m0058 As Integer
Dim m005A As Integer
Const mc005C = 1 ' &H1%
Const mc005E = 2 ' &H2%
Dim m0060 As Integer
Const mc0062 = 1 ' &H1%
Const mc0064 =-1 ' &HFFFF%
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
Dim m00D4 As Integer
Const mc00D6 = 13 ' &HD%
Dim m00DA(13) As T0534
Dim m00F0 As Integer
Dim m00F2 As Integer
Dim m00F4 As Integer
Dim m00F6 As Integer
Dim m00F8 As Integer
Dim m00FA As Integer
Dim m00FC As Integer
Const mc0100 = 2 ' &H2%
Const mc0102 = 3 ' &H3%
Const mc0104 = 4 ' &H4%
Const mc0106 = 5 ' &H5%
Const mc0108 = 6 ' &H6%
Const mc010A = 7 ' &H7%
Const mc010C = 8 ' &H8%
Const mc010E = 9 ' &H9%
Const mc0110 = 10 ' &HA%
Const mc0112 = 11 ' &HB%
Const mc0114 = 12 ' &HC%
Const mc0116 = 13 ' &HD%
Const mc0130 = 1 ' &H1%
Const mc014A = 3 ' &H3%
Const mc014C = 5 ' &H5%
Const mc014E = 2 ' &H2%
Const m0166 = 0 ' &H0%
Const mc0168 = 4 ' &H4%
Const mc024A = 2 ' &H2%

Function fn0240 (ByVal pv011A As Integer, ByVal pv011C As Integer, ByVal pv011E As Integer) As Integer
Dim l0120
Dim l0122 As Integer
Dim l0124 As Integer
  If  (pv011A < pv011C) Then
    l0122 = pv011A
    l0124 = pv011C
  Else
    l0122 = pv011C
    l0124 = pv011A
  End If
  fn0240 = -1
  For l0120 = 0 To mc00D6
    If  (m00DA(l0120).M054A) Then
      If  (m00DA(l0120).M0543 >= l0122 And m00DA(l0120).M0543 <= l0124) Then
        If  (pv011E > 0) Then
          If  (m00DA(l0120).M060B - 30 <= pv011E And m00DA(l0120).M060B + 30 >= pv011E) Then
            fn0240 = l0120
          End If
        Else
          fn0240 = l0120
        End If
      End If
    End If
  Next l0120
End Function

Sub sub0259 ()
Dim l0126
Dim l0128
Dim l012A As Integer
Dim l012C As Integer
Dim l012E As Integer
' Const mc0130 = 1 ' &H1%
' Const mc014A = 3 ' &H3%
' Const mc014C = 5 ' &H5%
' Const mc014E = 2 ' &H2%
    l012A = fn0240(m0070, (m0070 + (m0074 * 5)), m0072)
    If  (l012A > -1) Then
      If  (m00DA(l012A).M0553 = mc0130) Then
        If  (m0058 = mc005E) Then
          m0056 = m0056 + 1
          m0052 = m0052 + m0060
        End If
        If  (l012A = mc0106) Then
          l012C = m00D0
          l012E = 12
        Else
          l012C = m00CE
          l012E = 9
        End If
        gv01C6 = gv01C6 + m00DA(l012A).M0627
        lblPunkte.Caption = Str$(gv01C6)
        If  (m00DA(l012A).M0627 > 0) Then
          Call sub0100("P" & Trim(Str$(m00DA(l012A).M0627)) & ".WAV")
        End If
        For l0126 = 0 To l012E
          For l0128 = 0 To mc00D6
            If  (m00DA(l0128).M05F1 = True) Then
              m0020 = extfn01A1(m00DA(l0128).M0568, m00DA(l0128).M0573, m00DA(l0128).M0581, m00DA(l0128).M058F, 0)
            End If
          Next l0128
          If  (m0058 = mc005E) Then
            m0020 = extfn01A1(m00CC, m0026(m0052), m003C(m0052) + m00AC(m0056), m0056, 1)
          Else
            m0020 = extfn01A1(m00CA, m0026(m0052) + (m0080(m0056) * m0060), m003C(m0052), m0056, 1)
          End If
          m0020 = extfn01A1(l012C, m00DA(l012A).M0573 + m00DA(l012A).M0617, m00DA(l012A).M060B - 30, l0126, 0)
          m0020 = extfn01B5(picScreen.hWnd)
        Next l0126
        m006E = 0
        m0066 = 0
        If  (l012A = mc0100) Then
          timerHeliFire.Enabled = False
          m00F0 = -1
          If  (m00DA(mc0116).M05DC) Then
            m00F0 = mc0116
            m00DA(mc0116).M054A = True
            timerHeliFire.Enabled = True
          End If
        End If
        If  (l012A = mc0116) Then
          timerHeliFire.Enabled = False
          m00F0 = -1
        End If
        m00DA(l012A).M054A = False
        m00DA(l012A).M05F1 = False
        m00DA(l012A).M05DC = False
      ElseIf (m00DA(l012A).M0553 = mc014A) Then
        m00DA(l012A).M05F1 = True
        m00DA(l012A).M054A = False
        m006E = 0
        m0066 = 0
        If  (l012A = mc0102) Then m00F4 = 1
        If  (l012A = mc0104) Then m00F4 = 0
      ElseIf (m00DA(l012A).M0553 = mc014C) Then
        If  (m0058 = mc005E) Then
          m0056 = m0056 + 1
          m0052 = m0052 + m0060
        End If
        For l0126 = 0 To 9
          m00DA(l012A).M0581 = m00DA(l012A).M0581 + 10
          For l0128 = 0 To mc00D6
            If  (m00DA(l0128).M05F1 = True) Then
              m0020 = extfn01A1(m00DA(l0128).M0568, m00DA(l0128).M0573, m00DA(l0128).M0581, m00DA(l0128).M058F, 0)
            End If
          Next l0128
          If  (m0058 = mc005E) Then
            m0020 = extfn01A1(m00CC, m0026(m0052), m003C(m0052) + m00AC(m0056), m0056, 1)
          Else
            m0020 = extfn01A1(m00CA, m0026(m0052) + (m0080(m0056) * m0060), m003C(m0052), m0056, 1)
          End If
          m0020 = extfn01B5(picScreen.hWnd)
        Next l0126
        m006E = 0
        m0066 = 0
        m00DA(l012A).M054A = False
        m00DA(l012A).M05F1 = False
      ElseIf (m00DA(l012A).M0553 = mc014E) Then
        If  (m006E > 6) Then
          m0066 = m0066 - 1
        End If
        m006E = m006E - 1
        m0070 = m0070 + (m0074 * 4)
        If  (m0070 >= mc0022 Or m0070 < 0) Then
          m006E = 0
          m0066 = 0
        End If
      End If
      If  (m00DA(l012A).M05FA > -1) Then
        m00DA(m00DA(l012A).M05FA).M05F1 = True
        If  (m00DA(l012A).M05FA = mc010C) Then
          m00DA(mc010C).M0553 = mc014C
          m00DA(mc010C).M05FA = mc0108
          m00DA(mc010E).M05F1 = True
          m00DA(mc0104).M054A = True
          m00DA(mc0108).M05F1 = True
          m00DA(mc010A).M05F1 = True
          m00DA(m00FC).M054A = True
        End If
        If  (m00DA(l012A).M05FA = mc0108) Then
          m00DA(mc010A).M05F1 = False
          m00DA(mc010E).M05F1 = False
        End If
        If  (m00DA(l012A).M05FA = m00FC) Then
          m00DA(mc0110).M05F1 = True
          m00DA(mc0110).M054A = True
          m00DA(mc0112).M05F1 = True
        End If
        If  (m00DA(l012A).M05FA = mc0114) Then
          m00DA(mc0102).M055E = mc014C
          m00DA(mc0102).M0553 = mc014E
          m00DA(mc0102).M05FA = -1
          m00DA(mc0102).M054A = True
          m00DA(mc0108).M054A = True
        End If
      End If
    Else
      If  (m006E > 6) Then
        m0066 = m0066 - 1
      End If
      m006E = m006E - 1
      m0070 = m0070 + (m0074 * 4)
      If  (m0070 >= mc0022 Or m0070 < 0) Then
        m006E = 0
        m0066 = 0
      End If
    End If
End Sub

Sub sub0266 ()
Dim l0150
Dim l0152
Dim l0154 As Integer
Dim l0156 As Integer
    If  (m007A > 175 And m007A < 260) Then
      If  (m007E) Then
        l0154 = m007A - 3
        l0156 = m007A
      Else
        l0154 = m007A
        l0156 = m007A + 3
      End If
      If  (l0156 >= m0052 And l0154 <= m0052) Then
        If  (m0078 > 4 Or m0058 <> mc005E) Then
          Call sub0100("TOD1.WAV")
          For l0150 = 0 To 12
            For l0152 = 0 To mc00D6
              If  (m00DA(l0152).M05F1 = True) Then
                m0020 = extfn01A1(m00DA(l0152).M0568, m00DA(l0152).M0573, m00DA(l0152).M0581, m00DA(l0152).M058F, 0)
              End If
            Next l0152
            If  (m00F0 > -1) Then
              m0020 = extfn01A1(m00DA(m00F0).M0568, m00DA(m00F0).M0573, m00DA(m00F0).M0581, m00DA(m00F0).M058F, 0)
            End If
            If  (l0150 < 6) Then
              m0020 = extfn01A1(m00CA, m0026(m0052) + (m0080(m0056) * m0060), m003C(m0052), m0056, 1)
            End If
            m0020 = extfn01A1(m00D0, m0026(m0052) - 30, m003C(m0052) - 30, l0150, 0)
            m0020 = extfn01B5(picScreen.hWnd)
          Next l0150
          m0078 = 0
          Call sub0277
        End If
      ElseIf (m0078 = 1) Then
         Call sub0100("BOOOM1.WAV")
         m0020 = extfn01A1(m00CE, m0026(m007A), m007C - 25, 3, 0)
         m0078 = 0
      Else
        m007A = m007A + (3 * m007E)
        m007C = m007C + 7
        m0078 = m0078 - 1
      End If
    Else
      m0078 = 0
    End If
End Sub

Sub sub0277 ()
  gv01BE = gv01BE - 1
  m0052 = 95
  If  (gv01D2) Then gv01BE = 1
  If  (gv01BE = 0) Then
    frmGameOver.Show
    Unload Me
    Exit Sub
  End If
End Sub

Sub sub0285 ()
Dim l0160
Dim l0162
Dim l0164 As Integer
' Const m0166 = 0 ' &H0%
' Const mc0168 = 4 ' &H4%
    l0164 = fn0240(m0054, m0052, 0)
    If  (l0164 > -1) Then
      If  (m00DA(l0164).M055E = m0166) Then
        m0052 = m0054
        m0058 = m005A
      End If
      If  (m00DA(l0164).M055E = mc0130) Then
        Call sub0100("TOD1.WAV")
        For l0160 = 0 To 12
          For l0162 = 0 To mc00D6
            If  (m00DA(l0162).M05F1 = True) Then
              m0020 = extfn01A1(m00DA(l0162).M0568, m00DA(l0162).M0573, m00DA(l0162).M0581, m00DA(l0162).M058F, 0)
            End If
          Next l0162
          m0020 = extfn01A1(m00D0, m0026(m0052) - 30, m003C(m0052) - 30, l0160, 0)
          m0020 = extfn01B5(picScreen.hWnd)
        Next l0160
        m00DA(l0164).M054A = False
        m00DA(l0164).M05F1 = False
        m00DA(l0164).M0543 = m00DA(l0164).M05B2
        Call sub0277
      End If
      If  (m00DA(l0164).M055E = mc014C) Then
        If  (l0164 = mc0102) Then
          If  (m00DA(mc0110).M05F1 = True) Then
            l0164 = mc0110
            For l0160 = 0 To 5
              m00DA(l0164).M0581 = m00DA(l0164).M0581 + 40
              For l0162 = 0 To mc00D6
                If  (m00DA(l0162).M05F1 = True) Then
                  m0020 = extfn01A1(m00DA(l0162).M0568, m00DA(l0162).M0573, m00DA(l0162).M0581, m00DA(l0162).M058F, 0)
                End If
              Next l0162
              m0020 = extfn01A1(m00CA, m0026(m0052) + (m0080(m0056) * m0060), m003C(m0052), m0056, 0)
              m0020 = extfn01B5(picScreen.hWnd)
            Next l0160
            m00DA(mc0110).M05F1 = False
            m00DA(mc0110).M054A = False
            m00DA(mc0112).M05F1 = False
            m00DA(mc0102).M054A = False
          End If
        Else
          For l0160 = 0 To 5
            m00DA(l0164).M0581 = m00DA(l0164).M0581 + 10
            For l0162 = 0 To mc00D6
              If  (m00DA(l0162).M05F1 = True) Then
                m0020 = extfn01A1(m00DA(l0162).M0568, m00DA(l0162).M0573, m00DA(l0162).M0581, m00DA(l0162).M058F, 0)
              End If
            Next l0162
            m0020 = extfn01A1(m00CA, m0026(m0052) + (m0080(m0056) * m0060), m003C(m0052), m0056, 0)
            m0020 = extfn01B5(picScreen.hWnd)
          Next l0160
          Call sub0100("TOD1.WAV")
          For l0160 = 0 To 12
            m00DA(l0164).M0581 = m00DA(l0164).M0581 + 10
            For l0162 = 0 To mc00D6
              If  (m00DA(l0162).M05F1 = True) Then
                m0020 = extfn01A1(m00DA(l0162).M0568, m00DA(l0162).M0573, m00DA(l0162).M0581, m00DA(l0162).M058F, 0)
              End If
            Next l0162
            m0020 = extfn01A1(m00D0, m0026(m0052) - 30, m003C(m0052) - 30, l0160, 0)
            m0020 = extfn01B5(picScreen.hWnd)
          Next l0160
          m00DA(mc0110).M05F1 = False
          m00DA(mc0110).M054A = False
          m00DA(mc0112).M05F1 = False
          m00DA(mc0102).M054A = False
          Call sub0277
        End If
      End If
      If  (m00DA(l0164).M055E = mc0168) Then
        Call sub0100("CHORAL.WAV")
        gv01BE = gv01BE + 1
        m00DA(l0164).M054A = False
        m00DA(l0164).M05F1 = False
      End If
    End If
End Sub

Function fn029A (p016C As Integer) As Integer
  fn029A = False
  If  (m0060 = mc0062) Then
      If  (p016C >= 102 And p016C <= 164) Then
        If  (m00DA(mc0114).M05F1 = True) Then
          If  ((p016C >= 102 And p016C <= 109) Or (p016C >= 121 And p016C <= 124) Or (p016C >= 133 And p016C <= 136) Or (p016C >= 146 And p016C <= 152) Or (p016C >= 163 And p016C <= 164)) Then
            fn029A = True
          End If
        Else
          fn029A = True
        End If
      End If
  Else
      If  (p016C >= 100 And p016C <= 161) Then
        If  (m00DA(mc0114).M05F1 = True) Then
          If  ((p016C >= 101 And p016C <= 107) Or (p016C >= 118 And p016C <= 121) Or (p016C >= 131 And p016C <= 133) Or (p016C >= 143 And p016C <= 148) Or (p016C >= 160 And p016C <= 161)) Then
            fn029A = True
          End If
        Else
          fn029A = True
        End If
      End If
  End If
End Function

Function fn02AD (p0170 As Integer) As Integer
  fn02AD = False
  If  (m0058 = mc005C And (m0056 = 0 Or m0056 = 1 Or m0056 = 5 Or m0056 = 6)) Then
    If  (m0060 = mc0062) Then
      If  (p0170 >= 102 And p0170 <= 164) Then
        If  (m00DA(mc0114).M05F1 = True) Then
          If  ((p0170 >= 102 And p0170 <= 109) Or (p0170 >= 121 And p0170 <= 124) Or (p0170 >= 133 And p0170 <= 136) Or (p0170 >= 146 And p0170 <= 152)) Then
            fn02AD = True
          End If
        Else
          fn02AD = True
        End If
      End If
    Else
      If  (p0170 >= 100 And p0170 <= 161) Then
        If  (m00DA(mc0114).M05F1 = True) Then
          If  ((p0170 >= 101 And p0170 <= 107) Or (p0170 >= 118 And p0170 <= 121) Or (p0170 >= 131 And p0170 <= 133) Or (p0170 >= 143 And p0170 <= 148)) Then
            fn02AD = True
          End If
        Else
          fn02AD = True
        End If
      End If
    End If
  End If
End Function


Sub Form_Activate ()
  Call sub0100("ENTER.WAV")
End Sub

Sub Form_KeyDown (KeyCode As Integer, Shift As Integer)
  If  (KeyCode = 32) Then
    If  (gv01C2 > 0 And m006E = 0) Then
      m0066 = 2
      m006E = 8
      m0074 = m0060
      If  (m0074 = mc0064) Then
        m0070 = m0054 - 4
        m006C = m006A
      Else
        m0070 = m0054 + 1
        m006C = m0068
      End If
      If  (m0070 >= mc0022 Or m0070 < 0) Then
        m006E = 0
        m0066 = 0
      Else
        m0072 = m003C(m0054) + 31
        If  (m0058 = mc005E) Then m0072 = m0072 + m00AC(m0056 + 1)
        Call sub0100("FIRE.WAV")
        gv01C2 = gv01C2 - 1
        lblMunition.Width = gv01C2 * 100
      End If
    End If
  End If
End Sub

Sub Form_Load ()
  Call sub0204(frmScreen1)
  frmScreen1.Move 0, 0, Screen.Width, Screen.Height
  picScreen.Left = (frmScreen1.Width - picScreen.ScaleWidth) / 2
  picScreen.Top = (frmScreen1.Height - picScreen.ScaleHeight) / 2
  timerMain.Interval = gv01D0
  sub02FD
End Sub


Sub Form_Unload (Cancel As Integer)
  timerHeliFire.Enabled = False
  timerMain.Enabled = False
  timerSterben.Enabled = False
End Sub

Sub sub02FD ()
Dim l0196
  lblPunkte.Caption = Str$(gv01C6)
  m0020 = extfn0156(picScreen.Picture)
  If  (m0020 = 0) Then
    End
  End If
  m00DA(0).M0543 = 1
  m00DA(0).M054A = True
  m00DA(0).M0553 = mc0130
  m00DA(0).M055E = mc014E
  m00DA(0).M0568 = extfn01C7(imgKopf1.Picture)
  m00DA(0).M0573 = 574
  m00DA(0).M0581 = 366
  m00DA(0).M058F = 0
  m00DA(0).M059F = 0
  m00DA(0).M05F1 = False
  m00DA(0).M05FA = mc0114
  m00DA(0).M060B = 407
  m00DA(0).M0617 = 0
  m00DA(0).M0627 = 10000
  m00DA(1).M0543 = 253
  m00DA(1).M054A = True
  m00DA(1).M0553 = mc0130
  m00DA(1).M055E = m0166
  m00DA(1).M0568 = extfn01C7(imgKopf2.Picture)
  m00DA(1).M0573 = 25
  m00DA(1).M0581 = 38
  m00DA(1).M058F = 0
  m00DA(1).M059F = 0
  m00DA(1).M05F1 = True
  m00DA(1).M05FA = -1
  m00DA(1).M060B = 82
  m00DA(1).M0617 = 3
  m00DA(1).M0627 = 10000
  m00DA(2).M0543 = 263
  m00DA(2).M054A = False
  m00DA(2).M0553 = mc0130
  m00DA(2).M055E = mc014E
  m00DA(2).M0568 = extfn017B(imgHeli.Picture, &HFF0000&, 147, 66)
  m00DA(2).M0573 = 0
  m00DA(2).M0581 = 20
  m00DA(2).M058F = 0
  m00DA(2).M059F = 1
  m00DA(2).M05B2 = 263
  m00DA(2).M05BE = 175
  m00DA(2).M05C8 = -1
  m00DA(2).M05DC = True
  m00DA(2).M05F1 = False
  m00DA(2).M05FA = -1
  m00DA(2).M060B = 40
  m00DA(2).M0617 = 44
  m00DA(2).M0627 = 5000
  m00DA(3).M0543 = 60
  m00DA(3).M054A = True
  m00DA(3).M0553 = mc014A
  m00DA(3).M055E = mc014E
  m00DA(3).M0568 = extfn017B(imgFackel.Picture, &HFF0000&, 12, 33)
  m00DA(3).M0573 = 150
  m00DA(3).M0581 = 393
  m00DA(3).M058F = 0
  m00DA(3).M059F = 2
  m00DA(3).M05B2 = 0
  m00DA(3).M05BE = 0
  m00DA(3).M05C8 = 0
  m00DA(3).M05DC = True
  m00DA(3).M05F1 = False
  m00DA(3).M05FA = mc010C
  m00DA(3).M060B = 400
  m00DA(3).M0627 = 0
  m00DA(4).M0543 = 23
  m00DA(4).M054A = False
  m00DA(4).M0553 = mc014A
  m00DA(4).M055E = mc014E
  m00DA(4).M0568 = m00DA(3).M0568
  m00DA(4).M0573 = 453
  m00DA(4).M0581 = 393
  m00DA(4).M058F = 0
  m00DA(4).M059F = 2
  m00DA(4).M05B2 = 0
  m00DA(4).M05BE = 0
  m00DA(4).M05C8 = 0
  m00DA(4).M05DC = True
  m00DA(4).M05F1 = False
  m00DA(4).M05FA = m00FC
  m00DA(4).M060B = 400
  m00DA(4).M0627 = 0
  m00DA(12).M0543 = 0
  m00DA(12).M054A = False
  m00DA(12).M0553 = mc014E
  m00DA(12).M055E = mc014E
  m00DA(12).M0568 = extfn017B(imgScholle.Picture, &HFF0000&, 498, 35)
  m00DA(12).M0573 = 71
  m00DA(12).M0581 = 275
  m00DA(12).M058F = 0
  m00DA(12).M059F = 2
  m00DA(12).M05B2 = 0
  m00DA(12).M05BE = 0
  m00DA(12).M05C8 = 0
  m00DA(12).M05DC = True
  m00DA(12).M05F1 = False
  m00DA(12).M05FA = -1
  m00DA(12).M060B = 0
  m00DA(12).M0627 = 0
  m00DA(6).M0543 = 43
  m00DA(6).M054A = False
  m00DA(6).M0553 = mc014E
  m00DA(6).M055E = mc0168
  m00DA(6).M0568 = extfn017B(imgUrne.Picture, &HFF0000&, 22, 25)
  m00DA(6).M0573 = 300
  m00DA(6).M0581 = 439
  m00DA(6).M058F = 0
  m00DA(6).M059F = 0
  m00DA(6).M05F1 = False
  m00DA(6).M05FA = -1
  m00DA(6).M060B = 0
  m00DA(6).M0617 = 0
  m00DA(6).M0627 = 0
  m00DA(7).M0543 = 45
  m00DA(7).M054A = False
  m00DA(7).M0553 = mc014E
  m00DA(7).M055E = m0166
  m00DA(7).M0568 = extfn01C7(imgGrab.Picture)
  m00DA(7).M0573 = 280
  m00DA(7).M0581 = 405
  m00DA(7).M058F = 0
  m00DA(7).M059F = 0
  m00DA(7).M05F1 = False
  m00DA(7).M05FA = -1
  m00DA(7).M060B = 0
  m00DA(7).M0617 = 0
  m00DA(7).M0627 = 0
  m00DA(8).M0543 = 44
  m00DA(8).M054A = True
  m00DA(8).M0553 = mc014E
  m00DA(8).M055E = m0166
  m00DA(8).M0568 = extfn017B(imgEiszapfen.Picture, &HFF0000&, 21, 64)
  m00DA(8).M0573 = 306
  m00DA(8).M0581 = 329
  m00DA(8).M058F = 0
  m00DA(8).M059F = 0
  m00DA(8).M05F1 = False
  m00DA(8).M05FA = -1
  m00DA(8).M060B = 367
  m00DA(8).M0617 = -10
  m00DA(8).M0627 = 2000
  m00DA(9).M0543 = 0
  m00DA(9).M054A = False
  m00DA(9).M0553 = mc014E
  m00DA(9).M055E = mc014E
  m00DA(9).M0568 = extfn0169(imgEiszapfenMsk.Picture, &HFF0000&)
  m00DA(9).M0573 = 306
  m00DA(9).M0581 = 329
  m00DA(9).M058F = 0
  m00DA(9).M059F = 0
  m00DA(9).M05F1 = False
  m00DA(9).M05FA = -1
  m00DA(9).M0627 = 0
  m00DA(10).M0543 = 30
  m00DA(10).M054A = False
  m00DA(10).M0553 = mc014E
  m00DA(10).M055E = mc014C
  m00DA(10).M0568 = extfn0169(imgEisfalle.Picture, &HFF0000&)
  m00DA(10).M0573 = 362
  m00DA(10).M0581 = 327
  m00DA(10).M058F = 0
  m00DA(10).M059F = 0
  m00DA(10).M05F1 = False
  m00DA(10).M05FA = -1
  m00DA(10).M0627 = 0
  m00DA(11).M0543 = 0
  m00DA(11).M054A = False
  m00DA(11).M0553 = mc014E
  m00DA(11).M055E = mc014E
  m00DA(11).M0568 = extfn0169(imgEisfalleMsk.Picture, &HFF0000&)
  m00DA(11).M0573 = 362
  m00DA(11).M0581 = 327
  m00DA(11).M058F = 0
  m00DA(11).M059F = 0
  m00DA(11).M05F1 = False
  m00DA(11).M05FA = -1
  m00DA(11).M0627 = 0
  m00DA(5).M0543 = 150
  m00DA(5).M054A = False
  m00DA(5).M0553 = mc0130
  m00DA(5).M055E = mc0130
  m00DA(5).M0568 = extfn017B(imgMonster.Picture, &HFF0000&, 167, 107)
  m00DA(5).M0573 = 0
  m00DA(5).M0581 = 188
  m00DA(5).M058F = 0
  m00DA(5).M059F = 9
  m00DA(5).M05B2 = 160
  m00DA(5).M05BE = 105
  m00DA(5).M05DC = True
  m00DA(5).M05F1 = False
  m00DA(5).M05FA = -1
  m00DA(5).M060B = 232
  m00DA(5).M0617 = 40
  m00DA(5).M0627 = 5000
  m00DA(13).M0543 = 175
  m00DA(13).M054A = False
  m00DA(13).M0553 = mc0130
  m00DA(13).M055E = mc014E
  m00DA(13).M0568 = extfn017B(imgHeli1.Picture, &HFF0000&, 147, 66)
  m00DA(13).M0573 = 0
  m00DA(13).M0581 = 30
  m00DA(13).M058F = 0
  m00DA(13).M059F = 1
  m00DA(13).M05B2 = 175
  m00DA(13).M05BE = 263
  m00DA(13).M05C8 = 1
  m00DA(13).M05DC = True
  m00DA(13).M05F1 = False
  m00DA(13).M05FA = -1
  m00DA(13).M060B = 80
  m00DA(13).M0617 = 44
  m00DA(13).M0627 = 5000
  m00C2 = extfn017B(imgManStepR.Picture, &HFF0000&, 57, 82)
  m00C4 = extfn017B(imgManJumpR.Picture, &HFF0000&, 61, 91)
  m00C6 = extfn017B(imgManStepL.Picture, &HFF0000&, 57, 82)
  m00C8 = extfn017B(imgManJumpL.Picture, &HFF0000&, 61, 91)
  m006A = extfn0169(imgFireR.Picture, &HFF0000&)
  m0068 = extfn0169(imgFireL.Picture, &HFF0000&)
  m0076 = m006A
  m00CE = extfn017B(imgExplosionKlein.Picture, &HFF0000&, 59, 65)
  m00D0 = extfn017B(imgExplosionGross.Picture, &HFF0000&, 116, 107)
  m00F2 = extfn01C7(imgBlack.Picture)
  m00F6 = extfn01C7(imgFade.Picture)
  m00F4 = 2
  m00D2 = extfn017B(imgSterben.Picture, &HFF0000&, 47, 85)
  m00F8 = extfn017B(imgLeben.Picture, &HFF0000&, 57, 21)
  For l0196 = 0 To 87
    m0026(l0196) = 640 - l0196 * mc0024
    m0026(l0196 + 88) = 640 - l0196 * mc0024
    m0026(l0196 + 176) = 640 - l0196 * mc0024
  Next l0196
  For l0196 = 0 To 87
    m003C(l0196) = 382
  Next l0196
  For l0196 = 88 To 100
    m003C(l0196) = 197
  Next l0196
  For l0196 = 101 To 162
    m003C(l0196) = 201
  Next l0196
  For l0196 = 163 To 175
    m003C(l0196) = 196
  Next l0196
  For l0196 = 176 To 263
    m003C(l0196) = 67
  Next l0196
  m0080(0) = 0: m0080(1) = 0: m0080(2) = 0: m0080(3) = 10: m0080(4) = 10
  m0080(5) = 0: m0080(6) = 0: m0080(7) = 0: m0080(8) = 10: m0080(9) = 10
  m00AC(0) = -20: m00AC(1) = -30: m00AC(2) = -40: m00AC(3) = -50
  m00AC(4) = -40: m00AC(5) = -30: m00AC(6) = -20: m00AC(7) = -9
  m0052 = 95
  m0056 = 10
  m0060 = mc0062
  m0058 = m005A
  m00CA = m00C6
  m00CC = m00C8
  m0066 = 0
  m006E = 0
  m0074 = mc0062
  m006C = m0068
  m00F0 = -1
  lblMunition.Width = gv01C2 * 100
End Sub


Sub sub030C ()
  m0020 = extfn01A1(m00CC, m0026(m0052), m003C(m0052) + m00AC(m0056), m0056, m0066)
  m0056 = m0056 + 1
End Sub

Sub sub0317 ()
  m0056 = (m0056 + 1) Mod 10
  m0020 = extfn01A1(m00CA, m0026(m0052) + (m0080(m0056) * m0060), m003C(m0052), m0056, m0066)
End Sub


Sub sub0322 ()
Dim l0236
Dim l0238
      m00DA(m00F0).M0543 = m00DA(m00F0).M0543 + m00DA(m00F0).M05C8
      m00DA(m00F0).M0573 = m0026(m00DA(m00F0).M0543) - m00DA(m00F0).M0617
      m00DA(m00F0).M058F = m00DA(m00F0).M058F + 1
      If  (m00DA(m00F0).M058F > m00DA(m00F0).M059F) Then
        m00DA(m00F0).M058F = 0
      End If
      If  (m00DA(m00F0).M0543 = m00DA(m00F0).M05BE) Then
        m00DA(m00F0).M0543 = m00DA(m00F0).M05B2
        m00DA(m00F0).M054A = False
        timerHeliFire.Enabled = False
        m00F0 = -1
      ElseIf (m00DA(m00F0).M0543 = m00DA(m00F0).M05B2) Then
        m00DA(m00F0).M0543 = m00DA(m00F0).M05BE
        m00DA(m00F0).M054A = False
        timerHeliFire.Enabled = False
        m00F0 = -1
      Else
        m0020 = extfn01A1(m00DA(m00F0).M0568, m00DA(m00F0).M0573, m00DA(m00F0).M0581, m00DA(m00F0).M058F, 0)
      End If
      m0020 = fn0240(m0052, m0052, 0)
      If  (m0020 > -1) Then
        If  (m00DA(m0020).M055E = mc0130) Then
          Call sub0100("TOD1.WAV")
          For l0236 = 0 To 12
            For l0238 = 0 To mc00D6
              If  (m00DA(l0238).M05F1 = True) Then
                m0020 = extfn01A1(m00DA(l0238).M0568, m00DA(l0238).M0573, m00DA(l0238).M0581, m00DA(l0238).M058F, 0)
              End If
            Next l0238
            m0020 = extfn01A1(m00D0, m0026(m0052) - 30, m003C(m0052) - 30, l0236, 0)
            m0020 = extfn01B5(picScreen.hWnd)
          Next l0236
          m00DA(m00F0).M0543 = m00DA(m00F0).M05B2
          m00DA(m00F0).M054A = False
          timerHeliFire.Enabled = False
          m00F0 = -1
          Call sub0277
        End If
      End If
End Sub

Sub sub0337 ()
Dim l023A
' Const mc024A = 2 ' &H2%
    For l023A = 0 To mc00D6
      If  (m00DA(l023A).M05F1 = True) Then
        If  (m00DA(l023A).M05DC) Then
          If  (l023A = mc0114) Then
            m00DA(l023A).M058F = (m00DA(l023A).M058F + m00FA) Mod (m00DA(l023A).M059F + 1)
          Else
            m00DA(l023A).M058F = (m00DA(l023A).M058F + 1) Mod (m00DA(l023A).M059F + 1)
          End If
        End If
        m0020 = extfn01A1(m00DA(l023A).M0568, m00DA(l023A).M0573, m00DA(l023A).M0581, m00DA(l023A).M058F, 0)
      End If
    Next l023A
    If  (m00F4 = 2) Then
      m0020 = extfn0192(m00F2, 0, 324)
      m0020 = extfn0192(m00F2, 367, 324)
    End If
    If  (m00F4 = 1) Then
      m0020 = extfn0192(m00F6, 366, 324)
      m0020 = extfn0192(m00F2, 423, 324)
    End If
    If  (m0058 = m005A) Then
      m0020 = extfn01A1(m00CA, m0026(m0052) + (m0080(m0056) * m0060), m003C(m0052), m0056, m0066)
    ElseIf (m0058 = mc005C) Then
      Call sub0317
    ElseIf (m0058 = mc005E) Then
      Call sub030C
    End If
    If  (m006E > 1) Then
      m0020 = extfn0192(m006C, m0026(m0070), m0072)
    End If
    If  (m00F0 > -1) Then
      Call sub0322
    End If
    If  (m0078 > 0) Then
      m0020 = extfn0192(m0076, m0026(m007A), m007C)
    End If
    m0020 = extfn01A1(m00F8, 583, 459, gv01BE - 1, 0)
    m0020 = extfn01B5(picScreen.hWnd)
End Sub


Sub timerHeliFire_Timer ()
  If  (m0078 = 0) Then
    m007E = m00DA(m00F0).M05C8
    If  (m007E = -1) Then
      m007A = m00DA(m00F0).M0543 - 10
      m007C = 70
      m0076 = m006A
      m0078 = 10
    Else
      m007A = m00DA(m00F0).M0543
      m007C = 80
      m0076 = m0068
      m0078 = 9
    End If
  End If
End Sub


Sub timerMain_Timer ()
  gv01CA = gv01CA + 1
  m00FA = (m00FA + 1) Mod 2
  m0054 = m0052
  Call sub00BB
  If  (m0058 = mc005E And m0056 <= 7) Then
    m0052 = m0052 + m0060
    If  (Asc(Mid(gv00B2, 40, 1)) > 2) Then
      m0052 = m0052 - 1
    End If
    If  (Asc(Mid(gv00B2, 38, 1)) > 2) Then
      m0052 = m0052 + 1
    End If
  Else
    If  (m0058 = mc005E) Then
      m0056 = 0
      If  (fn029A(m0054)) Then
        m00D4 = 0
        timerMain.Enabled = False
        timerSterben.Enabled = True
        timerSterben.Interval = gv01D0 / 4
      End If
    End If
    m0058 = m005A
    If  (Asc(Mid(gv00B2, 40, 1)) > 2) Then
      m0058 = mc005C
      m0060 = mc0064
      m0052 = m0052 - 1
      m00CA = m00C2
      m00CC = m00C4
    End If
    If  (Asc(Mid(gv00B2, 38, 1)) > 2) Then
      m0058 = mc005C
      m0060 = mc0062
      m0052 = m0052 + 1
      m00CA = m00C6
      m00CC = m00C8
    End If
    If  (Asc(Mid(gv00B2, 39, 1)) > 2) Then
      m0058 = mc005E
      m0056 = 0
    End If
  End If
  If  (m0058 > m005A) Then
    Call sub0285
    If  (fn02AD(m0054)) Then
      m00D4 = 0
      timerMain.Enabled = False
      timerSterben.Enabled = True
      timerSterben.Interval = gv01D0 / 4
    End If
    If  (m0052 >= mc0022) Then
      m0052 = 0
      gv01B2 = mc024A
      frmLoading.Show
      Unload Me
      Exit Sub
    End If
    If  (m0052 < 0) Then
      m0052 = m0054
      m0058 = m005A
    End If
    m0054 = m0052
    If  (m0052 > 200 And m00DA(mc0100).M05DC And m00F0 <> mc0100) Then
      m00F0 = mc0100
      m00DA(mc0100).M054A = True
      Call timerHeliFire_Timer
      timerHeliFire.Enabled = True
    End If
    If  (m0052 > 112 And m0052 < 160 And m00DA(mc0106).M05DC And m00DA(mc0106).M05F1 = False) Then
      Call sub0100("MONSTER.WAV")
      m00DA(mc0106).M054A = True
      m00DA(mc0106).M05F1 = True
      m00DA(mc0106).M0543 = m00DA(mc0106).M05B2
      m00DA(mc0106).M058F = 0
    End If
  End If
  If  (m00DA(mc0106).M05F1) Then
    m00DA(mc0106).M0543 = m00DA(mc0106).M0543 - 2
    If  (m00DA(mc0106).M0543 < m00DA(mc0106).M05BE) Then
      m00DA(mc0106).M054A = False
      m00DA(mc0106).M05F1 = False
    End If
    If  (m00DA(mc0106).M058F = m00DA(mc0106).M059F) Then
      m00DA(mc0106).M058F = 4
    End If
    m00DA(mc0106).M0573 = m0026(m00DA(mc0106).M0543) - 80
    m0054 = m0052 + 3
    Call sub0285
    m0054 = m0052
  End If
  If  (m006E > 0) Then
    Call sub0259
  End If
  If  (m0078 > 0) Then
    Call sub0266
  End If
  Call sub0337
End Sub


Sub timerSterben_Timer ()
Dim l0254
    If  (m00D4 = 1) Then
      Call sub0100("TOD2.WAV")
    End If
    For l0254 = 0 To mc00D6
      If  (m00DA(l0254).M05F1 = True) Then
        m0020 = extfn01A1(m00DA(l0254).M0568, m00DA(l0254).M0573, m00DA(l0254).M0581, m00DA(l0254).M058F, 0)
      End If
    Next l0254
    If  (m00D4 < 6) Then
      m0020 = extfn01A1(m00D2, m0026(m0052), m003C(m0052), m00D4, 0)
    End If
    If  (m00F4 = 2) Then
      m0020 = extfn0192(m00F2, 0, 324)
      m0020 = extfn0192(m00F2, 367, 324)
    End If
    If  (m00F4 = 1) Then
      m0020 = extfn0192(m00F6, 366, 324)
      m0020 = extfn0192(m00F2, 423, 324)
    End If
    m0020 = extfn01B5(picScreen.hWnd)
    m00D4 = m00D4 + 1
    If  (m00D4 = 7) Then
      timerSterben.Enabled = False
      timerMain.Enabled = True
      Call sub0277
    End If
End Sub
