' Module3
Option Explicit
Declare Function extfn01A1 Lib "RAMM16.DLL" Alias "PaintMultiSprite" (ByVal p1%, ByVal p2%, ByVal p3%, ByVal p4%, ByVal p5%) As Integer
Declare Function extfn0192 Lib "RAMM16.DLL" Alias "PaintSprite" (ByVal p1%, ByVal p2%, ByVal p3%) As Integer
Declare Function extfn01B5 Lib "RAMM16.DLL" Alias "RefreshContext" (ByVal p1%) As Integer
Declare Function extfn0156 Lib "RAMM16.DLL" Alias "RegisterContext" (ByVal p1%) As Integer
Declare Function extfn017B Lib "RAMM16.DLL" Alias "RegisterMultiSprite" (ByVal p1%, ByVal p2&, ByVal p3%, ByVal p4%) As Integer
Declare Function extfn01C7 Lib "RAMM16.DLL" Alias "RegisterQuadSprite" (ByVal p1%) As Integer
Declare Function extfn0169 Lib "RAMM16.DLL" Alias "RegisterSprite" (ByVal p1%, ByVal p2&) As Integer
