\version "2.25.17"

\header {
  texidoc = "Many complex knee beams that test peculiar edge cases (such as
  subdivision or # of beams changes by@tie{}@geq{}@tie{}2)."
}

\layout {
  indent = #0
  ragged-right = ##t
}

{  
  \stopStaff
  \omit Staff.Clef
  \omit Staff.TimeSignature
  \override Beam.auto-knee-gap = 0

  c''''256[ c512 c c8  
  c''''128  c256 c c8 
  c''''64   c128 c c8 
  c''''32   c64  c c8 
  ] s128 s4. |
  c''''32[  c64  c c8  
  c''''64   c128 c c8 
  c''''128  c256 c c8 
  c''''256  c512 c c8 
  ] s128 s4. |
  c''''256[ c512 c c8  
  c''''64   c128 c c8 
  c''''16   c32  c c8 
  ] s64. s4.. |
  c''''16[  c32  c c8  
  c''''64   c128 c c8 
  c''''256  c512 c c8 
  ] s64. s4.. |

  c''''256[ \set stemLeftBeamCount = 1 c512 c c8
  c''''128  \set stemLeftBeamCount = 1 c256 c c8 
  c''''64   \set stemLeftBeamCount = 1 c128 c c8 
  c''''32   \set stemLeftBeamCount = 1 c64  c c8 
  ] s128 s4. |
  c''''32[  \set stemLeftBeamCount = 1 c64  c c8  
  c''''64   \set stemLeftBeamCount = 1 c128 c c8 
  c''''128  \set stemLeftBeamCount = 1 c256 c c8 
  c''''256  \set stemLeftBeamCount = 1 c512 c c8 
  ] s128 s4. |
  c''''256[ \set stemLeftBeamCount = 1 c512 c c8  
  c''''64   \set stemLeftBeamCount = 1 c128 c c8 
  c''''16   \set stemLeftBeamCount = 1 c32  c c8 
  ] s64. s4.. |
  c''''16[  \set stemLeftBeamCount = 1 c32  c c8  
  c''''64   \set stemLeftBeamCount = 1 c128 c c8 
  c''''256  \set stemLeftBeamCount = 1 c512 c c8 
  ] s64. s4.. |


  c''''256[ c c8  
  c''''128  c c8 
  c''''64   c c8 
  c''''32   c c8 
  ] s128 s4. |
  c''''32[  c c8  
  c''''64   c c8 
  c''''128  c c8 
  c''''256  c c8 
  ] s128 s4. |
  c''''256[ c c8  
  c''''64   c c8 
  ] s64. s8.
  c''''64[ c c8  
  c''''256   c c8 
  ] s64. s8. | \break

  c256[ c''''512 c'''' c''''8  
  c128  c''''256 c'''' c''''8 
  c64   c''''128 c'''' c''''8 
  c32   c''''64  c'''' c''''8 
  ] s128 s4. |
  c32[  c''''64  c'''' c''''8  
  c64   c''''128 c'''' c''''8 
  c128  c''''256 c'''' c''''8 
  c256  c''''512 c'''' c''''8 
  ] s128 s4. |
  c256[ c''''512 c'''' c''''8  
  c64   c''''128 c'''' c''''8 
  c16   c''''32  c'''' c''''8 
  ] s64. s4.. |
  c16[  c''''32  c'''' c''''8  
  c64   c''''128 c'''' c''''8 
  c256  c''''512 c'''' c''''8 
  ] s64. s4.. |

  c256[ \set stemLeftBeamCount = 1 c''''512 c'''' c''''8
  c128  \set stemLeftBeamCount = 1 c''''256 c'''' c''''8 
  c64   \set stemLeftBeamCount = 1 c''''128 c'''' c''''8 
  c32   \set stemLeftBeamCount = 1 c''''64  c'''' c''''8 
  ] s128 s4. |
  c32[  \set stemLeftBeamCount = 1 c''''64  c'''' c''''8  
  c64   \set stemLeftBeamCount = 1 c''''128 c'''' c''''8 
  c128  \set stemLeftBeamCount = 1 c''''256 c'''' c''''8 
  c256  \set stemLeftBeamCount = 1 c''''512 c'''' c''''8 
  ] s128 s4. |
  c256[ \set stemLeftBeamCount = 1 c''''512 c'''' c''''8  
  c64   \set stemLeftBeamCount = 1 c''''128 c'''' c''''8 
  c16   \set stemLeftBeamCount = 1 c''''32  c'''' c''''8 
  ] s64. s4.. |
  c16[  \set stemLeftBeamCount = 1 c''''32  c'''' c''''8  
  c64   \set stemLeftBeamCount = 1 c''''128 c'''' c''''8 
  c256  \set stemLeftBeamCount = 1 c''''512 c'''' c''''8 
  ] s64. s4.. |


  c256[ c'''' c''''8  
  c128  c'''' c''''8 
  c64   c'''' c''''8 
  c32   c'''' c''''8 
  ] s128 s4. |
  c32[  c'''' c''''8  
  c64   c'''' c''''8 
  c128  c'''' c''''8 
  c256  c'''' c''''8 
  ] s128 s4. |
  c256[ c'''' c''''8  
  c64   c'''' c''''8 
  ] s64. s8.
  c64[ c'''' c''''8  
  c256   c'''' c''''8 
  ] s64. s8. |
}
