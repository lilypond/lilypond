\header {
  texidoc = "Meshing stems in oppositely directed beams are handled
  correctly."  
}

\version "2.19.21"

\layout {
  ragged-right = ##t
}

{
  \relative { << { s16 e''16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative { << { s16 e''16 [ s cis, ] } \\ { b''16 [ s b ] } >> }
  \relative { << { s16 d''16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative { << { s16 c''16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative { << { s16 b'16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative { << { s16 a'16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative { << { s16 g'16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative { << { s16 c'16 [ s cis' ] } \\ { b'16 [ s b ] } >> }
  \relative { << { s16 c'16 [ s cis'' ] } \\ { b16 [ s b ] } >> }
  \relative { << { s16 f'16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative { << { s16 e'16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative { << { s16 d'16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative { << { s16 e''16 [ s cis ] } \\ { b'16 [ s d ] } >> }
  \relative { << { s16 e''16 [ s cis ] } \\ { b'16 [ s f' ] } >> }
  \relative { << { s16 e''16 [ s cis ] } \\ { b'16 [ s a ] } >> }
  \relative { << { s16 e''16 [ s cis ] } \\ { b'16 [ s gis ] } >> }
}
