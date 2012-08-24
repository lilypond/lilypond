\header {
  texidoc = "Meshing stems in oppositely directed beams are handled
  correctly."  
}

\version "2.16.0"

\layout {
  ragged-right = ##t
}

{
  \relative c'' { << { s16 e16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative c'' { << { s16 e16 [ s cis, ] } \\ { b''16 [ s b ] } >> }
  \relative c'' { << { s16 d16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative c'' { << { s16 c16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative c'' { << { s16 b16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative c'' { << { s16 a16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative c'' { << { s16 g16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative c'' { << { s16 c,16 [ s cis' ] } \\ { b'16 [ s b ] } >> }
  \relative c'' { << { s16 c,16 [ s cis'' ] } \\ { b16 [ s b ] } >> }
  \relative c'' { << { s16 f,16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative c'' { << { s16 e,16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative c'' { << { s16 d,16 [ s cis ] } \\ { b'16 [ s b ] } >> }
  \relative c'' { << { s16 e16 [ s cis ] } \\ { b'16 [ s d ] } >> }
  \relative c'' { << { s16 e16 [ s cis ] } \\ { b'16 [ s f' ] } >> }
  \relative c'' { << { s16 e16 [ s cis ] } \\ { b'16 [ s a ] } >> }
  \relative c'' { << { s16 e16 [ s cis ] } \\ { b'16 [ s gis ] } >> }
}
