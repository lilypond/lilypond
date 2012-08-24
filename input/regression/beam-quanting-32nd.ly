
\header {

texidoc = "Stem lengths take precedence over beam quants: `forbidden'
    quants are only avoided for 32nd beams when they are outside of
    the staff. However, that leads to very long stems, which is even
    worse."

}

\version "2.16.0"

\relative c''{ 
  \time 3/8
  a'16[ b] g[ b]
  a32[ b] g[ b] 
  g16[ a] f[ a]
  g32[ a] f[ a] 
  f16[ g] e[ g]
  f32[ g] e[ g] 
  e16[ f] d[ f]
  e32[ f] d[ f] 
  d16[ e] c[ e]
  d32[ e] c[ e]
  c16[ d] b[ d]
  c32[ d] b[ d]
  a'32[ a] g[ g] f[ f] e[ e] d[ d] c[ c]
}
