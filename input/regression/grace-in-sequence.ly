\version "2.27.4"

\header { texidoc = "Grace sequences can be chained, also when wrapped in
  sequential music."  }

{
  c'1 \grace { c'16 d' e' } \grace { f' g' a' b' } c'1
}

{
  { c'1 \grace { c'16 d' e' } } { \grace { f' g' a' b' } c'1 }
}

{
  { c'1 \grace { c'16 d' e' } } \grace { f' g' a' b' } c'1
}
