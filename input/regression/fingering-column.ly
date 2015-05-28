\version "2.19.21"

\header {
  texidoc = "Horizontal @code{Fingering} grobs that collide do not intersect.
Non-intersecting @code{Fingering} grobs are left alone. This is managed
by the @code{FingeringColumn} grob.
"
}

\relative {
   \set fingeringOrientations = #'(left)
   \override Fingering.staff-padding = #'()
   \override Fingering.add-stem-support = ##f
   <d''-0 c-3 f,-0>4 <d-0 f,-3 e-0>
   <d^0 c^3 f,-0> <d^0 f,-0> <c^3 f,-0>
   <d-0 c-0 b-0 a-0 g-0 f-0>
}
