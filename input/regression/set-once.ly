\version "2.19.21"

\header {

  texidoc = "@code{\\once \\set} should change a context property value for just one timestep
and then return to the previous value."

}
\relative {
  \set fingeringOrientations = #'(left)
  <e'-1>1 |
  \once \set fingeringOrientations = #'(right)
  <e-1> |
  <e-1> -"left" |

  \once \set fingeringOrientations = #'(right)
  <e-1>
  \once \set fingeringOrientations = #'(up)
  <e-1>
  <e-1>-"left"

  \set fingeringOrientations = #'(left)
  <e-1>
  \once \set fingeringOrientations = #'(up)
  \once \set fingeringOrientations = #'(right)
  <e-1>-"right"
 
  <e-1>-"left"
}
