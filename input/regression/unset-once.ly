\version "2.19.21"

\header {

  texidoc = "@code{\\once \\unset} should change a context property
value for just one timestep and then return to the previous value."

}
\relative {
  \set fingeringOrientations = #'(left)
  <e'-1>1 |
  \once \unset fingeringOrientations
  <e-1>-"default" |
  <e-1>-"left" |

  \unset fingeringOrientations
  <e-1>-"default" |
  \once\unset fingeringOrientations
  <e-1>-"default" |
  <e-1>-"default" |
  \set Score.fingeringOrientations = #'(right)
  <e-1>-"right"
  \once\unset fingeringOrientations
  <e-1>-"right"
  <e-1>-"right"
  \once\set fingeringOrientations = #'(left)
  <e-1>-"left"
  <e-1>-"right"
  \set fingeringOrientations = #'(left)
  <e-1>-"left"
  \once \unset fingeringOrientations
  <e-1>-"right"
  <e-1>-"left"
  \unset fingeringOrientations
  \set Score.fingeringOrientations = #'(up down)
  <e-1>-"default"
}
