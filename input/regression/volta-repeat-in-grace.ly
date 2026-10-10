\version "2.27.4"

\header {
  texidoc = "Volta repeats with alternatives wholly contained in grace time
don't get the alternatives mixed up."
}

{ \grace { c' \repeat volta 2 { c' \alternative { d' e' } } } g'1 }
