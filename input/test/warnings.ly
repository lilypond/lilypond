\version "1.3.146"

\header {
texidoc="Various warnings
"
}

linebreak = \penalty #-1000

\score {
  \notes {
    \property Voice.Stem \set #'length = #"foo"
    \property Voice.Stem \set #'longth = #2
    \property Staff.clefStyle = #"foo"
    \property Staff.clefStyle = #2
 }
}
