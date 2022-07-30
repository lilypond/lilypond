\version "2.23.6"

divisions = {
  \time 2/4
  \skip 2 \bar "!"
  \skip 2 \bar ";"
  \skip 2 \bar "'"
  \skip 2 \bar ","
  \skip 2 \bar "|"
  \skip 2 \bar "-span|"
  \skip 2 \bar "||"
  \skip 2 \bar "|."
}

labels = {
  \time 2/4
  \skip 2 \mark "!"
  \skip 2 \mark ";"
  \skip 2 \mark "'"
  \skip 2 \mark ","
  \skip 2 \mark "|"
  \skip 2 \mark \markup \tiny "-span|"
  \skip 2 \mark "||"
  \skip 2 \mark "|."
}

music = \fixed c' {
  \time 2/4
  c2
  d2
  e2
  f2
  f2
  e2
  d2
  c2
}
