\version "2.23.14"

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
  \skip 2 \textMark "!"
  \skip 2 \textMark ";"
  \skip 2 \textMark "'"
  \skip 2 \textMark ","
  \skip 2 \textMark "|"
  \skip 2 \textMark \markup \tiny "-span|"
  \skip 2 \textMark "||"
  \skip 2 \textMark "|."
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
