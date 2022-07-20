\version "2.23.12"

divisions = {
  \time 2/4
  s2 % default measure bar here
  s2 % start-repeat bar here
  \repeat volta 2 s2 % double repeat bar here
  \repeat volta 2 s2 % end-repeat bar here
  s2 \caesura
  s2 \caesura \shortfermata
  s2 \caesura \fermata
  s2 \section
  s2 \fine
}

labels = {
  \time 2/4
  \skip 2 \mark "Meas."
  \skip 2
  \repeat volta 2 \skip 2
  \repeat volta 2 \skip 2
  \skip 2 \sectionLabel "\\caesura ..."
  \skip 2
  \skip 2
  \skip 2 \mark "Sec."
  \skip 2
}

music = \fixed c' {
  \time 2/4
  c2
  d2
  \repeat volta 2 e2
  \repeat volta 2 f2
  g2
  f2
  e2
  d2
  c2
}
