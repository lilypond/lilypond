\version "1.5.68"
% include file for tutorial

\header {
  title = "Zo, goed lieverd?"
  subtitle = "How's, this babe?"
  composer = "JCN"
  opus = "1"
  piece = "Laid back"
}
global = {
  \time 2/4
  \skip 2*4 \bar "|."
}
Key = \notes \key as \major
flautoI = \notes\relative c'' {
  f8 g f g f g f g
  bes as bes as bes as bes as
}
flautoII = \notes\relative c'' {
  as8 bes as bes R1 d4 ~ d
}
tromboI = \notes\relative c'' {
  c4. c8 c8 c4. es4 r as, r
}
tromboII = \notes\relative c'' {
  as4. as8 as8 as4. R1*1/2 as4 es'
}
timpani = \notes\relative c, {
  \times 2/3 { f4 f f }
  \times 4/5 { as8 as as as as }
  R1
}
corno = \notes\relative c' {
   bes4 d f, bes d f, bes d
}
