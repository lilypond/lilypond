\header {
  title = "Zo, goed lieverd?";
  subtitle = "How's, this babe?";
  composer = "JCN";
  piece = "Laid back";
}
global = \notes {
  \time 2/4;
  \skip 2*4; \bar "|.";
}
Key = \notes \key as \major;
flautoI = \notes\relative c'' {
  f8 g f g f g f g
  bes as bes as bes as bes as
}
flautoII = \notes\relative c'' {
%  d8 es d es R1 d4 ~ d
  as8 bes as bes R1 d4 ~ d
}
tromboI = \notes\relative c'' {
  c4. c8 c8 c4. es4 R1*1/2 es4
}
tromboII = \notes\relative c'' {
  as4. as8 as8 as4. R1*1/2 as4 es'
}
timpani = \notes\relative c, {
  \times 2/3 { f4 f f }
  \times 4/5 { as8 as as as as }
}
corno = \notes\relative c' {
   bes4 d f, bes d f, bes d
}
