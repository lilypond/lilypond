\version "2.11.51"
\header {
    title	= "Puer natus est nobis"
    subtitle	= "Antiphona ad introitum VII"
    texidoc = "

Demonstrate gregorian chant notation

This file pretty nicely demonstrates what still does not work among
ligatures: (i) horizontal spacing between ligatures and lyrics
aligment is broken; (ii) there is no satisfactory support for
initials; (iii) all scripts are broken as soon as they appear within
ligatures, and episem is broken also outside of ligatures; (iv)
augmentum dots is missing; (v) accidentals must be placed before the
ligature (not demonstrated in this example)."

}

#(set-global-staff-size 26)
\include "gregorian-init.ly"

%%% N.B.: Yes, I know, the formatting of the following looks awful,
%%% but this is intentional for editorial purposes (simplifies some
%%% global search/replace operations in emacs).

cantus = \new VaticanaVoice = "cantus"  {
  \set Staff.instrumentName = \markup {
    \column {
      " " " " " " " " "VII" " "
      {
	\bigger \bigger \bigger \bigger
	\bigger \bigger \bigger \bigger
	\bigger \bigger \bigger \bigger
	"P"
      }
    }
  }
  \[
    g4\melisma	%%% Pu-
    \pes
    d'\melismaEnd
  \]
  d'\augmentum	%%% er
  \[
    d'\melisma	%%% na-
    \pes e' \flexa
    d'\melismaEnd
  \]
  c'		%%% tus
  \[
    c'\melisma	%%% est
    c'
    c'\melismaEnd
  \]
  \[
    d'\melisma	%%% no-
    \flexa c' e' \flexa
    d'\melismaEnd
  \]
  d'\augmentum  %%% bis,
  \divisioMaior
  \[
    g\melisma	%%% et
    \pes \deminutum
    d'\melismaEnd
  \]
  \[
    d'\melisma	%%% fí-
    \pes e' \flexa
    d'\melismaEnd
  \]
  \[
    c'\melisma	%%% li-
    \flexa
    b\melismaEnd
  \]
  a		%%% us
  \[
    c'\melisma	%%% da-
    c' \pes
    d'\melismaEnd
  \]
  c'		%%% tus-
  c'		%%% est
  \[
    c'\melisma	%%% no-
    \pes d' \flexa c'
    c'\melismaEnd
  \]
  \[
    g\melisma	%%% bis:
    \pes a \flexa
    g\melismaEnd\augmentum
  \]
  \divisioMaxima
  g		%%% cu-
  a		%%% ius
  c'		%%% im-
  \[
    b\melisma	%%% pé-
    \pes d' e'
    \pes f'\melismaEnd
  \]
  \[
    d'\melisma	%%% ri-
    \flexa c'\melismaEnd
  \]
  c'\augmentum	%%% um
  \divisioMinima
  c'		%%% su-
  c'		%%% per
  \[
    d'\melisma	%%% hú-
    \flexa c'
    e' \flexa
    d'\melismaEnd
  \]
  \[
    c'\melisma	%%% me-
    \flexa b\melismaEnd
  \]
  \[
    c'\melisma	%%% rum
    c' c'\melismaEnd
  \]
  \[
    c'\melisma	%%% e-
    \flexa a
    c' \flexa b
    \virga c' \inclinatum b
    \inclinatum a\melismaEnd
  \]
  \[
    b\melisma	%%% ius:
    \augmentum \flexa
    a\melismaEnd \augmentum
  \]
  \divisioMaxima
  \[
    c'\melisma	%%% et
    \flexa b\melismaEnd
  \]
  c'		%%% vo-
  \[
    c'\melisma	%%% cá-
    \pes e' \flexa
    d'\melismaEnd
  \]
  c'		%%% bi-
  \[
    c'\melisma	%%% tur
    c' c'\melismaEnd
  \]
  c'		%%% no-
  \[
    c'\melisma	%%% men
    c' c'\melismaEnd
  \]
  \[
    c'\melisma  %%% e-
    \pes d' \flexa b
    \virga c' \inclinatum b
    \inclinatum a\melismaEnd
  \]
  \[
    b\melisma	%%% ius,
    \augmentum \flexa
    a\melismaEnd\augmentum
  \]
  \divisioMaior
  \[
    c'\melisma	%%% ma-
    \pes e'\melismaEnd
  \]
  d'		%%% gni
  \[
    g\melisma	%%% con-
    \pes \deminutum
    c'\melismaEnd
  \]
  c'		%%% sí-
  \[
    c'\melisma	%%% li-
    c' c' \flexa a\melismaEnd
  \]
  a		%%% i
  \[
    a\melisma	%%% An-
    \pes c' \flexa a
    \quilisma b \pes
    c'\melismaEnd
  \]
  \[
    g\melisma	%%% ge-
    \pes a \flexa
    g\melismaEnd
  \]
  g\augmentum	%%% lus.
  s_\markup { \italic { "Ps." } }
  \finalis
  \[
    g\melisma	%%% Can-
    \pes c' \flexa b\melismaEnd
  \]
  \[
    c'\melisma	%%% tá-
    \pes d'\melismaEnd
  \]
  d'		%%% te
  d'		%%% Dó-
  d'		%%% mi-
  d'		%%% no
  \[
    d'\melisma	%%% cán-
    \pes f'\melismaEnd
  \]
  e'		%%% ti-
  e'		%%% cum
  \[
    e'\melisma  %%% no-
    \flexa d'\melismaEnd
  \]
  \[
    d'\melisma	%%% vum:
    \augmentum \pes
    e'\melismaEnd \augmentum
  \]
  \[
    d'\melisma	%%% qui-
    \flexa b\melismaEnd
  \]
  \[
    c'\melisma	%%% a
    \pes d'\melismaEnd
  \]
  d'		%%% mi-
  d'		%%% ra-
  \[
    d'\melisma	%%% bí-
    \quilisma e'
    \pes f'\melismaEnd
  \]
  d'		%%% li-
  c'		%%% a
  \[
    c'\melisma	%%% fe-
    c' c'\melismaEnd
  \]
  \[
    a\melisma  %%% cit.
    \augmentum \flexa
    g\melismaEnd \augmentum
  \]
  \finalis
}

verba = \new Lyrics = "verba" \lyricmode {
  U -- ER na -- tus est no -- bis,
  et fí -- li -- us da -- tus est no -- "bis :"
  cu -- ius im -- pé -- ri -- um su -- per
  hú -- me -- rum e -- "ius :" et vo -- cá --
  bi -- tur no -- men e -- ius, ma -- gni
  con -- sí -- li -- i An -- ge -- lus.
  Can -- tá -- te Dó -- mi -- no cán --
  ti -- cum no -- "vum :" qui -- a mi -- ra --
  bí -- li -- a fe -- cit.
}

\paper  {
%    line-thickness = \staff-space / 7.0
%    line-thickness = \staff-space / 3.0
}

\score {
  <<
    \cantus
    \lyricsto "cantus" \verba
  >>
  \layout {
    indent = 17.0\mm
  }
}

%%% Local Variables:
%%% coding: utf-8
%%% End:
