\version "2.23.8"
\include "example-header.ily"

#(set-global-staff-size 23)

\include "gregorian.ly"

\score {
  <<
    \new VaticanaVoice = "cantus" {
      \clef "vaticana-do3"
      % Verse 1 — Salve, Regína
      a\melisma \[ a \flexa g \pes a\melismaEnd \] d
      \divisioMinima
      \[ a\melisma \flexa g\melismaEnd \]
      \[ f\melisma \flexa e f \pes g \flexa f\melismaEnd \]
      \[ e\melisma \flexa d\melismaEnd \]
      \divisioMaior
      c d \[d\melisma \flexa c\melismaEnd \] d
      \[ e\melisma \pes f\melismaEnd\] g
      \[d\melisma \pes e \flexa c\melismaEnd \] d
      \finalis
      % Verse 2 — Vita, dulcédo
      % a\melisma \[ a \flexa g \pes a\melismaEnd \] d
      % \divisioMinima
      % \[ a\melisma \flexa g\melismaEnd \]
      % \[ f\melisma \flexa e f \pes g \flexa f\melismaEnd \]
      % \[ e\melisma \flexa d\melismaEnd \]
      % \divisioMaior
      % c d \[e\melisma \pes f\melismaEnd \] g
      % \[d\melisma \pes e \flexa c\melismaEnd \] d
      % \finalis
      % Verse 3 — Ad te clamámus
      \[ d\melisma \pes f\melismaEnd\] a g
      \[ g\melisma \flexa f \pes a\melismaEnd\] e
      \divisioMaior
      g f \[ e\melisma \flexa d \pes g\melismaEnd \]
      \divisioMinima
      c d \[ e\melisma \flexa d \pes g\melismaEnd \]
      \[ f\melisma \flexa e\melismaEnd \] d
      \finalis
      % Verse 4 — Ad te suspirámus
      \[ d\melisma \pes f\melismaEnd \] a c' g
      \[ g\melisma \flexa f \pes g\melismaEnd \] a
      \divisioMaior
      d \[ f\melisma \pes \deminutum g\melismaEnd \] g d
      \[ \virga f\melisma \inclinatum e \inclinatum d\melismaEnd \]
      c \divisioMaior
      d \[ d\melisma \flexa c \pes f\melismaEnd \]
      \[ g\melisma \pes a\melismaEnd \]
      g \[ f\melisma \flexa e\melismaEnd \] g
      \[ f\melisma \flexa \deminutum e\melismaEnd \]
      \[ d\melisma \flexa c \pes d\melismaEnd \]
      \finalis
      % Verse 5 — Eia ergo, Advocáta nostra
      f\melisma \[ f \pes g\melismaEnd \] f
      \[ g\melisma \pes \deminutum a\melismaEnd \] a
      \divisioMinima
      c' g \[ \virga a\melisma \inclinatum g \inclinatum f\melismaEnd \] d g a
      \divisioMaior
      d' d' \[ c'\melisma \flexa b c' \pes d'\melismaEnd \] a
      \divisioMinima
      d' c' a \[ g\melisma \flexa f \pes a\melismaEnd \] g
      \[ d\melisma \pes e\melismaEnd \] f
      \[ \virga e\melisma \inclinatum d \inclinatum c\melismaEnd \]
      \divisioMaior
      \[ c\melisma \pes d\melismaEnd \]
      f \[ g\melisma \flexa \deminutum f\melismaEnd \]
      \[ d\melisma \flexa c \pes d\melismaEnd \] d
      \finalis
      % Verse 6 — Et Jesum
      d a, \[ c\melisma \pes d\melismaEnd \]
      \divisioMinima
      d \[ d\melisma \pes e\melismaEnd \] \[ e\melisma \flexa d d\melismaEnd \]
      c g f \[ e\melisma \flexa \deminutum d\melismaEnd \] g
      \[ f\melisma \flexa e\melismaEnd \]
      \[ d\melisma \flexa c \pes d\melismaEnd \]
      \[ d\melisma \pes a \] \virga bes\melismaEnd a
      \divisioMinima
      \[ \virga a\melisma \inclinatum g \inclinatum f\melismaEnd \] g d f
      \[ f\melisma \flexa e\melismaEnd \] \[ d\melisma \flexa c\melismaEnd \]
      \divisioMinima
      \[ e\melisma \pes f \flexa e\melismaEnd \] d d
      \finalis
      % Verse 7 ad finem — O clemens: O pia: O dulcis Virgo María
      a\melisma \[a \flexa g a \quilisma b \pes c'\melismaEnd \]
      \[ \virga b\melisma \inclinatum a \inclinatum g\melismaEnd \] a
      \finalis
      \[ g\melisma \pes a \quilisma b \pes c' \] \[ c' \flexa b\melismaEnd \]
      \[ a\melisma \flexa g\melismaEnd \] \[ g\melisma \pes a\melismaEnd \]
      \finalis
      \[ a\melisma \flexa d \virga f \inclinatum e \inclinatum d \inclinatum c d \]
      \divisioMinima
      \[ d \pes e f \pes g\melismaEnd \]
      \[ g\melisma \flexa \deminutum f\melismaEnd \]
      \[ g\melisma \pes a\melismaEnd \]
      d c d \[ d\melisma \pes g \flexa f f\melismaEnd \]
      \[ e\melisma \flexa d\melismaEnd \]
      \finalis
    }
    \new VaticanaLyrics \lyricsto "cantus" {
      Sal -- ve, Re -- gí -- na, ma -- ter mi -- se -- ri -- cór -- di -- ae:
      % Vi -- ta, dul -- cé -- do, et spes no -- stra, sal -- ve.
      Ad te cla -- má -- mus, éx -- su -- les, fi -- li -- i He -- vae.
      Ad te su -- spi -- rá -- mus, ge -- mén -- tes et flen -- tes
        in hac la -- cri -- má -- rum val -- le.
      E -- ia er -- go, Ad -- vo -- cá -- ta no -- stra,
        il -- los tu -- os mi -- se -- ri -- cór -- des
        ó -- cu -- los ad nos con -- vér -- te.
      Et Je -- sum, be -- ne -- díc -- tum fruc -- tum ven -- tris tu -- i,
        no -- bis post hoc ex -- sí -- li -- um os -- tén -- de.
      O cle -- mens:
      O pi -- a:
      O dul -- cis Vir -- go Ma -- rí -- a.
    }
  >>
}
