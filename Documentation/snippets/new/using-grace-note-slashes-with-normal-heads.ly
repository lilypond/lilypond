\version "2.15.15"

\header {
  lsrtags = "rhythms"

%% Translation of GIT committish: 2d548a99cb9dba80f2ff035582009477cd37eceb
  texidoces = "

Es posible aplicar la barrita que cruza la barra de las
acciaccaturas, en otras situaciones.

"

  doctitlees = "Utilizar la barra que tacha las notas de adorno con notas normales"



%% Translation of GIT committish: 374d57cf9b68ddf32a95409ce08ba75816900f6b
  texidocfr = "
Le trait que l'on trouve sur les hampes des acciaccatures peut
être appliqué dans d'autres situations.

"

  doctitlefr = "Utilisation de hampe barrée pour une note normale"

  texidoc = "
The slash through the stem found in acciaccaturas can be applied in
other situations.

"
  doctitle = "Using grace note slashes with normal heads"
} % begin verbatim

\relative c'' {
  \override Flag #'stroke-style = #"grace"
  c8( d2) e8( f4)
}

