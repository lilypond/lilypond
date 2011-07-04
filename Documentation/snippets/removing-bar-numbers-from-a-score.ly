%% DO NOT EDIT this file manually; it is automatically
%% generated from LSR http://lsr.dsi.unimi.it
%% Make any changes in LSR itself, or in Documentation/snippets/new/ ,
%% and then run scripts/auxiliar/makelsr.py
%%
%% This file is in the public domain.
\version "2.14.0"

\header {
  lsrtags = "rhythms, contexts-and-engravers"

%% Translation of GIT committish: 615cbf212fdaf0b220b3330da417d0c3602494f2
  texidoces = "

Se pueden eliminar completamente los números de compás quitando el
grabador @code{Bar_number_engraver} del contexto de @code{Score}.

"

  doctitlees = "Suprimir los números de compás de toda la partitura"



%% Translation of GIT committish: 0a868be38a775ecb1ef935b079000cebbc64de40
  texidocde = "
Taktnummern können vollkommen aus den Noten entfernt werden, indem
man den @code{Bar_number_engraver} aus dem @code{Score}-Kontext
entfernt.

"
  doctitlede = "Entfernung von Taktnummern in einer Partitur"



%% Translation of GIT committish: 374d57cf9b68ddf32a95409ce08ba75816900f6b
  texidocfr = "
Désactiver le graveur concerné --- @code{Bar_number_engraver} ---
donnera une partition  --- contexte @code{Score} --- sans numéros de
mesure.

"
  doctitlefr = "Supprimer les numéros de mesure d'une partition"


  texidoc = "
Bar numbers can be removed entirely by removing the
@code{Bar_number_engraver} from the @code{Score} context.

"
  doctitle = "Removing bar numbers from a score"
} % begin verbatim

\layout {
  \context {
    \Score
    \remove "Bar_number_engraver"
  }
}

\relative c'' {
  c4 c c c \break
  c4 c c c
}

