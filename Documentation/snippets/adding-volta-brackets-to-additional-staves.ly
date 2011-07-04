%% DO NOT EDIT this file manually; it is automatically
%% generated from LSR http://lsr.dsi.unimi.it
%% Make any changes in LSR itself, or in Documentation/snippets/new/ ,
%% and then run scripts/auxiliar/makelsr.py
%%
%% This file is in the public domain.
\version "2.14.0"

\header {
  lsrtags = "repeats"

%% Translation of GIT committish: 615cbf212fdaf0b220b3330da417d0c3602494f2
  texidoces = "
El grabador @code{Volta_engraver} reside de forma predeterminada
dentro del contexto de @code{Score}, y los corchetes de la repetición
se imprimen así normalmente sólo encima del pentagrama superior.  Esto
se puede ajustar añadiendo el grabador @code{Volta_engraver} al
contexto de @code{Staff} en que deban aparecer los corchetes; véase
también el fragmento de código @qq{Volta multi staff}.

"
  doctitlees = "Añadir corchetes de primera y segunda vez a más pentagramas"


%% Translation of GIT committish: 0a868be38a775ecb1ef935b079000cebbc64de40
  texidocde = "
Der @code{Volta_engraver} befindet sich im @code{Score}-Kontext und Klammern
werden deshalb nur auf dem obersten System dargestellt.  Das kann umgangen
werden, indem man den @code{Volta_engraver} zu dem @code{Staff}-Kontext
hinzufügt, in dem die Klammern zusätzlichen vorkommen sollen.  Siehe auch
das \"Volta multi staff\"-Schnipsel.

"
  doctitlede = "Volta-Klammern zu zusätzlichen Systemen hinzufügen"

%% Translation of GIT committish: 4ab2514496ac3d88a9f3121a76f890c97cedcf4e
  texidocfr = "
D'ordinaire, le graveur @code{Volta_engraver} réside dans le contexte
@code{Score} ; les crochets précédant une reprise s'impriment donc
seulement au-dessus de la portée du haut.  On peut ajuster cela en
déplaçant ce graveur vers les contextes de portée (@code{Staff}) qui
doivent comporter ces crochets.

"
  doctitlefr = "Ajout du crochet de reprise à d'autres portées"


  texidoc = "
The @code{Volta_engraver} by default resides in the @code{Score}
context, and brackets for the repeat are thus normally only printed
over the topmost staff. This can be adjusted by adding the
@code{Volta_engraver} to the @code{Staff} context where the brackets
should appear; see also the @qq{Volta multi staff} snippet.

"
  doctitle = "Adding volta brackets to additional staves"
} % begin verbatim

<<
  \new Staff { \repeat volta 2 { c'1 } \alternative { c' } }
  \new Staff { \repeat volta 2 { c'1 } \alternative { c' } }
  \new Staff \with { \consists "Volta_engraver" } { c'2 g' e' a' }
  \new Staff { \repeat volta 2 { c'1 } \alternative { c' } }
>>

