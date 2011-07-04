%% DO NOT EDIT this file manually; it is automatically
%% generated from LSR http://lsr.dsi.unimi.it
%% Make any changes in LSR itself, or in Documentation/snippets/new/ ,
%% and then run scripts/auxiliar/makelsr.py
%%
%% This file is in the public domain.
\version "2.14.0"

\header {
  lsrtags = "contexts-and-engravers, midi"

%% Translation of GIT committish: 615cbf212fdaf0b220b3330da417d0c3602494f2
  texidoces = "
Al producir una salida MIDI, el comportamiento predeterminado es que
cada pentagrama representa un canal MIDI, con todas las voces de dicho
pentagrama mezcladas.  Esto reduce al mínimo el riesgo de que se agote
el número de canales MIDI disponibles, pues existe un máximo de 16
canales por cada puerto MIDI, y la mayoría de los dispositivos sólo
tiene un puerto.

Sin embargo, cuando se traslada el interpretador
@code{Staff_performer} al contexto @code{Voice}, cada voz de un
pentagrama puede tener su propio canal MIDI, como se muestra en el
siguiente ejemplo: a pesar de estar sobre el mismo pentagrama, se
crean dos canales MIDI, cada uno con un @code{midiInstrument}
distinto.

"
  doctitlees = "Modificar la salida MIDI para que tenga un canal por cada voz"

%% Translation of GIT committish: d7cf09411ee80eaf0092af0aa532de64c0c6248e
  texidocfr = "
Lorsque LilyPond génère un fichier MIDI, chaque portée sera par défaut
affectée à un canal, quel que soit le nombre de voix qu'elle contient.
Ceci permet d'éviter de se retrouver à court de canaux, sachant qu'il
n'y en a que seize de disponibles.

Le fait de déplacer le @code{Staff_performer} dans le contexte
@code{Voice} permet d'affecter à chaque voix d'une même portée un canal
MIDI spécifique.  Dans l'exemple suivant, la même portée donnera lieu à
deux canaux MIDI différents, chacun étant affecté de son propre
@code{midiInstrument}.

"
  doctitlefr = "Affectation d'un canal MIDI par voix"

  texidoc = "
When outputting MIDI, the default behavior is for each staff to
represent one MIDI channel, with all the voices on a staff amalgamated.
This minimizes the risk of running out of MIDI channels, since there
are only 16 available per track.

However, by moving the @code{Staff_performer} to the @code{Voice}
context, each voice on a staff can have its own MIDI channel, as is
demonstrated by the following example: despite being on the same staff,
two MIDI channels are created, each with a different
@code{midiInstrument}.

"
  doctitle = "Changing MIDI output to one channel per voice"
} % begin verbatim

\score {
  \new Staff <<
    \new Voice \relative c''' {
      \set midiInstrument = #"flute"
      \voiceOne
      \key g \major
      \time 2/2
      r2 g-"Flute" ~
      g fis ~
      fis4 g8 fis e2 ~
      e4 d8 cis d2
    }
    \new Voice \relative c'' {
      \set midiInstrument = #"clarinet"
      \voiceTwo
      b1-"Clarinet"
      a2. b8 a
      g2. fis8 e
      fis2 r
    }
  >>
  \layout { }
  \midi {
    \context {
      \Staff
      \remove "Staff_performer"
    }
    \context {
      \Voice
      \consists "Staff_performer"
    }
    \context {
      \Score
      tempoWholesPerMinute = #(ly:make-moment 72 2)
    }
  }
}

