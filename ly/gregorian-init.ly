\version "2.4.0"

%%%%%%%%
%%%%%%%% shortcuts common for all styles of gregorian chant notation
%%%%%%%%

%
% declare head prefix shortcuts
%
virga =
  \once \override NoteHead  #'virga = ##t
stropha =
  \once \override NoteHead  #'stropha = ##t
inclinatum =
  \once \override NoteHead  #'inclinatum = ##t
auctum =
  \once \override NoteHead  #'auctum = ##t
descendens =
  \once \override NoteHead  #'descendens = ##t
ascendens =
  \once \override NoteHead  #'ascendens = ##t
pes =
  \once \override NoteHead  #'pes-or-flexa = ##t
flexa =
  \once \override NoteHead  #'pes-or-flexa = ##t
oriscus =
  \once \override NoteHead  #'oriscus = ##t
quilisma =
  \once \override NoteHead  #'quilisma = ##t
deminutum =
  \once \override NoteHead  #'deminutum = ##t
linea =
  \once \override NoteHead  #'linea = ##t
cavum =
  \once \override NoteHead  #'cavum = ##t

%
% declare divisiones shortcuts
%
virgula = {
  \once \override BreathingSign  #'text = #(make-musicglyph-markup "scripts-rcomma")
  \once \override BreathingSign  #'font-size = #-2

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \override BreathingSign  #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}
caesura = {
  \once \override BreathingSign  #'text = #(make-musicglyph-markup "scripts-rvarcomma")
  \once \override BreathingSign  #'font-size = #-2

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \override BreathingSign  #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}
divisioMinima = {
  \once \override BreathingSign  #'print-function = #Breathing_sign::divisio_minima

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \override BreathingSign  #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}
divisioMaior = {
  \once \override BreathingSign  #'print-function = #Breathing_sign::divisio_maior
  \once \override BreathingSign  #'Y-offset-callbacks = #'()

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \override BreathingSign  #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}
divisioMaxima = {
  \once \override BreathingSign  #'print-function = #Breathing_sign::divisio_maxima
  \once \override BreathingSign  #'Y-offset-callbacks = #'()

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \override BreathingSign  #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}
finalis = {
  \once \override BreathingSign  #'print-function = #Breathing_sign::finalis
  \once \override BreathingSign  #'Y-offset-callbacks = #'()

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \override BreathingSign  #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}

%
% declare articulation shortcuts
%
accentus = #(make-articulation "accentus")
ictus = #(make-articulation "ictus")
semicirculus = #(make-articulation "semicirculus")
circulus = #(make-articulation "circulus")
episemInitium = #(make-span-event 'TextSpanEvent START)
episemFinis = #(make-span-event 'TextSpanEvent STOP)

%
% shortcut music functions for Liber Hymnarius neumes table
% (experimental)
%
%#(define (make-script x)
%   (make-music 'ArticulationEvent
%               'articulation-type x))
%    
%#(define (add-script m x)
%   (if
%     (equal? (ly:music-property m 'name) 'EventChord)
%     (set! (ly:music-property m 'elements)
%           (cons (make-script x)
%                 (ly:music-property m 'elements))))
%   m)
%
%#(define (add-staccato m)
%   (add-script m "staccato"))
%
% % \applymusic #(lambda (x) (music-map add-staccato x)) { c c }
%
% % \climacus { x y z ... }:
% % \[ \virga x \inclinatum y \inclinatum z ... \]
%
%#(defmacro def-climacus-function (start stop)
%  `(def-music-function (location music) (ly:music?)
%     (make-music 'SequentialMusic
%        'elements (list 'LigatureStartEvent
%			(ly:music-deep-copy ,start)
%                        music
%                        (ly:music-deep-copy ,stop)
%			'LigatureStopEvent))))
%climacus = #(def-climacus-function startSequentialMusic stopSequentialMusic)

%
% example layout block for gregorian chant notation
%

neumeDemoLayout = \layout {
    interscoreline = 1
    \context {
	\Score
	\remove "Bar_number_engraver"
    }
    \context {
	\Staff
	\remove "Clef_engraver"
	\remove "Key_engraver"
	\override StaffSymbol #'transparent = ##t
	\remove "Time_signature_engraver"
	\remove "Bar_engraver"
	minimumVerticalExtent = ##f
    }
    \context {
	\Voice
	\remove Ligature_bracket_engraver
	\consists Vaticana_ligature_engraver
	\override NoteHead #'style = #'vaticana_punctum
	\override Stem #'transparent = ##t
    }
}
