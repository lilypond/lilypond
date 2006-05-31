\version "2.7.39"

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
  \once \override BreathingSign  #'text = #(make-musicglyph-markup "scripts.rcomma")
  \once \override BreathingSign  #'font-size = #-2

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \override BreathingSign  #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}
caesura = {
  \once \override BreathingSign  #'text = #(make-musicglyph-markup "scripts.rvarcomma")
  \once \override BreathingSign  #'font-size = #-2

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \override BreathingSign  #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}
divisioMinima = {
  \once \override BreathingSign  #'stencil = #ly:breathing-sign::divisio-minima

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \override BreathingSign  #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}
divisioMaior = {
  \once \override BreathingSign  #'stencil = #ly:breathing-sign::divisio-maior
  \once \override BreathingSign  #'Y-offset = #0

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \override BreathingSign  #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}
divisioMaxima = {
  \once \override BreathingSign  #'stencil = #ly:breathing-sign::divisio-maxima
  \once \override BreathingSign  #'Y-offset = #0

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \override BreathingSign  #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}
finalis = {
  \once \override BreathingSign  #'stencil = #ly:breathing-sign::finalis
  \once \override BreathingSign  #'Y-offset = #0

  % Workaround: add padding.  Correct fix would be spacing engine handle this.
  \once \override BreathingSign  #'extra-X-extent = #'(-1.0 . 0)

  \breathe
}

augmentum = {
  %%% TODO: A ligature head postfix that indicates that an
  %%% augmentum dot should be appended to the right end of
  %%% the surrounding ligature.  [Not yet implemented.]
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

#(define (make-ligature music)
   (make-music 'SequentialMusic
	       'elements (append
			  (cons
			   (make-music 'EventChord
				       'elements (list
						  (make-span-event 'LigatureEvent START)))
			   (ly:music-property music 'elements))
			  (list
			   (make-music 'EventChord
				       'elements (list
						  (make-span-event 'LigatureEvent STOP)))))))

ligature = #(define-music-function
	      (location music) (ly:music?)
	      (make-ligature music))

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
% % \applyMusic #(lambda (x) (music-map add-staccato x)) { c c }
%
% % \climacus { x y z ... }:
% % \[ \virga x \inclinatum y \inclinatum z ... \]
%
%#(defmacro def-climacus-function (start stop)
%  `(define-music-function (location music) (ly:music?)
%     (make-music 'SequentialMusic
%        'elements (list 'LigatureStartEvent
%			(ly:music-deep-copy ,start)
%                        music
%                        (ly:music-deep-copy ,stop)
%			'LigatureStopEvent))))
%climacus = #(def-climacus-function startSequentialMusic stopSequentialMusic)

\layout {
    indent = 0.0
    packed = ##t

    %%% TODO: should raggedright be the default?
    %ragged-right = ##t
    ragged-last = ##t

    %%% FIXME: line-thickness should be set here to a slightly
    %%% thicker value as the default; but we can not set it here,
    %%% since neither \staff-space nor #staff-space is yet defined
    %%% when lily comes across the following line.
    %line-thickness = \staff-space / 7.0

    \context {
	\VaticanaStaff
	 \override StaffSymbol #'color = #red
	 \override LedgerLineSpanner #'color = #red
    }
    \context {
	\Score
	\remove "Bar_number_engraver"

	%%%
	%%% FIXME: Musicologically seen, timing should be set to #f.
	%%% Unfortunately, setting it to #f will result in no
	%%% line-breakable items being created, such that the whole
	%%% music will end up in a single line.  Therefore, we
	%%% currently set it to #t, until the ligature code is fixed
	%%% to automatically insert breakable items.
	%%%
	timing = ##t

	%%%
	%%% FIXME: Setting barAlways to #t would fix the above
	%%% "timing = ##t" problem, but, surprisingly, it increases
	%%% the space between ligatures.  Hence, we set it to #f.
	%%%
	barAlways = ##f

	\override SpacingSpanner #'packed-spacing = ##t

	%%%
	%%% TODO: Play around with the following SpacingSpanner
	%%% settings to yield better spacing between ligatures.
	%%%
	%%% FIXME: setting #'spacing-increment to a small value
	%%% causes tons of "programming error: adding reverse spring,
	%%% setting to unit" messages.
	%%%
	%\override SpacingSpanner #'shortest-duration-space = #0
	%\override SpacingSpanner #'average-spacing-wishes = ##f
	%\override SpacingSpanner #'spacing-increment = #0.0
	%\override SpacingSpanner #'uniform-stretching = ##t
    }
}

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
	\override NoteHead #'style = #'vaticana.punctum
	\override Stem #'transparent = ##t
    }
}
