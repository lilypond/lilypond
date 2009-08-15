\version "2.12.0"
\include "example-header.ily"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is a brief passage from Enrique Granados %
% Goyescas, "Coloquio en la Reja."              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(set-global-staff-size 18)

% EXAMPLE BROKEN; uncomment when fixed.
%{
\paper {
  ragged-right = ##f
  #(set-paper-size "letter" 'landscape)
  system-count = 1
}
%}

csh = \change Staff = "high" 
csm = \change Staff = "middle"
csl = \change Staff = "low"

crmolto = \markup { \italic \large "cresc. molto" }
appassmolto = \markup { \italic \large "appassionato molto" }

#(ly:set-option 'point-and-click #f)

global = {
  \time 3/4
  \set fontSize = #1
  \override Beam #'thickness = #0.5
  \override NoteColumn #'ignore-collision = ##t
}

upperVoiceOne = \relative c'' {
  \voiceOne
  \override TupletBracket #'transparent = ##t
  <aes f'>8\([ \times 4/5{g'32( aes g f g]) } 
    <es aes>8[ \times 4/5{<d bes'>32( c' bes aes bes]) }
    <es, aes es'>8 <d fis b d>\) | % end m. 1
  %--------------------------------------------------%
  <c g' c>4\( \voiceTwo <bes es bes'\arpeggio>\) 
    \slurUp <aes aes'>16( <bes bes'> <g g'>8) % end m. 2
  %--------------------------------------------------%
  \voiceOne 
  <f aes d f>8\([ \times 4/5{<g g'>32( aes' g f g]) }
  \set subdivideBeams = ##t
  \set beatLength = #(ly:make-moment 1 8)
    <aes, aes'>16 <c f> \times 4/5{ bes'32( c bes aes bes]) }
    \set subdivideBeams = ##f
    \ottava #1 <es es'>16 <f f'> <d d'> \appoggiatura f8 <es es'>16\)
}

upperVoiceTwo = \relative c'' {
  \voiceTwo
  s8 c8\< <bes, f'>[ <bes aes'> c' <bes, d fis>\!]
  s32 s32_\appassmolto s8. \voiceOne r8 <bes'' es bes'>-> s4 
  \override Stem #'cross-staff = ##t
  \override Stem #'length = #28
  \override Stem #'flag-style = #'no-flag
  s8 \voiceTwo g,8 aes4 s4
}

middleVoiceOne = \relative c' {
  \override Stem #'cross-staff = ##t
  \override Stem #'length = #32
  \override Stem #'flag-style = #'no-flag
  d!8\noBeam s8 s8 s8_\crmolto s4  % 1
  s4 <g bes\arpeggio>8[ <es' g>] \voiceOne e,8( dis16 e) | % 2
  \revert Stem #'length
  \revert Stem #'cross-staff
  \showStaffSwitch
  \csh \stemUp f4 s2
  %s2.  % beginning m. 3
}

middleVoiceTwo = \relative c' {
  s2. | % 1
  \override Stem #'cross-staff = ##t
  \override Stem #'length = #24
  \override Stem #'flag-style = #'no-flag
  s2 \voiceTwo e!4 | % 2
  s4 \voiceTwo <bes c es f>8 <f' aes es'>16 d' <bes, f' aes c>8 <bes' fis'> | % 3
}

lowerVoiceOne = \relative c, {
  \override Staff.NoteCollision #'merge-differently-headed = ##t
  \override Staff.NoteCollision #'merge-differently-dotted = ##t
  bes8 \csm \stemDown <bes'' c es>8 s2
  \csl \stemUp
  \set subdivideBeams = ##t
  \set beatLength = #(ly:make-moment 1 16)
  s8 \hideNotes \slurUp \stemDown 
    es,,64( bes'' s64 \unHideNotes \stemUp g64[ bes c d c]) s2
  \set subdivideBeams = ##f
  bes,,8 \csm \stemDown <bes'' c es>8 s2
}

lowerVoiceTwo = \relative c, {
  \voiceTwo
  bes2. 
  \csh
  \once \override Beam #'damping = #+inf.0
  <bes'' es g>8 \csl \slurUp 
  %\once\override Slur #'extra-offset = #'(0 . 4) 
  es,,64 bes' es g s32. 
    c64
    s4 <bes des>
  bes,,2. 
}


\score {
  \new Staff { c'4^"EXAMPLE BROKEN" }
%  uncomment the below once it works.
%{
  \new PianoStaff <<
    \set PianoStaff.connectArpeggios = ##t
%    \override PianoStaff.Arpeggio #'stencil = #ly:arpeggio::brew-chord-bracket
    \new Staff = "high" << 	
      \global
      \context Voice = "upperVoiceOne" { \upperVoiceOne }
      \context Voice = "upperVoiceTwo" { \upperVoiceTwo }
    >>
    \new Staff = "middle" << 
      \global
      \context Voice = "middleVoiceOne" { \middleVoiceOne }
      \context Voice = "middleVoiceTwo" { \middleVoiceTwo }
    >>
    \new Staff = "low" << 	
      \clef bass 
      \global
      \context Voice = "lowerVoiceOne" { \lowerVoiceOne }
      \context Voice = "lowerVoiceTwo" { \lowerVoiceTwo }
    >>
  >>
  \layout {
    \context {
      \Score
      \override TimeSignature #'stencil = ##f
      \remove "Bar_number_engraver"
    }
  }
%}
  %{\midi {
    \context {
      \Score
      tempoWholesPerMinute = #(ly:make-moment 120 4)
    }
  }%}
}
