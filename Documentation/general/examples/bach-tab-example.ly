\version "2.11.62"
\include "example-header.ily"

% Example of Tab Polyphony from Bach Lute Suite in E Minor



%#(define (fret-letter-tablature-format string tuning pitch)
%(make-string 1 (integer->char
%(+ (char->integer #\a)
%(- (pitch-semitones pitch)
%(list-ref tuning (- string 1)))))))

global = {
  \time 12/8
  \key e \minor
  \set Staff.midiInstrument = "acoustic guitar (nylon)"
%  \set TabStaff.tablatureFormat = #fret-letter-tablature-format
}

upper = \relative c' {
	\global
	\voiceOne
	r4. r8 e, fis g16 b g e e' b c b a g fis e
}

lower = \relative c {
	\global
	\voiceTwo
	r16 e d c b a g4 fis8 e fis g a b c | 
}

\score {
  <<
    \new StaffGroup = "tab with traditional" <<
       \new Staff = "guitar" <<
	    \clef "treble_8"
	    \context Voice = "upper" \upper
	    \context Voice = "lower" \lower
       >>
       \new TabStaff = "guitar" << 
            \context TabVoice = "upper"  \upper 
            \context TabVoice = "lower"  \lower 
       >>
    >>
  >>
  \layout {}
  %\midi {}
}

