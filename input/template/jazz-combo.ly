\version "2.3.2"
\header {
        title = "Song"
        subtitle = "(tune)"
        composer = "Me"
        meter = "moderato"
        piece = "Swing"
	tagline = "LilyPond example file by Amelie Zapf, Berlin 07/07/2003"
	texidoc = "Jazz tune for combo (horns, guitar, piano, bass, drums)."
}

#(set-global-staff-size 16)
\include "english.ly"

%%%%%%%%%%%% Some macros %%%%%%%%%%%%%%%%%%%

sl = {
    \override NoteHead  #'style = #'slash
    \override Stem  #'transparent = ##t
}
nsl = {
    \revert NoteHead #'style
    \revert Stem #'transparent
}
cr = \override NoteHead  #'style = #'cross
ncr = \revert NoteHead #'style

%% insert chord name style stuff here.

jzchords = { } 


%%%%%%%%%%%% Keys'n'thangs %%%%%%%%%%%%%%%%%

global = \notes {
    \time 4/4
}

Key = \notes { \key c \major }

% ############ Horns ############
% ------ Trumpet ------
trpt = \notes \transpose c d \relative c'' {
    \Key
    c1 c c
}

trpharmony = \transpose c' d { \jzchords }
trumpet = {
    \global 
    \set Staff.instrument = #"Trumpet"
    \clef treble
    \context Staff <<
	\trpt
    >>
}

% ------ Alto Saxophone ------
alto = \transpose c a \relative c' {
	\Key
        c1 c c
}

altoharmony = \transpose c' a { \jzchords }
altosax = {
        \global
        \set Staff.instrument = #"Alto Sax"
        \clef treble
        \context Staff <<
                \alto
        >>
}

% ------ Baritone Saxophone ------
bari = \transpose c a' \relative c {
	\Key
        c1 c \sl d4^"Solo" d d d \nsl
}

bariharmony = \transpose c' a \chords { \jzchords s1 s d2:maj e:m7 }
barisax = {
        \global
        \set Staff.instrument = #"Bari Sax"
        \clef treble
        \context Staff <<
                \bari
        >>
}
% ------ Trombone ------
tbone = \notes \relative c {
	\Key
        c1 c c
}

tboneharmony = \chords { \jzchords }
trombone = {
        \global
        \set Staff.instrument = #"Trombone"
        \clef bass
        \context Staff <<
                \tbone
        >>
}
% ############ Rhythm Section #############
% ------ Guitar ------
gtr = \notes \relative c'' {
	\Key
        c1 \sl b4 b b b \nsl c1
}

gtrharmony = \chords { \jzchords
        s1 c2:min7+ d2:maj9
}

guitar = {
        \global
        \set Staff.instrument = #"Guitar"
        \clef treble
        \context Staff <<
        	\gtr
        >>
}

%% ------ Piano ------
rhUpper = \notes \relative c'' {
        \voiceOne
	\Key
        c1 c c
}

rhLower = \notes \relative c' {
        \voiceTwo
	\Key
        e1 e e
}

lhUpper = \notes \relative c' {
        \voiceOne
	\Key
        g1 g g
}

lhLower = \notes \relative c {
        \voiceTwo
	\Key
        c1 c c
}

PianoRH = {
        \clef treble
        \global
        \set Staff.midiInstrument = "acoustic grand"
        \context Staff <<
                \context Voice = one \rhUpper
        	\context Voice = two \rhLower
        >>
}

PianoLH = {
        \clef bass
        \global
        \set Staff.midiInstrument = "acoustic grand"
        \context Staff <<
        	\context Voice = one \lhUpper
        	\context Voice = two \lhLower
        >>
}

piano = {
	\context PianoStaff <<
		\set PianoStaff.instrument = #"Piano"
        	\context Staff = upper \PianoRH
        	\context Staff = lower \PianoLH
        >>
}

% ------ Bass Guitar ------
bass = \notes \relative c {
	\Key
        c1 c c
}

bass = {
    \global
    \set Staff.instrument = #"Bass"
    \clef bass
    \context Staff <<
	\bass
    >>
}

				% ------ Drums ------

up = \drums {
    hh4 <hh sn>4 hh <hh sn> hh <hh sn>4
    hh4 <hh sn>4
    hh4 <hh sn>4
    hh4 <hh sn>4
}

down = \drums {
    bd4 s bd s bd s bd s bd s bd s
}

drumContents = {
	\global
	<<
		\set DrumStaff.instrument = #"Drums"
		\new DrumVoice { \voiceOne \up }
		\new DrumVoice { \voiceTwo \down }
	>>
}

%%%%%%%%% It All Goes Together Here %%%%%%%%%%%%%%%%%%%%%%

\score {
<<
        \context StaffGroup = horns <<
                \context Staff = trumpet \trumpet

                \context Staff = altosax \altosax

		\context ChordNames = barichords \bariharmony

                \context Staff = barisax \barisax

                \context Staff = trombone \trombone
        >>

        \context StaffGroup = rhythm <<
		\context ChordNames = chords \gtrharmony
                \context Staff = guitar \guitar
		\context PianoStaff = piano \piano
                
                \context Staff = bass \bass
                
                \new DrumStaff { \drumContents }
        >>
>>
        \paper {
                linewidth = 15.0 \cm
                \context { \RemoveEmptyStaffContext }
                \context {
                        \Score
                        \override BarNumber #'padding = #3
                        \override RehearsalMark #'padding = #2
                        skipBars = ##t
                }
        }
        \midi { \tempo 4 = 75 }
}
