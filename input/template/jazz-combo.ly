\version "2.1.19"
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
    \property Voice.NoteHead \override #'style = #'slash
    \property Voice.Stem \override #'transparent = ##t
}
nsl = {
    \property Voice.NoteHead \revert #'style
    \property Voice.Stem \revert #'transparent
}
cr = \property Voice.NoteHead \override #'style = #'cross
ncr = \property Voice.NoteHead \revert #'style

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

trpharmony = \chords \transpose c' d { \jzchords }
trumpet = {
    \global 
    \property Staff.instrument = #"Trumpet"
    \clef treble
    \context Staff <<
	\trpt
    >>
}

% ------ Alto Saxophone ------
alto = \notes \transpose c a \relative c' {
	\Key
        c1 c c
}

altoharmony = \chords \transpose c' a { \jzchords }
altosax = {
        \global
        \property Staff.instrument = #"Alto Sax"
        \clef treble
        \context Staff <<
                \alto
        >>
}

% ------ Baritone Saxophone ------
bari = \notes \transpose c a' \relative c {
	\Key
        c1 c \sl d4^"Solo" d d d \nsl
}

bariharmony = \chords \transpose c' a { \jzchords s1 s d2:maj e:m7 }
barisax = {
        \global
        \property Staff.instrument = #"Bari Sax"
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
        \property Staff.instrument = #"Trombone"
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
        \property Staff.instrument = #"Guitar"
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
        \property Staff.midiInstrument = "acoustic grand"
        \context Staff <<
                \context Voice = one \rhUpper
        	\context Voice = two \rhLower
        >>
}

PianoLH = {
        \clef bass
        \global
        \property Staff.midiInstrument = "acoustic grand"
        \context Staff <<
        	\context Voice = one \lhUpper
        	\context Voice = two \lhLower
        >>
}

piano = {
	\context PianoStaff <<
		\property PianoStaff.instrument = #"Piano"
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
    \property Staff.instrument = #"Bass"
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
		\property DrumStaff.instrument = #"Drums"
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
                \translator { \RemoveEmptyStaffContext }
                \translator {
                        \ScoreContext
                        BarNumber \override #'padding = #3
                        RehearsalMark \override #'padding = #2
                        skipBars = ##t
                }
        }
        \midi { \tempo 4 = 75 }
}

