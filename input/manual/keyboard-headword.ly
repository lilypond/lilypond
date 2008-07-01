\version "2.11.49"
\include "english.ly"

#(set-global-staff-size 15)
\paper{
  ragged-right=##t
  line-width=17\cm
  indent=0\cm
}

\new PianoStaff <<
  \set PianoStaff.connectArpeggios = ##t
  \new Staff {
     \time 2/4
     \key fs \major
     <<
        \new Voice {
           \voiceOne
           fs''8 (
           ^ \markup \column {
              \line \bold { Un peu retenu  }
              \line \italic { très expressif } }
           es''16
           cs''16
           as'4 )
           |
           fs''8 (
           es''16
           cs''16
           as'4 )
           |
           fs''8 (
           es''16
           cs''16
           as'8
           cs''8 )
           |
        }
        \new Voice {
           \voiceTwo
           r8 \ppp
           fs'4 (
           es'8 )
           |
           r8
           fs'4 (
           es'8 )
           |
           r8
           fs'4 (
           es'8 )
           |
        }
    >>
     \clef bass
     <ds b! es'>4 (
     ^ \markup \bold { Rall. }
     \once \override Script #'padding = #2
     <ds' as'>8 ) \fermata
     \noBeam
     \clef treble
     \slurUp
     <as fs'>8 ( \pp
     |
     <gs b cs'>4. )
     ^ \markup \bold { a tempo }
     \slurUp
     <as fs'>8 (
     |
     <gs b cs'>4. )
     <<
        \new Voice {
           \voiceOne
           <as fs'>8 (
           ^ \markup \bold { Rallentando }
           |
           cs'8
           b16
           cs'16
           d'8
           e'16
           fs'16
           |
           <as! cs' gs'>4. )
           s8
           |
           r8
           <cs'' as'' cs'''>4 \arpeggio
           e''16 (
           ^ \markup \bold { Lent }
           fs''16
           |
           \stemDown
           <as'! cs'' gs''>2 )
           |
        }
      \new Voice {
           \voiceTwo
           s8
           |
           <gs b>4 \<
           <fs bs>4 \>
           |
           s4. \!
           \slurUp
           \once \override Script #'direction = #up
           <a bs e'>8 ( \accent
           |
           <as! cs' gs'>4. )
           <a' bs'>8 \ppp \>
           |
           s8 \!
           \stemDown
           \once \override Script #'direction = #up
           #(set-octavation 1)
           % \once \override Arpeggio #'extra-offset = #'(-0.25 .
           % 0)
           <cs''' as''' cs''''>4. \arpeggio \fermata
           #(set-octavation 0)
           \bar "|."
        }
     >>
  }
  \new Staff <<
     \set Staff.pedalSustainStyle = #'bracket
     \key fs \major
     \clef bass
     \new Voice {
        \voiceOne
        ds'4 \tenuto
        cs'4 \tenuto
        |
        ds'4 \tenuto
        cs'4 \tenuto
        |
        ds'4 \tenuto
        cs'4 \tenuto
        |
        s8
        \clef treble
        <b' cs''>8 [
        \clef bass
        <es b cs'>8 \fermata ]
        s8
        |
        r8
        \clef treble
        <b' cs''>4 \tenuto
        s8
        |
        r8
        \clef treble
        <b' cs''>4 \tenuto
        s8
        |
        s2
        |
        r8
        \clef treble
        <as' cs''>4
        \clef bass
        s8
        |
        s8
        \clef treble
        <as'>4 \arpeggio
        \clef bass
        s8
        |
        s8
        \clef treble
        <as''>4. \arpeggio \fermata
        |
     }
     \new Voice {
        \voiceTwo
        ds'8 [ (
        < ds bs >8
        cs'8
        < ds as >8 ] )
        |
        ds'8 [ (
        < ds bs >8
        cs'8
        < ds as >8 ] )
        |
        ds'8 [ (
        < ds bs >8
        cs'8
        < ds as >8 ] )
        |
        \once \override Script #'outside-staff-priority = #100
        \once \override TextScript #'outside-staff-priority = #500
        <cs, gs,>4. \fermata
        _ \markup \italic { ped. }
        <fs, cs>8 (
        |
        <e, b,>4. ) \sustainDown
        \clef bass
        <fs, cs>8 ( \sustainUp
        |
        <e, b,>4. ) \sustainDown
        \clef bass
        <fs, cs>8 ( \sustainUp
        |
        <e, b,>4
        <d, a,>4
        |
        <fs,, cs,>4. ) \sustainDown
        <a, e>8 ( \sustainUp
        |
        <fs, cs>4. ) \sustainDown
        \slurUp
        <a e'>8 ( \sustainUp \sustainDown
        |
        <fs cs'>2 ) \sustainUp \sustainDown
        |
     }
  >>
>>

