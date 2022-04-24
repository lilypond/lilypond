\version "2.23.6"

\header {
  tagline = ##f
  title = "Violent Dance For Orchestra"
  composer = "Hu Haipeng"
%  arranger = "July 5, 2009"

%  poet = "  I'm writing this piece because I'm terribly frustrated, facing a task which will seriously stain my aesthetics and conviction to the true art. It consists of all kinds of devils, dancing and whirling violently, turning the world into an abyss of darkness. Although the main melodies are derived from folk music, these are only a beautiful skin, and the essence of this piece is violent and evil, full of my 10 years' pain and rage. It's a large volcano of my long repressed heart!"
}

\paper{
  line-width = 158\mm
}

%% text defs
presto = \markup { \bold \italic "Presto" }
div = \markup { \bold "Div." }
nondiv = \markup { \bold "Non div." }
unis = \markup { \bold "Unis." }
piz = \markup { \bold "Pizz." }
arc = \markup { \bold "Arco" }
pizz = \set Staff.midiInstrument = "pizzicato strings"
arco = \set Staff.midiInstrument = "string ensemble 1"
pont = \markup { \bold \italic "Sul ponticello" }
naturale = \markup { \bold \italic "Naturale" }
moltocr = {
  \set crescendoText = \markup { \italic "Molto cresc." }
  \set crescendoSpanner = #'text
  \override DynamicTextSpanner.style = #'dotted-line
}
offCr = {
  \unset crescendoText
  \unset crescendoSpanner
  \revert DynamicTextSpanner.style
}

%% Layout for piano dynamics
\layout {
  \context {
    \Voice
    \override Glissando.breakable = ##t
    \override TextSpanner.breakable = ##t
    \override DynamicLineSpanner.breakable = ##t
    \override DynamicTextSpanner.breakable = ##t
    \override TrillSpanner.breakable = ##t
  }
}

%% layout to create orchestra staff group
%% with non-spanned barlines between two instrument groups
\layout {
  \context {
    \StaffGroup
    \name Orchestra
    \remove Span_bar_engraver
  }
  \context {
    \Score
    \accepts Orchestra
  }
}

%% Layout to produce SquareStaff context
%% to group similar instruments in a staff group with thin square bracket
\layout {
  \context {
    \StaffGroup
    \name SquareStaff
    systemStartDelimiter = #'SystemStartSquare
  }
  \context {
    \Orchestra
    \accepts SquareStaff
  }
  \context {
    \StaffGroup
    \accepts SquareStaff
  }
}

%% Layout to produce MarkLine context
%% to place rehearsal marks and texts above full score
\layout {
  \context {
    \type Engraver_group
    \name MarkLine
    \consists Output_property_engraver
    \consists Axis_group_engraver
    \consists Mark_engraver
    \consists Metronome_mark_engraver
    \consists Script_engraver
    \consists Text_engraver
    \consists Text_spanner_engraver
    \consists Font_size_engraver
    \override VerticalAxisGroup.staff-affinity = #DOWN
    \override VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding = #2
    \override VerticalAxisGroup.nonstaff-unrelatedstaff-spacing.padding = #5
    \override TextSpanner.breakable = ##t
  }
  \context {
    \Score
    \accepts MarkLine
  }
  \context {
    \Orchestra
    \accepts MarkLine
  }
  \context {
    \StaffGroup
    \accepts MarkLine
  }
}

%% layout to produce a smaller markline
%% put before 1st violin part
\layout {
  \context {
    \MarkLine
    \name SmallMarkLine
    \override MetronomeMark.outside-staff-priority = #800
    \override RehearsalMark.outside-staff-priority = #1200
  }
  \context {
    \Score
    \accepts SmallMarkLine
  }
  \context {
    \Orchestra
    \accepts SmallMarkLine
  }
  \context {
    \StaffGroup
    \accepts SmallMarkLine
  }
}

modern =
#`(Staff ,(make-accidental-rule 'same-octave 0)
  ,(make-accidental-rule 'any-octave 0)
  ,(make-accidental-rule 'same-octave 1))

\layout {
  \context {
    \Score
    autoAccidentals = #modern
    autoCautionaries = #modern
  }
}

  marks = \relative c' {
    \set rehearsalMarkFormatter = #format-mark-box-numbers
    \tempo \presto 4.=112
\set Score.currentBarNumber = #11
    s2.*4 |
    s1*9/8 |
  }

  piccolo = \relative c'''' {
    \clef treble \key ees \minor \time 6/8
    \transposition c''
    R2.
    ges,16(\mf\< ees c ees ges bes) c( bes ges bes c ees) |
    ges8-.->\!\ff \offCr r r r4 r8 | R2. |
    \time 9/8
    R1*9/8 |
  }

  flutes = \relative {
    \clef treble \key ees \minor \time 6/8
    R2.
    <ges'' c,>16(\mf\< <ees bes> <c ges> <ees bes> <ges c,> <bes ees,>) <c ges>( <bes ees,> <ges c,> <bes ees,> <c ges> <ees bes>) |
    <ges c,>8-.->\!\ff \offCr r r r4 r8 | R2. |
    \time 9/8
    R1*9/8 |
  }

  oboes = \relative {
    \clef treble \key ees \minor \time 6/8
    R2. |
    <ges' c,>4(\mf\< <bes ees,>8 <c ges>4 <ees bes>8) |
    <ges c,>-.->\!\ff \offCr r r r4 r8 | R2. |
    \time 9/8
    R1*9/8 |
  }

  clarinets = \relative c' {
    \clef treble \key f \minor \time 6/8
    \transposition bes
    <aes' d,>4(\p\< <c f,>8) <d aes>4( <f c>8) |
    <aes d,>4( <c f,>8) <d aes>4( <f c>8) |
    <aes d,>-.->\!\ff \offCr r r r4 r8 | R2. |
    \time 9/8
    R1*9/8 |
  }

  bassoons = \relative {
    \clef bass \key ees \minor \time 6/8
    <ees, bes'>4.\pp\< c'^"a2" |
    bes8-. bes-. bes-. ges-. ges-. ges-. |
    ees-.->\!\ff \offCr <ees bes'>4\pp ~ <ees bes'>4. ~ | <ees bes'>2. |
    \time 9/8
    ges4\p^"I" aes8 aes ees ges ges4 aes16( ges) |
  }

  hornI = \relative c'' {
    \clef treble \key bes \minor \time 6/8
    \transposition f
    r4 r8 <f bes,>4.\p\< ~ |
    <f bes,>8-. <f bes,>-. <f bes,>-. <f bes,>-. <f bes,>-. <f bes,>-. |
    <f bes,>-.->\!\ff \offCr r r r4 r8 | R2. |
    \time 9/8
    r4 r8 <b, g>2.\pp |
  }

  hornII = \relative c'' {
    \clef treble \key bes \minor \time 6/8
    \transposition f
    \moltocr <des g,>2.\pp\< ~ |
    <des g,>8-. <f bes,>-. <f bes,>-. <f bes,>-. <f bes,>-. <f bes,>-. |
    <f bes,>-.->\!\ff \offCr r r r4 r8 | R2. |
    \time 9/8
    <f, des>2.\pp <f des>4. ~ |
  }

  trumpetI = \relative c''' {
    \clef treble \key f \minor \time 6/8
    \transposition bes
R2. |
    r4 r8 <aes f>-.\f\< <aes f>-. <aes f>-. |
    <c aes>-.->\!\ff r r r4 r8 | R2. |
    \time 9/8
    R1*9/8 |
  }

  trumpetII = \relative c'' {
    \clef treble \key f \minor \time 6/8
    \transposition bes
R2. |
    r8 d-.\mf\< d-. d-. d-. d-. |
    d-.->\!\ff \offCr r r r4 r8 | R2. |
    \time 9/8
    R1*9/8 |
  }

  trombones = \relative {
    \clef tenor \key ees \minor \time 6/8
    r4 r8 <ges c>4.\mp\< ~ |
    <ges c>8-. <ges c>-. <ges c>-. <ges c>-. <ges c>-. <ges c>-. |
    <ges c>-.->\!\ff \offCr r r r4 r8 | R2. |
    \time 9/8
    R1*9/8 |
  }

  tuba = \relative {
    \clef bass \key ees \minor \time 6/8
    <ees, ees'>4.(\pp\< <c c'> |
    <bes bes'>8-.) <bes bes'>-. <bes bes'>-. <ges ges'>-. <ges ges'>-. <ges ges'>-. |
    <ees ees'>-.->\!\ff \offCr r r r4 r8 | R2. |
    \time 9/8
    R1*9/8 |
  }

  timpani = \relative {
    \clef bass \key ees \minor \time 6/8
    ees8\< ees ees ees ees ees |
    bes bes bes bes bes bes |
    ees,->\!\f \offCr ees'\pp ees ees ees ees |
    ees ees ees ees ees ees |
    \time 9/8
    ees r r r4 r8 r4 r8 |
  }

  trian = {
    \clef percussion \time 6/8
    R2.*4 |
    \time 9/8
    R1*9/8 |
  }

  cym = {
    \clef percussion \time 6/8
    R2.*4 |
    \time 9/8
    R1*9/8 |
  }

  tamt = {
    \clef percussion \time 6/8
R2. |
    r4 r8 r c4\mf\<^"*" ~ |
    8\!\ff r r r4 r8 | R2. |
    \time 9/8
    R1*9/8 |
  }

  tamb = {
    \clef percussion \time 6/8
    R2.*4 |
    \time 9/8
    R1*9/8 |
  }

  snare = {
    \clef percussion \time 6/8
    R2.*4 |
    \time 9/8
    c8\pp 8 8 8 8 8 8 8 8 |
  }

  bsdrum = {
    \clef percussion \time 6/8
    c2.:32\pp\< ~ | 2.: ~ |
    8\!\ff \offCr r r r4 r8 | R2. |
    \time 9/8
    2.:32\pp ~ 4.: |
  }

  harprh = \relative c'' {
    \clef treble \key ees \minor \time 6/8
    \showStaffSwitch
    R2.*4_\markup { \harp-pedal "--^|^^^^" } |
    \time 9/8
    R1*9/8 |
  }

  harplh = \relative c {
    \clef bass \key ees \minor
    \showStaffSwitch
    R2.*4 |
    R1*9/8 |
  }

  dynamics = {
    s2.*4 |
    s1*9/8 |
  }


  violinI = \relative {
    \clef treble \key ees \minor \time 6/8
    ges'16(\pp\< ees c ees ges bes) c( bes ges bes c ees) |
    ges( ees c ees ges bes) c( bes ges bes c ees) |
    ges8-.->\!\ff \offCr r r r4 r8 | R2. |
    \time 9/8
    R1*9/8 |
  }

  violinII = \relative {
    \clef treble \key ees \minor \time 6/8
    c'16(\pp\< bes ges bes c ees) ges( ees c ees ges bes) |
    c( bes ges bes c ees) ges( ees c ees ges bes) |
    c8-.->\!\ff \offCr r r r4 r8 | R2. |
    \time 9/8
    r4 r8 \repeat tremolo 6 { c,,32->\pp^\pont e } r4 r8 |
  }

  viola = \relative {
    \clef alto \key ees \minor \time 6/8
    <ees bes'>8-.\pp\< <ees bes'>-. <ees bes'>-. <ges c>-. <ges c>-. <ges c>-. |
    <bes ees>-. <bes ees>-. <bes ees>-. <c ges'>-. <c ges'>-. <c ges'>-. |
    <ees bes'>-.->\!\ff \offCr r r r4 r8 | R2. |
    \time 9/8
    \repeat tremolo 12 { ges,32->^\pont bes } \repeat tremolo 6 {ges->( bes) } |
  }

  cello = \relative {
    \clef bass \key ees \minor \time 6/8
    <c ges'>8-.\pp\< <c ges'>-. <c ges'>-. <bes ees>-. <bes ees>-. <bes ees>-. |
    <c ges'>-. <c ges'>-. <c ges'>-. <c ges'>-. <c ges'>-. <c ges'>-. |
    <bes ges'>8-.->\!\ff \offCr <c ges'>-.\pp <c ges'>-. <c ges'>-. <c ges'>-. <c ges'>-. |
    <c ges'>-. r r r4 r8 |
    \time 9/8
    \repeat tremolo 12 <c ges'>32(^\pont \repeat tremolo 12 <cis g'> \repeat tremolo 12 <c ges'>) |
  }

  contrabass = \relative c {
    \clef bass \key ees \minor \time 6/8
    \transposition c
    <ees bes'>8-.\pp\< ees-. ees-. c-. c-. c-. |
    bes-. bes-. ges-. ges-. ges-. ges-. |
    ees-.->\!\ff \offCr <ees' bes'>-.\pp <ees bes'>-. <ees bes'>-. <ees bes'>-. <ees bes'>-. |
    <ees bes'>-. <ees bes'>-. <ees bes'>-. <ees bes'>-. <ees bes'>-. <ees bes'>-. |
    \time 9/8
    <ees bes'>-. <ees bes'>-. <ees bes'>-. <ees bes'>-. <ees bes'>-. <ees bes'>-. <ees bes'>-. <ees bes'>-. <ees bes'>-. |
  }

  #(set-global-staff-size 10)

  \score {
    \new Orchestra = "orchestra" <<
    \set Score.skipBars = ##f
    \new MarkLine { \marks }
      \new StaffGroup = "woodwind" <<
      \new SquareStaff = "picc fl" <<
        \new Staff = "piccolo" {
          \set Staff.instrumentName = "Piccolo"
          \set Staff.shortInstrumentName = "Picc."
        \piccolo
        }
        \new Staff = "flutes" {
          \set Staff.instrumentName = "Flutes I & II"
          \set Staff.shortInstrumentName = "Fl."
        \flutes
        }
      >>
      \new Staff = "oboes" {
        \set Staff.instrumentName = "Oboes I & II"
        \set Staff.shortInstrumentName = "Ob."
      \oboes
      }
      \new Staff = "clarinets" {
        \set Staff.instrumentName = \markup {
          \column { \line { "Clarinets I & II" }
            \line { "in B" \smaller \flat } } }
        \set Staff.shortInstrumentName = "Cl."
      \clarinets
      }
      \new Staff = "bassoons" {
        \set Staff.instrumentName = "Bassoons I & II"
        \set Staff.shortInstrumentName = "Bn."
      \bassoons
      }
    >>
    \new StaffGroup = "brass" <<
      \new SquareStaff = "horns" <<
        \new Staff = "hornsI" {
          \set Staff.instrumentName = \markup {
            \column { \line { "Horns I & II" }
              \line { "in F" } } }
          \set Staff.shortInstrumentName = "Hn. I & II"
        \hornI
        }
        \new Staff = "hornsII" {
          \set Staff.instrumentName = \markup {
            \column { \line { "Horns III & IV" }
              \line { "in F" } } }
          \set Staff.shortInstrumentName = "Hn. III & IV"
        \hornII
        }
      >>
      \new SquareStaff = "trumpets" <<
        \new Staff = "trumpetI" {
          \set Staff.instrumentName = \markup {
            \column { \line { "Trumpets I & II" }
            \line { "in B" \smaller \flat } } }
          \set Staff.shortInstrumentName = "Tp. I & II"
        \trumpetI
        }
        \new Staff = "trumpetII" {
          \set Staff.instrumentName = \markup {
            \column { \line { "Trumpet III" }
            \line { "in B" \smaller \flat } } }
          \set Staff.shortInstrumentName = "Tp. III"
        \trumpetII
        }
      >>
      \new SquareStaff = "trombones" <<
        \new Staff = "trombones 1 & 2" {
          \set Staff.instrumentName = "Trombones I & II"
          \set Staff.shortInstrumentName = "Tb. I & II"
        \trombones
        }
        \new Staff = "tuba" {
          \set Staff.instrumentName = "Bass trombone & Tuba"
          \set Staff.shortInstrumentName = "Btb. & Tu."
        \tuba
        }
      >>
    >>
    \new Staff = "timpani" {
      \set Staff.instrumentName = "Timpani in A, D & E"
      \set Staff.shortInstrumentName = "Tim."
    \timpani
    }
    \new GrandStaff = "drums" <<
      \new RhythmicStaff = "triangle" {
        \set RhythmicStaff.instrumentName = "Triangle"
        \set RhythmicStaff.shortInstrumentName = "Tri."
      \trian
      }
      \new RhythmicStaff = "cymbals" {
        \set RhythmicStaff.instrumentName = "Suspended cymbal"
        \set RhythmicStaff.shortInstrumentName = "Susp. cym."
      \cym
      }
      \new RhythmicStaff = "tamtam" {
        \set RhythmicStaff.instrumentName = "Tamtam"
        \set RhythmicStaff.shortInstrumentName = "Tamt."
      \tamt
      }
      \new RhythmicStaff = "tambourine" {
        \set RhythmicStaff.instrumentName = "Tambourine"
        \set RhythmicStaff.shortInstrumentName = "Tamb."
      \tamb
      }
      \new RhythmicStaff = "snare" {
        \set RhythmicStaff.instrumentName = "Snare drum"
        \set RhythmicStaff.shortInstrumentName = "Sn."
      \snare
      }
      \new RhythmicStaff = "bass drum" {
        \set RhythmicStaff.instrumentName = "Bass drum"
        \set RhythmicStaff.shortInstrumentName = "Bd."
      \bsdrum
      }
    >>
    \new PianoStaff = "harp" <<
      \set PianoStaff.instrumentName = "Harp"
      \set PianoStaff.shortInstrumentName = "Hrp."
      \set PianoStaff.connectArpeggios = ##t
      \new Staff = "rh" { \harprh }
      \new Dynamics { \dynamics }
      \new Staff = "lh" { \harplh }
    >>
    \new SmallMarkLine { \marks }
    \new StaffGroup = "strings" <<
      \new SquareStaff = "violins" <<
        \new Staff = "violin I" {
          \set Staff.instrumentName = "Violin I"
          \set Staff.shortInstrumentName = "Vn. I"
        \violinI
        }
        \new Staff = "violin II" {
          \set Staff.instrumentName = "Violin II"
          \set Staff.shortInstrumentName = "Vn. II"
        \violinII
        }
      >>
      \new Staff = "viola" {
        \set Staff.instrumentName = "Viola"
        \set Staff.shortInstrumentName = "Vl."
      \viola
      }
      \new SquareStaff = "Cello and Bass" <<
        \new Staff = "violoncello" {
          \set Staff.instrumentName = "Violoncello"
          \set Staff.shortInstrumentName = "Vc."
        \cello
        }
        \new Staff = "contrabass" {
          \set Staff.instrumentName = "Contrabass"
          \set Staff.shortInstrumentName = "Cb."
        \contrabass
        }
        >>
      >>
    >>
    \layout {
      \context {
        \Score
        \remove Mark_engraver
        \remove Metronome_mark_engraver
      }
      \context {
        \Staff \RemoveEmptyStaves
      }
    }
  }
