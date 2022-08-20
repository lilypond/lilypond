\version "2.23.3"

\header {
  lsrtags = "ancient-notation, symbols-and-glyphs"

  texidoc = "
Shown here are many of the symbols that are included in LilyPond's
ancient notation.
"

  doctitle = "Ancient fonts"
}


upperStaff = \new VaticanaStaff = "upperStaff" <<
  \context VaticanaVoice <<
    \transpose c c {

      \override NoteHead.style = #'vaticana.punctum
      \key es \major
      \clef "vaticana-fa2"
      c1 des e f ges

      \override NoteHead.style = #'vaticana.inclinatum
      a! b ces'
      \bar "|"

      \override NoteHead.style = #'vaticana.quilisma
      b! des'! ges! fes!
      \breathe
      \clef "vaticana-fa1"
      \override NoteHead.style = #'vaticana.plica
      es d
      \override NoteHead.style = #'vaticana.reverse.plica
      c d
      \bar "|"

      \override NoteHead.style = #'vaticana.punctum.cavum
      es f
      \override NoteHead.style = #'vaticana.lpes
      g as
      \override NoteHead.style = #'vaticana.upes
      bes as
      \override NoteHead.style = #'vaticana.vupes
      g f
      \override NoteHead.style = #'vaticana.linea.punctum
      \once \override Staff.BarLine.bar-extent = #'(-1 . 1) \bar "|"

      es d
      \override NoteHead.style = #'vaticana.epiphonus
      c d
      \override NoteHead.style = #'vaticana.cephalicus
      es f

      \set Staff.alterationGlyphs =
        #alteration-medicaea-glyph-name-alist
      \override Staff.Custos.style = #'medicaea
      \override NoteHead.style = #'medicaea.punctum
      \clef "medicaea-fa2"
      ces des
      \bar "|"

      e! f! ges
      \clef "medicaea-do2"
      \override NoteHead.style = #'medicaea.inclinatum
      a! b! ces'
      \override NoteHead.style = #'medicaea.virga
      b! a!
      \bar "|"

      ges fes
      \clef "medicaea-fa1"
      \override NoteHead.style = #'medicaea.rvirga
      e des ces

      \set Staff.alterationGlyphs =
        #alteration-hufnagel-glyph-name-alist
      \override Staff.Custos.style = #'hufnagel
      \override NoteHead.style = #'hufnagel.punctum
      \clef "hufnagel-fa2"
      ces des es
      \bar "|"

      fes ges
      \clef "hufnagel-do2"
      \override NoteHead.style = #'hufnagel.lpes
      as! bes! ces'
      \override NoteHead.style = #'hufnagel.virga
      bes! as!
      \bar "|"

      ges! fes!
      \clef "hufnagel-do-fa"
      \override NoteHead.style = #'hufnagel.punctum
      es! des ces des! es! fes!
      \bar "||"

      s32*1
    }
  >>
>>

lowerStaff = \new MensuralStaff = "lowerStaff" <<
  \context MensuralVoice <<
    \transpose c c {

      \key a \major
      cis'1 d'\breve gis'\breve e'\breve \[ e'\longa fis'\longa \]
      \set Staff.forceClef = ##t
      \clef "neomensural-c2"
      cis1
      \bar "|"

      \[ g\breve dis''\longa \]
      b\breve \[ a\longa d\longa \]
      \clef "petrucci-c2"

      fis1 ces1
      \clef "petrucci-c2"
      r\longa
      \set Staff.forceClef = ##t
      \clef "mensural-c2"
      r\breve
      \bar "|"

      r2
      \clef "mensural-g"
      r4 r8 r16 r16
      \override NoteHead.style = #'mensural
      \override Rest.style = #'mensural
      \clef "petrucci-f"
      c8 b, c16 b, c32 b, c64 b, c64 b,
      d8 e d16 e d32 e d64 e d64 e
      r\longa
      \set Staff.forceClef = ##t
      \clef "petrucci-f"
      r\breve
      \bar "|"

      r\breve
      \clef "mensural-f"
      r2 r4 r8 r16 r16

      \set Staff.forceClef = ##t
      \clef "mensural-f"
      e\breve f g a1
      \clef "mensural-g"

      \[ bes'!\longa a'!\longa c''!\longa \]
      e'1 d' c' d' \bar "|"
      \bar "|"

      bes'!\longa fis'!1 as'!1 ges'!\longa % lig
      \set Staff.forceClef = ##t
      \clef "mensural-g"
      e'2 d' c' \bar "|"

      \set Staff.forceClef = ##t
      \clef "petrucci-g"
      c'2 d' e' f'
      \clef "petrucci-g"
      g' as'! bes'! cis''!
      bes'! as'! gis'! fis'!
      \set Staff.forceClef = ##t
      \clef "mensural-g"
      es'! des'! cis'!1 \bar "||"
    }
  >>
>>

\paper {
  line-thickness = #(/ staff-space 5.0)
}

\score {
  <<
    \upperStaff
    \lowerStaff
  >>
  \layout {
    indent = 0.0
    \context {
      \Score
      timing = ##f
    }
    \context {
      \MensuralVoice
      \override NoteHead.style = #'neomensural
      \override Rest.style = #'neomensural
      \override Flag.style = #'mensural
      \override Stem.thickness = #1.0
    }
    \context {
      \MensuralStaff
      \revert  BarLine.transparent
      alterationGlyphs =
        #alteration-mensural-glyph-name-alist
      clefGlyph = #"clefs.petrucci.c2"
    }
    \context {
      \VaticanaStaff
      \revert  BarLine.transparent
      \override StaffSymbol.thickness = #2.0
      alterationGlyphs =
        #alteration-vaticana-glyph-name-alist
      \override Custos.neutral-position = #4
    }
  }
}
