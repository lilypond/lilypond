\version "2.25.33"

\header {
  texidoc="
Note head shapes may be set from several choices.
The stem endings should be adjusted according to the note head.
If you want different note head styles on one stem,
you must create a special context.

Harmonic notes have a different shape and different
dimensions.
"
}

\layout {
  indent = 0.0
  ragged-right = ##t

  \context {
    \Score
    \remove Bar_number_engraver
    \override BarLine.allow-span-bar = ##f
  }

  \context {
    \Staff
    \clef C
    \omit Clef
  }
}

pattern =
#(define-music-function (head-style) (symbol?) #{ <<
  \override Staff.NoteHead.style = #head-style
  \once \override Staff.TimeSignature.style = #'numbered
  \once \override Staff.TimeSignature.denominator-style = #'note
  \once \override Staff.TimeSignature.note-head-style = #head-style
  \once \override Staff.TimeSignature.break-visibility = #end-of-line-invisible
  \time 2/2
  <>^\markup #(symbol->string head-style)
  \new Voice {
    \override Stem.direction = #UP
    e'4 2. 1 \breve*1/2 \longa*1/4
    \once \override Score.BarLine.allow-span-bar = ##t
    \section
  }
  \new Voice {
    \override Stem.direction = #DOWN
    a4  2. 1 \breve*1/2 \longa*1/4
    \once \override Score.BarLine.allow-span-bar = ##t
    \section
  }
>> #} )

%% When rearranging these to add new styles, take into account that it probably
%% requires less effort for a reader to compare styles that are near each other
%% in the same column.  Try to group styles that are likely to be compared that
%% way, e.g. mensural, neomensural, and petrucci.  Also, try to put similar
%% styles with contrasting applications near each other so that the contrast is
%% more easily noticed, e.g. triangle and arrow.  (LilyPond's "arrow" style
%% corresponds to Gould's "triangular noteheads" and SMUFL glyphs with
%% "triangle" in the name.)
\new StaffGroup <<
  \new Staff {
    \pattern #'default
    \pattern #'mensural
  }
  \new Staff {
    \pattern #'altdefault
    \pattern #'neomensural
  }
  \new Staff {
    \pattern #'baroque
    \pattern #'petrucci
  }
  \new Staff {
    \pattern #'slash
    \pattern #'diamond
  }
  \new Staff {
    \pattern #'arrow
    \pattern #'harmonic
  }
  \new Staff {
    \pattern #'triangle
    \pattern #'harmonic-black
  }
  \new Staff {
    \pattern #'cross
    \pattern #'harmonic-mixed
  }
  \new Staff {
    \pattern #'xcircle
    \once \omit Staff.TimeSignature % meh
  }
>>
