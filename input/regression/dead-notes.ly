\version "2.21.0"

\header{ texidoc = "Muted notes (also called dead notes) are supported
                    within normal staves and tablature.  They are printed
                    correctly, even if another font for TabNoteHead is used. "
       }

%{
You may have to install additional fonts.

Red Hat Fedora

  dejavu-fonts-all

Debian GNU/Linux, Ubuntu

  fonts-dejavu-core
%}

mynotes = \relative c,, {
   \deadNotesOn
   e8. e16
   \deadNotesOff
   g4 a b |
   e8. \deadNote e16 g4 a b |
   e,4. \deadNote { e8 e e } e4 |
   < e, \deadNote b' e >8 < e \deadNote b' e > < e \deadNote b' e >4 < e \deadNote b' e >4 r
   \bar "|."
}

\score {
  \new StaffGroup <<
    \new Staff {
      \new Voice {  % Warning: explicit voice instantiation is required
                    %   to have deadNotesOff work properly
                    %   when deadNotesOn comes at the beginning
                    %   of the piece
        \clef "bass_8"
        \mynotes
      }
    }
    \new TabStaff
      \with {
        instrumentName = \markup \tiny "default-font"
      }{
      \new TabVoice {  % Warning:  explicit voice instantiation is
                       %   required to have deadNotesOff work properly
                       %   when deadNotesOn comes at the beginning
                       %   of the piece
        \mynotes
      }
    }
    \new TabStaff
      \with {
        \override TabNoteHead.font-name = "DejaVu Sans Mono"
        instrumentName =
          \markup \tiny \center-column
            { "TabNoteHead-" "font: DejaVu" "Sans Mono" }
      }{
      \new TabVoice {  % Warning:  explicit voice instantiation is
                       %   required to have deadNotesOff work properly
                       %   when deadNotesOn comes at the beginning
                       %   of the piece
        \mynotes
      }
    }
  >>
  \layout {
    indent = 20
    \context {
      \TabStaff
      stringTunings = #bass-tuning
    }
  }
}


