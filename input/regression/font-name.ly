\version "2.21.0"

\header {
  texidoc = "Other fonts can be used by setting @code{font-name} for
the appropriate object.  The string should be a Pango font description
without size specification."
}

\layout { ragged-right = ##t }

%{
You may have to install additional fonts.

Red Hat Fedora

  bitstream-vera-sans-fonts
  dejavu-fonts-all
  linux-libertine-fonts

Debian GNU/Linux, Ubuntu

  fonts-dejavu-core
  fonts-linuxlibertine
  ttf-bitstream-vera
%}

{
  \override Score.PaperColumn.keep-inside-line = ##f

  \override Staff.TimeSignature.font-name = "Linux Libertine O"
  \time 3/4
  \set Score.skipBars = ##t
  \override Staff.MultiMeasureRestText.font-name = "DejaVu Sans Mono"
  R1*21^"Rest in DejaVu Sans Mono"

  c'1_\markup {
    \override #'((font-name . "Bitstream Vera Sans, Bold")
                 (font-size . 4))
      { "This text is in large Vera Sans Bold" }
  }
}
