\header {
  texidoc = "The default font families for text can be
  overridden with @code{make-pango-font-tree}."
}

\version "2.21.2"

%{
You may have to install additional fonts.

Red Hat Fedora

  dejavu-fonts-all

Debian GNU/Linux, Ubuntu

  fonts-dejavu-core
  fonts-dejavu-extra
%}

\paper {
  % change for other default global staff size.
  myStaffSize = #20

  #(define fonts
    (make-pango-font-tree "DejaVu Serif"
                          "DejaVu Sans"
                          "DejaVu Sans Mono"
     (/ myStaffSize 20)))
}

{
  g'''4^\markup {
    DejaVu Serif: \bold bold
                  \italic italic
                  \italic \bold { bold italic }
  }
  g4_\markup {
    \override #'(font-family . sans) {
      DejaVu Sans: \bold bold
                   \italic italic
                   \italic \bold { bold italic }
    }
  }
  g''2^\markup {
    \override #'(font-family . typewriter) {
      DejaVu Sans Mono: \bold bold
                        \italic italic
                        \italic \bold { bold italic }
    }
  }
}
