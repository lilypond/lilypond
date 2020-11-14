\version "2.16.0"

\header {

  texidoc = "A single Pango string is processed according to the
Unicode Bidirectional Algorithm.  The strong Hebrew characters in
this example are set right-to-left, and the Latin numerals, space
character, and punctuation are set according to the rules of the
algorithm."

}

%{
You may have to install additional fonts.

Red Hat Fedora

  linux-libertine-fonts (Latin, Hebrew)

Debian GNU/Linux, Ubuntu

  fonts-linuxlibertine (Latin, Hebrew)
%}

% Linux Libertine fonts contain Hebrew glyphs.
\paper {
  #(define fonts
    (set-global-fonts
     #:roman "Linux Libertine O, serif"
   ))
}

\markup {
 "לל1ללל, רר2רר."
}
