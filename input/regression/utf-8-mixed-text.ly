\version "2.16.0"

\header {
  texidoc = "words in mixed font in a single string
 are separated by spaces as in the input string.
 Here a Russian word followed by a roman word."
  
}

%{
You may have to install additional fonts.

Red Hat Fedora

  linux-libertine-fonts (Latin, Cyrillic)

Debian GNU/Linux, Ubuntu

  fonts-linuxlibertine (Latin, Cyrillic)
%}

% Linux Libertine fonts contain Cyrillic glyphs.
\paper {
  #(define fonts
    (set-global-fonts
     #:roman "Linux Libertine O, serif"
   ))
}

\markup { "Здравствуйте Hallo" }
