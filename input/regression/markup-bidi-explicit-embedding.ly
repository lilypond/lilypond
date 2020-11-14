\version "2.16.0"

\header {
  texidoc = "
The explicit directional embedding codes, U+202A and U+202B, are
supported in single-line markup strings.  The embeddings must be
terminated with the pop directional formatting character, U+202C.
"
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
  \column {
    "אבה אבה \"ABC!\" אבה אבה!"
    "אבה אבה \"‪ABC!‬\" אבה אבה!"
    \null
    "abc def \"אבה!\" ghi jkl!"
    "abc def \"‫אבה!‬\" ghi jkl!"
  }
}
