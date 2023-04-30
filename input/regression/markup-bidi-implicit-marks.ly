\version "2.25.5"

\header {
  texidoc = "
The implicit directional marks, U+200E and U+200F, are supported
in single-line markup strings.
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
  property-defaults.fonts.serif = "Linux Libertine O, serif"
}

\markup {
  \column {
    "אבה \"ABC!\" אבה"
    "אבה \"ABC!‎\" אבה"
    \null
    "abc \"אבה!\" def"
    "abc \"אבה!‏\" def"
  }
}
