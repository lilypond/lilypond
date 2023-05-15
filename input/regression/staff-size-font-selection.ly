\version "2.25.5"

\header {
  texidoc = "Font size configuration is independent from font
family configuration.  In this test, the call to
@code{set-global-staff-size} should not affect the font
families defined previously."
}

%{
You may have to install additional fonts.

Red Hat Fedora

  linux-libertine-fonts

Debian GNU/Linux, Ubuntu

  fonts-linuxlibertine
%}

\paper {
  fonts.serif = "Linux Libertine O,serif"
}

#(set-global-staff-size 40)

\markup \wordwrap-string "This text should be rendered in
Linux Libertine (provided that the font is installed)."
