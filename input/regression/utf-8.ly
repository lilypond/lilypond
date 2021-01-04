\version "2.19.21"

\header {
  texidoc = "
Various scripts may be used for texts (like titles and lyrics) by
entering them in UTF-8 encoding, and using a Pango based backend.
Depending on the fonts installed, this fragment will render Bulgarian
(Cyrillic), Hebrew, Japanese and Portuguese.
"
}

%{
You may have to install additional fonts.

Red Hat Fedora

  linux-libertine-fonts (Latin, Cyrillic, Hebrew)
  google-noto-serif-jp-fonts (Japanese)

Debian GNU/Linux, Ubuntu

  fonts-linuxlibertine (Latin, Cyrillic, Hebrew)
  fonts-noto-cjk (Japanese)
%}

% 'Linux Libertine' fonts also contain Cyrillic and Hebrew glyphs.
\paper {
  #(define fonts
    (set-global-fonts
     #:roman "Linux Libertine O, Noto Serif CJK JP, Noto Serif JP, serif"
   ))
}

bulgarian = \lyricmode {
  Жълтата дюля беше щастлива, че пухът, който цъфна, замръзна като гьон.
}

hebrew = \lyricmode {
  זה כיף סתם לשמוע איך תנצח קרפד עץ טוב בגן.
}

japanese = \lyricmode {
  いろはにほへど ちりぬるを
  わがよたれぞ  つねならむ
  うゐのおくや  まけふこえて
  あさきゆめみじ ゑひもせず
}

% "a nice song for you"
portuguese = \lyricmode {
  à vo -- cê uma can -- ção le -- gal
}

\paper {
  ragged-right = ##T
}

\relative c' {
  c2 d
  e2 f
  g2 f
  e2 d
}
\addlyrics { \bulgarian }
\addlyrics { \hebrew }
\addlyrics { \japanese }
\addlyrics { \portuguese }
