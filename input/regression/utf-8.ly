\version "2.11.51"

%% Edit this file using a Unicode aware editor, such as GVIM, GEDIT, Emacs

%{

You may have to install additional fonts.

Red Hat Fedora

    taipeifonts fonts-xorg-truetype ttfonts-ja fonts-arabic \
	 ttfonts-zh_CN fonts-ja fonts-hebrew 

Debian GNU/Linux

   apt-get install emacs-intl-fonts xfonts-intl-.* \
	ttf-kochi-gothic ttf-kochi-mincho \
	xfonts-bolkhov-75dpi xfonts-cronyx-100dpi xfonts-cronyx-75dpi 
%} 

\header {

  texidoc = "Various scripts may be used for texts (like titles and
lyrics) introduced by entering them in UTF-8 encoding, and using a
Pango based backend.  Depending on the fonts installed, this fragment
will render Bulgarian (Cyrillic), Hebrew, Japanese and Portuguese.

"
  
}

% Cyrillic font
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

% "a legal song to you"
portuguese = \lyricmode { 
  à vo -- cê uma can -- ção legal
}

\paper {
  ragged-right = ##T
}

\relative  { 
  c2 d e f g f e
}
\addlyrics { \bulgarian }
\addlyrics { \hebrew }
\addlyrics { \japanese }
\addlyrics { \portuguese }
