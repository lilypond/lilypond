\version "2.5.10"

% Edit this file using a Unicode aware editor, such as GVIM, GEDIT, Emacs

%{

I installed some additional font packages to get this working.

taipeifonts 
fonts-xorg-truetype 
ttfonts-ja 
fonts-arabic 
ttfonts-zh_CN 
fonts-ja 
fonts-hebrew 

%} 

\header {

  texidoc = "Various scripts may be used for texts (like titles and
lyrics) introduced by entering them in UTF-8 encoding, and using a
Pango based backend. Depending on the fonts installed, this fragment
will render Japanese, Hebrew and Cyrillic.

"
  
}


japanese = \lyricsto "melody" \new Lyrics {  

  いろはにほへど ちりぬるを
  わがよたれぞ  つねならむ
  うゐのおくや  まけふこえて
  あさきゆめみじ ゑひもせず 

 }


bulgarian = \lyricsto "melody" \new Lyrics {

    Жълтата дюля беше щастлива, че пухът, който цъфна, замръзна като гьон.

}

hebrew = \lyricsto "melody" \new Lyrics { 
זה כיף סתם לשמוע איך תנצח קרפד עץ טוב בגן.
}

% latin1 section start

 <<
  \context Voice = "melody" \relative  { 
     c2 d e f g f e 
  } 
  \hebrew
  \bulgarian

  %% no support for TTF  yet.
  
  \japanese
  
>>
