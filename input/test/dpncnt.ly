\version "1.7.18"

\header {

texidoc = "test file for new-new-chord names, ie, chord-generic-names
    "
    
}

efullmusic = \notes {

    %% ? what 'bout maj7?
    %% c:7 = \markup { \normal-size-super "maj7" }

    %% Choose your symbol for the fully diminished chord
    %% American:
    %% c:3-.5-.7- = \markup { "dim" }
    %% Jazz:
    %% c:3-.5-.7-
    <<c es ges bes>>-\markup { \super " o" }

    %% Hmm, this ok?
    %% c:7+
    <<c e g b>>-\markup { \normal-size-super
			  \override #'(font-family . math) "N" }
    %%c:3.5.7 = \markup { \override #'(font-family . math) "M" }
    %%c:3.5.7 = \markup { \normal-size-super "maj7" }
}

efull = #(sequential-music-to-chord-exceptions efullmusic #f)

epartialmusic = \notes {
    %c:2^3 =
    <<c d>>-\markup { \normal-size-super "2" }
    %c:3-
    <<c es>>-\markup { "m" }
    %c:4
    <<c f>>-\markup { \normal-size-super "sus4" }
    %c:5^3
    <<c g>>-\markup { \normal-size-super "5" }
}

epartial = #(sequential-music-to-chord-exceptions epartialmusic #f)

% ugh FIXME?
% default octave \chord mode has changed to c'
ch = \notes \transpose c c' 
{
	<<c e g>>1
	<<c es g>>
	<<c e gis>>
	<<c es ges>> \break
	<<c e g bes>>
	<<c es g bes>>
	<<c e g b>>
	<<c es ges beses>> 
	<<c es ges b>> \break
	<<c e gis bes>>
	<<c es g b>>
	<<c e gis b>> 
	<<c es ges bes>>\break
	<<c e g a>>
	<<c es g a>>
	<<c e g bes d'>> % ?? 
	<<c es g bes d'>> \break
	<<c es g bes d' f' a' >>
	<<c es g bes d' f' >>
	<<c es ges bes d' >> 
	<<c e g bes des' >> \break
	<<c e g bes dis'>>
	<<c e g bes d' f'>>
	<<c e g bes d' fis'>>
	<<c e g bes d' f' a'>>\break
	<<c e g bes d' fis' as'>>
	<<c e gis bes dis'>>
	<<c e g bes dis' fis'>>
	<<c e g bes d' f' as'>>\break
	<<c e g bes des' f' as'>>
	<<c e g bes d' fis'>>
	<<c e g b d'>>
	<<c e g bes d' f' as'>>\break
	<<c e g bes des' f' as'>>
	<<c e g bes des' f' a'>>
	<<c e g b d'>>
	<<c e g b d' f' a'>>\break
	<<c e g b d' fis'>>
	<<c e g bes des' f ' a'>>
	<<c f g>>
	<<c f g bes>>\break
	<<c f g bes d'>>
	<<c e g d'>>
	<<c es g f'>>
}


\score{
    <
	\context ChordNames {
       \property ChordNames.chordNameFunction = #banter-chord-names
	\ch
       \property ChordNames.chordNameFunction = #jazz-chord-names
	
	\ch
    }
	\context Staff \notes \transpose c c' { \ch \ch}
    >
    \paper{
	\translator { 
	    \ChordNamesContext
	    ChordName \override #'word-space = #1 
	}
    }
}
