
\header {
    texidoc = "test file for new-new-chord names, ie, double-plus-new-chord-name
TODO: FIXME.
"
}

\score { \notes { c4^"fixme"}}

%{

efull = \chordnames {

    %% ? what 'bout maj7?
    %% c:7 = \markup { \normal-size-super "maj7" }

    %% Choose your symbol for the fully diminished chord
    %% American:
    %% c:3-.5-.7- = \markup { "dim" }
    %% Jazz:
    c:3-.5-.7- = \markup { \super " o" }

    %% Hmm
    %%    	   ;;Pick your favorite maj7
    %%	   ((0) mathm-markup-object)  ;;a white triangle
    %%	   ;;((0) mathn-markup-object) ;;a black triangle
    %% ;;((0) (make-simple-markup "maj7")) ;;good old maj7

    %% This ok?
    c:7+ = \markup { \normal-size-super \override #'(font-family . math) "N" }
    %%c:3.5.7 = \markup { \override #'(font-family . math) "M" }
    %%c:3.5.7 = \markup { \normal-size-super "maj7" }
}

epartial = \chordnames {
    c:2^3 = \markup { \normal-size-super "2" }
    c:3-  = \markup { "m" }
    c:4   = \markup { \normal-size-super "sus4" }
    c:5^3 = \markup { \normal-size-super "5" }
}


ch = \notes \transpose c' c' 
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
	% #(set-chord-name-style 'jazz)
	% #(set-chord-name-style 'double-plus-new-banter)
	% #(set-chord-name-style 'double-plus-new-jazz)
	
	#(set-double-plus-new-chord-name-style 'banter
	   `((separator . ,(make-simple-markup ":"))
	     (full-exceptions . ,efull)
	     (partial-exceptions . ,epartial)))
	\ch
	#(set-double-plus-new-chord-name-style 'jazz
	   `((separator . ,(make-simple-markup ":"))
	     (full-exceptions . ,efull)
	     (partial-exceptions . ,epartial)))
	
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

%} 
