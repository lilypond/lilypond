\header {

texidoc = "Jazz chords, following [Ignatzek1995], page 17 and 18."

}


chs = \notes \transpose c' c' 
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
	\context ChordNames { \chs }
	\context Staff \notes \transpose c c' { \chs }
    >
    \paper{
	\translator { 
	    \ChordNamesContext
	    ChordName \override #'word-space = #1 
	}
    }
}
	
