\header {

texidoc = " Chord names are generated from a list pitches.  The
functions constructing the names are customisable. This file shows
Jazz chords.  Compare with @file{chords-ignatzek.ly}."

}

chs = \notes \transpose c c' 
{
	<<c e g>>1-"dpn"
	<<c es g>>  % m = minor triad
	<<c e gis>>
	<<c es ges>> \break
	<<c e g bes>>
	<<c es g bes>>
	<<c e g b>> 		% triangle = maj
	<<c es ges beses>> 
	<<c es ges b>> \break
	<<c e gis bes>>
	<<c es g b>>
	<<c e gis b>> 
	<<c es ges bes>>\break
	<<c e g a>>   % 6 = major triad with added sixth
	<<c es g a>>  % m6 = minor triad with added sixth
	<<c e g bes d'>> 
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
	<<c e g d'>>	% add9
	<<c es g f'>>
}

efullmusic = \notes
{
    <<c e gis>>1-\markup { "+" }
    <<c e g b>>-\markup { \normal-size-super
    %			  \override #'(font-family . math) "N" }
			  \override #'(font-family . math) "M" }
    %%c:3.5.7 = \markup { \override #'(font-family . math) "M" }
    %%c:3.5.7 = \markup { \normal-size-super "maj7" }

   <<c es ges>>-\markup { \super "o" } % should be $\circ$ ?
   <<c es ges bes>>-\markup { \super \combine "o" "/" }
   <<c es ges beses>>-\markup { \super  "o7" }
}

efull = #(sequential-music-to-chord-exceptions efullmusic #f)

epartialmusic = \notes{
    <<c d>>1-\markup { \normal-size-super "2" }
    <<c es>>-\markup { "m" }
    <<c f>>-\markup { \normal-size-super "sus4" }
    <<c g>>-\markup { \normal-size-super "5" }
    
    %% TODO, partial exceptions
    <<c es f>>-\markup { "m" }-\markup { \normal-size-super "sus4" }
    <<c d es>>-\markup { "m" }-\markup { \normal-size-super "sus2" }
}

epartial = #(sequential-music-to-chord-exceptions epartialmusic #f)


\score{
    <
    \context ChordNames {
	
%{
     \property ChordNames.chordNameFunction = #double-plus-new-chord->markup
     \property ChordNames.chordNameStyle = #'jazz
%}

       \property ChordNames.majorSevenSymbol = #whiteTriangleMarkup
       \property ChordNames.chordNameSeparator = #(make-simple-markup  "/")
       \property ChordNames.chordNameExceptionsFull = #efull
       \property ChordNames.chordNameExceptionsPartial = #epartial
	
	%% FIXME
	%%\property ChordNames.chordNoteNamer = #'step->markup-ignatzek
	%%chordRootNamer = #note-name->markup
	
	#(set-chord-name-style 'jazz)
	
	\chs
    }
    \context Staff \notes \transpose c c { \chs }
    >
    \paper{
	\translator { 
	    \ChordNamesContext
	    ChordName \override #'word-space = #1 
	}
    }
}
	
