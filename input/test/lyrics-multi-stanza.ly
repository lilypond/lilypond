
\version "1.3.110";
\include "english.ly"

\header{
texidoc="
Multiple stanzas

 Tests a number of features:
 * Lyric_phrasing_engraver
 * Stanza_number_engraver
 * Automatic melismata on beamed notes

";

    title = "Crowned with Honour";
    composer = "Oliver Holden (1765-1844)";
    poet = "Thomas Kelly (1769-1855)";
}

allup = \notes{
	\stemUp
	\slurUp
        \tieUp
	\property Voice.DynamicLineSpanner \override #'direction = #1
	\autoBeamOff
}

alldown = \notes{
	\stemDown
	\slurDown
        \tieDown
	\property Voice.DynamicLineSpanner \override #'direction = #-1
	\autoBeamOff
}

Global =  \notes{
        \key g \major;
        \time 4/4;
	\partial 4;
}

Soprano = \notes \relative c' {
	\allup
	d4 | g g b b a g a b a g b a g( a )g % modified to test melisma align right

	[a8 b] | b4 % modified to test melisma align left
	 a g b [d16 d c8] [b a] b4 % modified to test beam melisma
%	a4 | b a g b [d8 \melisma c] \melismaEnd [b \melisma a] \melismaEnd b4

	d d2 d e d4( cs8 ~ )cs d2.

	b4 | d b g b [a8  g]  [a b]  a4
%	b4 | d b g b [a8 \melisma g] \melismaEnd [a \melisma b] \melismaEnd a4

	g d'2 c b4.( c8 )a4 a g2.
}
Alto = \notes \relative c'{
	\alldown
	d4 | d d g g fs g fs g fs e g fs d2.
	d4 | g d b g' [b8 a] [g fs] g4 fs g2 a g fs4( e8 )g fs2.
	d4 | g g d g [fs8 e] [fs g] fs4 g f2 e d4.( d8 )d4 fs4 d2.
}
Tenor = \notes \relative c{
	\allup
	d4 | b' b d d c  b c  d c  b d c  b2.
	a4 | b a g b  [d8 c] [b a] b4 a  b2 c b  a   a2.
	g4 | b d b d [c8  b] [c  d]  c4 b g2 g g4.( a8 [fs )a] c4 b2.
}
Bass = \notes \relative c{
	\alldown
	d4 | g g g g d d d g d e d d g,2.
	d'4 | g d b g' [b8 a] [g fs] g4 d g2 fs e a d,2.
	g4 | g g g g d d d e b2 c d2. d4 g,2.
}

TheLyrics =  \lyrics <
        {
	    \context LyricsVoice = "Soprano-1"
	    \property LyricsVoice .stanza = "1:"
	    \property LyricsVoice .stz = "(1)"
	         The4 head    that once was crowned with thorns
	         Is   crowned with glo -- ry  now;
		 A roy -- al di -- a -- dem a -- dorns
		 The might -- y Vic -- tor's  brow.
		 A roy -- al di -- a -- dem a -- dorns
		 The might -- y Vic -- tor's  brow.
        }
        {
	    \context LyricsVoice = "Soprano-2"
	    \property LyricsVoice .stanza = "2:"
	    \property LyricsVoice .stz = "(2)"
	         The4 high -- est place that heav'n af -- fords
	         Is His by sov -- 'reign  right;
		 The King of kings, the Lord of lords,
 		 He reigns in glo -- ry  bright,
		 The King of kings, the Lord of lords, 
 		 He reigns in glo -- ry  bright.
        }
        {
	    \context LyricsVoice = "Soprano-3"
	    \property LyricsVoice .stanza = "3:"
	    \property LyricsVoice .stz = "(3)"
	         The joy of all who dwell a -- bove,
	         The joy of saints be --  low,
		 To4 whom He man -- i -- fests His love,
		 And grants His name to  know,
		 To4 whom He man -- i -- fests His love,
		 And grants His name to4  know.
        }
>


\score{
  \context ChoirStaff
    \notes
    <
      \property Score.barNumberScriptPadding = #10.0
      \context Staff = "treblestaff"{
        <
	  \context Voice = "Soprano" { }
	  \context Voice = "Alto" { }
        >
      }
      \context Lyrics = mainlyrics { }
      \context Staff = "treblestaff"{
	<
	  \Global
	  \addlyrics { \context Voice = "Soprano" \Soprano }
	    { \context Lyrics = mainlyrics \TheLyrics }
	  \context Voice = "Alto"  \Alto
        >
	\bar "|.";
      }
      \context Staff = "bassstaff"{
        \clef "bass";
	<
	  \context Voice = "Tenor" { \Tenor }
	  \context Voice = "Bass"  { \Bass }
	>
        \bar "|.";
      }
    >

   \paper {
      \translator{
	\VoiceContext
	automaticMelismata = ##t;
	noAutoBeaming = ##t;
        \remove   "Auto_beam_engraver";
      }

   }
}
