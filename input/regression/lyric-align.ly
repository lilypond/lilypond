\version "1.5.68"
\header{
  texidoc="Lyric alignment

  Lyric alignment is adjustable both interms of alignment between stanzas and on notehead.

  If the property alignment is not set, there is automatic determination of alignment type based on punctuation. (ee lyric-phrasing.ly)

"
}

%\paper { linewidth = -1. }
\score {
< \context Voice = "v" \notes  \relative c'' {
      \property Staff.automaticMelismata = ##t
      \cadenzaOn
      a\breve  \bar "||" a1 \bar "|"  a \bar "|"  a \bar "||" \break a \bar "|" a \bar "|" a  \bar "|" a \bar "||" \break a \bar "|" a \bar "|."
    }
    \context Lyrics <
      \context LyricsVoice = "v-1" \lyrics {
%        \property LyricsVoice . stanza = "1:"
	\property Lyrics . LyricText \override #'ignore-length-mismatch = ##t
	\property Lyrics . LyricText \override #'alignment = #-1
	\property Lyrics . LyricText \override #'begin-alignment = #8

	"Particularly useful for reciting notes  "\breve
	left1

	\property Lyrics . LyricText \override #'alignment = #0
	centered

	\property Lyrics . LyricText \override #'alignment = #1        

	right

	\property Lyrics . LyricText \override #'alignment = #-1 
	\property Lyrics . LyricText \override #'begin-alignment = #2 

	left_half_way

	\property Lyrics . LyricText \override #'begin-alignment = #4 

	left_one_quarter

	\property Lyrics . LyricText \override #'begin-alignment = #10

	left_one_tenth

	\property Lyrics . LyricText \override #'begin-alignment = #1

	left_one_whole

	\property Lyrics . LyricText \override #'ignore-length-mismatch = ##f
	\property Lyrics . LyricText \override #'begin-alignment = #4

	Very_short_lyrics_remain_in_touch_with_their_note

	\property Lyrics . LyricText \override #'alignment = #1
	\property Lyrics . LyricText \override #'end-alignment = #1.1
	\property Lyrics . LyricText \override #'ignore-length-mismatch = ##t


	Unless_ignore-length-mismatch_is_true

      }
      \context LyricsVoice = "v-2" \lyrics {
%        \property LyricsVoice . stanza = "2:"
        " with many syllables under them."\breve
	  l1 c r1 l
	  l1 l x x x

				% note' true'
				%% ? what are the last 2 for? 
      }
   >
   >
}
