\score { \notes \relative c{
c''4 c2 c8  c16 c16 c1 
\property Voice.noteHeadStyle = "diamond"
c4 c2 c8  c16 c16  c1
\property Voice.noteHeadStyle = "transparent"
c4 c2 c8  c16 c16  c1
\property Voice.noteHeadStyle = "cross"
c4 c2 c8  c16 c16  c1
\property Voice.noteHeadStyle = "harmonic"
c4 c2 c8  c16 c16  c1

   \context ThreadedVoice <
    \context Thread = TA
      { \property Thread.noteHeadStyle = "cross"
        \property ThreadedVoice.verticalDirection = \up c16} 
    \context Thread = TB
      { \property Thread.noteHeadStyle = "" a16  }
    
    \context Thread = TC
      { \property Thread.noteHeadStyle = "harmonic" d16 }
    
  >
}

    \paper {
        \translator {
	  \VoiceContext
	  \remove Note_heads_engraver;
	  \accepts Thread;
	  \name ThreadedVoice;
	}
	\translator {
	  \StaffContext
	  \accepts ThreadedVoice;
	}
	\translator {
	  \type Engraver_group_engraver;
	  \consists Note_heads_engraver;
	  \name Thread;
	}
    }
}
\version "1.0.20"; 
