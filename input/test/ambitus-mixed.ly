
\header {
texidoc = "The showing of ambituses can be switched off or they can be
shifted horizontally by using @code{applyoutput}.

If you want to mix per-voice and per-staff ambituses, then you have to
define new context type derived from the @code{Voice} or @code{Staff} 
context.  The derived context must contain the @code{Ambitus_engraver} 
and it must be accepted by a proper parent context, which are respectively
the @code{Staff} context or @code{Score} context in the example below. 
The original context and the derived context can then be used in parallel 
in the same score (not demonstrated in this file).
"
}

\version "2.3.2"

#(define (kill-ambitus grob grob-context apply-context)
  (if (memq 'ambitus-interface (ly:grob-property grob 'interfaces))
   (ly:grob-suicide grob)
  ))

#(define ((shift-ambitus x) grob grob-context apply-context)
  (if (memq 'ambitus-interface (ly:grob-property grob 'interfaces))
   (ly:grob-translate-axis! grob x X)
  ))



voiceA = \notes \relative c'' {
  c4 a d e f2
}
voiceB = \notes \relative c' {
  es4 f g as b2 
}
\score {
  \context ChoirStaff <<
    \new Staff <<
	{
	   \applyoutput  #(shift-ambitus 1.0)
	    \voiceA
	   } \\
       {
	   \voiceB
       }
    >>
    \new Staff <<
       {  \applyoutput #kill-ambitus \voiceA } \\
       {  \applyoutput #kill-ambitus \voiceB }
    >>
  >>
  \paper {
    raggedright = ##t

    \context {
	\Voice
      \consists Ambitus_engraver
    }
    }
}
