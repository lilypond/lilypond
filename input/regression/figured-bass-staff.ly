
\paper {
  ragged-right = ##t
}

\version "2.12.0"

\header {

  texidoc = "Figured bass can also be added to Staff context directly.
In that case, the figures must be entered with @code{\\figuremode} and be directed
to an existing @code{Staff} context.

Since these engravers are on @code{Staff} level, properties
controlling figured bass should be set in @code{Staff} context.

"

}

<<

  \new Staff = someUniqueName
  \relative c'' {
    c4 c'8 r8 c,4 c'
  }

  %% send to existing Staff.
  \context Staff = someUniqueName 
  \figuremode {
    <4>4 <4>8 s8
    
    \set Staff.useBassFigureExtenders = ##t
    <4 6>4 <4 6>
  }
>>
