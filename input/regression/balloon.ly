
\header {
  texidoc = "With balloon texts, objects in the output can be marked,
with lines and explanatory text added."
}
\version "2.7.39"

\layout{ ragged-right = ##t }

{
  
  \relative c'  {

    %% by hand:
    \once\override Stem #'stencil = #ly:balloon-interface::print
    \once\override Stem #'original-stencil = #ly:stem::print
    \once\override Stem #'balloon-text = #"I'm a stem"
    \once\override Stem #'balloon-text-offset = #'(3 . 4)
    \once\override Stem #'balloon-text-props
    = #'((font-family .  roman))


    %% use predefd function. 
    \context Voice \applyOutput #(add-balloon-text
				  'NoteHead "heads, or tails?"
				  '(0 . -3))

    
    c8
  }
}
