
\header {
    texidoc = "With balloon texts, objects in the output can be marked,
with lines and explanatory text added."
    }
\version "2.3.17"

\score  {
  {
     
   \relative c'  {

       %% by hand:
       \once\override Stem  #'print-function = #Balloon_interface::print
       \once\override Stem  #'balloon-original-callback = #Stem::print
       \once\override Stem  #'balloon-text = #"I'm a stem"
       \once\override Stem  #'balloon-text-offset = #'(3 . 4)
       \once\override Stem  #'balloon-text-props
      = #'((font-family .  roman))


       %% use predefd function. 
       \context Voice \applyoutput #(add-balloon-text
				     'NoteHead "heads, or tails?"
				     '(0 . -3))

      
       c8
       }
  }
 \paper{ raggedright = ##t }
}
