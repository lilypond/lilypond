
\header {
    texidoc = "With balloon texts, objects in the output can be marked,
with lines and explanatory text added."
    }
\version "2.1.11"

\score  {
 \notes {
     
   \relative c'  {

       %% by hand:
       \once\property Voice.Stem \set #'molecule-callback = #Balloon_interface::brew_molecule
       \once\property Voice.Stem \set #'balloon-original-callback = #Stem::brew_molecule
       \once\property Voice.Stem \set #'balloon-text = #"I'm a stem"
       \once\property Voice.Stem \set #'balloon-text-offset = #'(3 . 4)
       \once\property Voice.Stem \set #'balloon-text-props
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
