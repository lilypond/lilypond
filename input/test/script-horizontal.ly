

%{

Please don't use this Scheme hacking unless you have a good reason.
Support for the \property textExtraOffset will not go into LilyPond in
this form.

I think extra-offset will stay - but perhaps I'll change the name

%}


#(set! generic-thread-properties
	(append!
	 generic-thread-properties
	 (list
	  (cons "Text_item"
	   (list
	    (list 'textExtraOffset pair? 'extra-offset)
	   )
	  )
	  (cons "Script"
	   (list
	    (list 'scriptExtraOffset pair? 'extra-offset)
	   )
	  )
	 )
))
	

	
\score {

	\notes \context Voice <
		\context Thread = TA { c'4_1 }
		\context Thread = TB {
			\property Thread.scriptHorizontal = ##t
			\property Thread.textExtraOffset = #'(-0.5 . -0.5)
			\property Thread.scriptExtraOffset = #'(2.25 . -0.5)
		e'4-2^\prall }
		\context Thread = TC { g'4^4 }
	>
	
	\paper { \translator {
		\VoiceContext
		\remove Script_engraver;
		\remove Text_engraver;
		
		}
		\translator {
		\ThreadContext
		Generic_property_list = #generic-thread-properties
		\consists Script_engraver;
		\consists Text_engraver;		
		}
	}
}
