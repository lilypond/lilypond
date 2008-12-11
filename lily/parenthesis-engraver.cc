/*
  parenthesis-engraver.cc -- implement Parenthesis_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "engraver.hh"

#include "item.hh"
#include "pointer-group-interface.hh"
#include "simple-closure.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

class Parenthesis_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Parenthesis_engraver);

protected:
  DECLARE_ACKNOWLEDGER (grob);
};

Parenthesis_engraver::Parenthesis_engraver ()
{
}

void
Parenthesis_engraver::acknowledge_grob (Grob_info info)
{
  if (Stream_event *ev = info.event_cause ())
    {
      if (to_boolean (ev->get_property ("parenthesize")))
	{
	  if (Item *victim = dynamic_cast<Item*> (info.grob ()))
	    {
	      Engraver *eng = dynamic_cast<Engraver*> (info.origin_translator ());
	      Item *paren = eng->make_item ("ParenthesesItem", victim->self_scm ());
	      Pointer_group_interface::add_grob (paren, ly_symbol2scm ("elements"), victim);

	      paren->set_parent (victim, Y_AXIS);
	      
	      Real size = robust_scm2double (paren->get_property ("font-size"), 0.0)
		+ robust_scm2double (victim->get_property ("font-size"), 0.0);
	      paren->set_property ("font-size", scm_from_double (size));
	      
	      /*
		TODO?

		enlarge victim to allow for parentheses space? 
	      */
	    }
	  else
	    {
	      programming_error ("Don't know how to parenthesize spanners.");
	    }
	}
    }
}

ADD_ACKNOWLEDGER (Parenthesis_engraver, grob);
ADD_TRANSLATOR (Parenthesis_engraver,
		/* doc */
		"Parenthesize objects whose music cause has the"
		" @code{parenthesize} property.",
		
		/* create */
		"ParenthesesItem ",

		/* read */
		"",

		/* write */
		""
		);
