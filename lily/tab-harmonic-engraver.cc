/*
  tab-harmonic-engraver.cc -- implement Tab_harmonic_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "engraver.hh"

#include "item.hh"
#include "pointer-group-interface.hh"
#include "simple-closure.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

class Tab_harmonic_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Tab_harmonic_engraver);

protected:
  DECLARE_ACKNOWLEDGER (note_head);
};

Tab_harmonic_engraver::Tab_harmonic_engraver ()
{
}

void
Tab_harmonic_engraver::acknowledge_note_head (Grob_info info)
{
  if (Stream_event *note = info.event_cause ())
    {
      for (SCM s = note->get_property ("articulations");
	   scm_is_pair (s); s = scm_cdr (s))
	{
	  Stream_event *ev = unsmob_stream_event (scm_car (s));
	  
	  if (!ev)
	    continue;

	  
	  if (ev->in_event_class ("harmonic-event"))
	    {
	      if (Item *victim = info.item ())
		{
		  Engraver *eng = dynamic_cast<Engraver*> (info.origin_translator ());
		  Item *paren = eng->make_item ("HarmonicParenthesesItem", victim->self_scm ());
		  Pointer_group_interface::add_grob (paren, ly_symbol2scm ("elements"), victim);

		  paren->set_parent (victim, Y_AXIS);
	      
		  Real size = robust_scm2double (paren->get_property ("font-size"), 0.0)
		    + robust_scm2double (victim->get_property ("font-size"), 0.0);
		  paren->set_property ("font-size", scm_from_double (size));
		}
	    }
	}
    }
}

ADD_ACKNOWLEDGER (Tab_harmonic_engraver, note_head);
ADD_TRANSLATOR (Tab_harmonic_engraver,
		/* doc */
		"In a tablature, parenthesize objects whose music cause has"
		" the @code{parenthesize} property.",
		
		/* create */
		"HarmonicParenthesesItem ",

		/* read */
		"",

		/* write */
		""
		);
