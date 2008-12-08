/*
  tab-staff-symbol-engraver.cc -- implement Tab_staff_symbol_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"
#include "spanner.hh"

class Tab_staff_symbol_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Tab_staff_symbol_engraver);
protected:
  DECLARE_ACKNOWLEDGER (staff_symbol);
};

void
Tab_staff_symbol_engraver::acknowledge_staff_symbol (Grob_info gi)
{
  int k = scm_ilength (get_property ("stringTunings"));
  if (k >= 0)
    gi.grob ()->set_property ("line-count", scm_from_int (k));
}

Tab_staff_symbol_engraver::Tab_staff_symbol_engraver ()
{
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Tab_staff_symbol_engraver, staff_symbol);
ADD_TRANSLATOR (Tab_staff_symbol_engraver,
		/* doc */
		"Create a tablature staff symbol, but look at"
		" @code{stringTunings} for the number of lines.",

		/* create */
		"StaffSymbol ",

		/* read */
		"stringTunings ",

		/* write */
		""
		);
