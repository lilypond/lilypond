/*
  tab-staff-symbol-engraver.cc -- implement Tab_staff_symbol_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "staff-symbol-engraver.hh"
#include "spanner.hh"

class Tab_staff_symbol_engraver : public Staff_symbol_engraver
{
public:
  TRANSLATOR_DECLARATIONS (Tab_staff_symbol_engraver);
protected:
  virtual void start_spanner ();
};

void
Tab_staff_symbol_engraver::start_spanner ()
{
  bool init = !span_;
  Staff_symbol_engraver::start_spanner ();
  if (init)
    {
      int k = scm_ilength (get_property ("stringTunings"));
      if (k >= 0)
	span_->set_property ("line-count", scm_from_int (k));
    }
}

Tab_staff_symbol_engraver::Tab_staff_symbol_engraver ()
{
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Tab_staff_symbol_engraver, grob);
ADD_TRANSLATOR (Tab_staff_symbol_engraver,
		/* doc */ "Create a staff-symbol, but look at stringTunings for the number of lines."
		"staff lines.",
		/* create */ "StaffSymbol",
		/* accept */ "staff-span-event",
		/* read */ "stringTunings",
		/* write */ "");
