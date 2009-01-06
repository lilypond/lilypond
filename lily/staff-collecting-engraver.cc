/*
  staff-collecting-engraver.cc -- implement Staff_collecting_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2001--2009  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "staff-symbol.hh"
#include "engraver.hh"
#include "grob.hh"
#include "context.hh"

class Staff_collecting_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Staff_collecting_engraver);
  DECLARE_ACKNOWLEDGER (staff_symbol);
  DECLARE_END_ACKNOWLEDGER (staff_symbol);
};

Staff_collecting_engraver::Staff_collecting_engraver ()
{
}

void
Staff_collecting_engraver::acknowledge_staff_symbol (Grob_info gi)
{
  SCM staffs = get_property ("stavesFound");
  staffs = scm_cons (gi.grob ()->self_scm (), staffs);

  context ()->set_property ("stavesFound", staffs);
}

void
Staff_collecting_engraver::acknowledge_end_staff_symbol (Grob_info gi)
{
  SCM staffs = get_property ("stavesFound");
  staffs = scm_delq (gi.grob ()->self_scm (), staffs);

  context ()->set_property ("stavesFound", staffs);
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Staff_collecting_engraver, staff_symbol);
ADD_END_ACKNOWLEDGER (Staff_collecting_engraver, staff_symbol);

ADD_TRANSLATOR (Staff_collecting_engraver,
		/* doc */
		"Maintain the @code{stavesFound} variable.",

		/* create */
		"",

		/* read */
		"stavesFound ",

		/* write */
		"stavesFound "
		);
