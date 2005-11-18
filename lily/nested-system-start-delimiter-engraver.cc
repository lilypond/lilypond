/*
  new-system-start-delimiter-engraver.cc -- implement
  Nested_system_start_delimiter_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "side-position-interface.hh"
#include "system-start-delimiter.hh"
#include "engraver.hh"
#include "staff-symbol.hh"
#include "pointer-group-interface.hh"
#include "paper-column.hh"
#include "output-def.hh"
#include "spanner.hh"

class Nested_system_start_delimiter_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Nested_system_start_delimiter_engraver);

protected:
  Spanner *delimiter_;
  
  DECLARE_ACKNOWLEDGER (system_start_delimiter);
  DECLARE_ACKNOWLEDGER (staff_symbol);

  void process_music ();
  virtual void finalize ();
};

Nested_system_start_delimiter_engraver::Nested_system_start_delimiter_engraver ()
{
  delimiter_ = 0;
}

bool
add_staff_to_hierarchy (SCM hierarchy, SCM grob)
{
  for (SCM s = hierarchy; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);

      if (unsmob_grob (entry))
	;
      else if (scm_is_pair (entry))
	{
	  bool success = add_staff_to_hierarchy (entry, grob);
	  if (success)
	    return success;
	}
      else
	{
	  scm_set_car_x (s, grob);
	  return true;
	}
    }

  return false;
}

void
Nested_system_start_delimiter_engraver::process_music ()
{
  if (!delimiter_)
    {
      delimiter_ = make_spanner ("NestedSystemStartDelimiter", SCM_EOL);
      SCM hierarchy = get_property ("systemStartDelimiterHierarchy");
      
      
      delimiter_->set_object ("staff-hierarchy", ly_deep_copy (hierarchy));
      delimiter_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));
    }
}

void
Nested_system_start_delimiter_engraver::finalize ()
{
  if (delimiter_)
    delimiter_->set_bound (RIGHT,
			   unsmob_grob (get_property ("currentCommandColumn")));
}

void
Nested_system_start_delimiter_engraver::acknowledge_staff_symbol (Grob_info inf)
{
  Grob *staff = inf.grob();
  SCM hier = delimiter_->get_object ("staff-hierarchy");
  bool succ = add_staff_to_hierarchy (hier, staff->self_scm ());

  if (!succ)
    {
      hier = scm_append_x (scm_list_2 (hier,
				       scm_list_1 (staff->self_scm ())));

      delimiter_->set_object ("staff-hierarchy", hier);
    }
}


void
Nested_system_start_delimiter_engraver::acknowledge_system_start_delimiter (Grob_info inf)
{
  Side_position_interface::add_support (inf.grob (), delimiter_);
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Nested_system_start_delimiter_engraver, staff_symbol);
ADD_ACKNOWLEDGER (Nested_system_start_delimiter_engraver, system_start_delimiter);

ADD_TRANSLATOR (Nested_system_start_delimiter_engraver,
		/* doc */ "Creates a system start delimiter (ie. SystemStart@{Bar, Brace, Bracket@} spanner",
		/* create */ "NestedSystemStartDelimiter",
		/* accept */ "",
		/* read */ "systemStartDelimiterHierarchy",
		/* write */ "");
