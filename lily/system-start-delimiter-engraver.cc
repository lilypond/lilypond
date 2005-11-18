/*
  system-start-delimiter-engraver.cc -- implement System_start_delimiter_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"

#include "system-start-delimiter.hh"
#include "staff-symbol.hh"
#include "pointer-group-interface.hh"
#include "paper-column.hh"
#include "output-def.hh"
#include "spanner.hh"
#include "side-position-interface.hh"

class System_start_delimiter_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (System_start_delimiter_engraver);

protected:
  Spanner *delimiter_;
  DECLARE_ACKNOWLEDGER (system_start_delimiter);
  DECLARE_ACKNOWLEDGER (staff_symbol);

  void process_music ();
  virtual void finalize ();
};

void
System_start_delimiter_engraver::acknowledge_staff_symbol (Grob_info inf)
{
  Pointer_group_interface::add_grob (delimiter_, ly_symbol2scm ("elements"), inf.grob ());
}


void
System_start_delimiter_engraver::acknowledge_system_start_delimiter (Grob_info inf)
{
  Side_position_interface::add_support (inf.grob (), delimiter_);
}

System_start_delimiter_engraver::System_start_delimiter_engraver ()
{
  delimiter_ = 0;
}

void
System_start_delimiter_engraver::process_music ()
{
  if (!delimiter_)
    {
      SCM delimiter_name = get_property ("systemStartDelimiter");
      delimiter_ = make_spanner_from_properties (this, delimiter_name, SCM_EOL,
						 ly_symbol2string (delimiter_name).to_str0 ());

      delimiter_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));
    }
}
void
System_start_delimiter_engraver::finalize ()
{
  if (delimiter_)
    delimiter_->set_bound (RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
}

#include "translator.icc"

ADD_ACKNOWLEDGER (System_start_delimiter_engraver, system_start_delimiter);
ADD_ACKNOWLEDGER (System_start_delimiter_engraver, staff_symbol);

ADD_TRANSLATOR (System_start_delimiter_engraver,
		/* doc */ "Creates a system start delimiter (ie. SystemStart@{Bar, Brace, Bracket@} spanner",
		/* create */ "SystemStartBar SystemStartBrace SystemStartBracket",
		/* accept */ "",
		/* read */ "systemStartDelimiter",
		/* write */ "");
