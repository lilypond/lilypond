/*
  system-start-delimiter-engraver.cc -- implement System_start_delimiter_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "system-start-delimiter.hh"
#include "engraver.hh"
#include "staff-symbol.hh"
#include "pointer-group-interface.hh"
#include "paper-column.hh"
#include "output-def.hh"
#include "spanner.hh"

class System_start_delimiter_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (System_start_delimiter_engraver);

protected:
  Spanner *delim_;
  DECLARE_ACKNOWLEDGER (system_start_delimiter);
  DECLARE_ACKNOWLEDGER (staff_symbol);

  void process_music ();
  virtual void finalize ();
};

void
System_start_delimiter_engraver::acknowledge_staff_symbol (Grob_info inf)
{
  /*
    don't add as Axis_group_interface::add_element (delim_, ),
    because that would set the parent as well */

  Pointer_group_interface::add_grob (delim_, ly_symbol2scm ("elements"), inf.grob ());
}

void
System_start_delimiter_engraver::acknowledge_system_start_delimiter (Grob_info inf)
{
  SCM gl = inf.grob ()->get_property ("glyph");
  SCM my_gl = delim_->get_property ("glyph");

  /*
    UGH UGH
  */
  if (scm_is_string (gl) && ly_is_equal (gl, scm_makfrom0str ("brace"))
      && scm_is_string (my_gl) && ly_is_equal (my_gl, scm_makfrom0str ("bracket")))
    add_offset_callback (inf.grob (), scm_from_double (-0.8), X_AXIS);
  else if (scm_is_string (gl) && ly_is_equal (gl, scm_makfrom0str ("bracket"))
	   && scm_is_string (my_gl) && ly_is_equal (my_gl, scm_makfrom0str ("bracket")))
    add_offset_callback (inf.grob (), scm_from_double (-0.8), X_AXIS);
}

System_start_delimiter_engraver::System_start_delimiter_engraver ()
{
  delim_ = 0;
}

void
System_start_delimiter_engraver::process_music ()
{
  if (!delim_)
    {
      SCM delim_name = get_property ("systemStartDelimiter");
      delim_ = make_spanner_from_properties (this, delim_name, SCM_EOL,
					     ly_symbol2string (delim_name).to_str0 ());

      delim_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));
    }
}
void
System_start_delimiter_engraver::finalize ()
{
  if (delim_)
    delim_->set_bound (RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
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
