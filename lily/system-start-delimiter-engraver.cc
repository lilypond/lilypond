/*
  system-start-delimiter-engraver.cc -- implement System_start_delimiter_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "system-start-delimiter.hh"
#include "engraver.hh"
#include "staff-symbol.hh"
#include "group-interface.hh"
#include "paper-column.hh"
#include "output-def.hh"
#include "spanner.hh"

class System_start_delimiter_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (System_start_delimiter_engraver);

protected:
  Spanner *delim_;
  virtual void acknowledge_grob (Grob_info);
  virtual void process_music ();
  virtual void finalize ();
};

void
System_start_delimiter_engraver::acknowledge_grob (Grob_info inf)
{
  if (Staff_symbol::has_interface (inf.grob ()))
    {
      /*
	don't add as Axis_group_interface::add_element (delim_, ),
	because that would set the parent as well */

      Pointer_group_interface::add_grob (delim_, ly_symbol2scm ("elements"), inf.grob ());
    }
  else if (System_start_delimiter::has_interface (inf.grob ()))
    {
      SCM gl = inf.grob ()->get_property ("glyph");
      SCM my_gl = delim_->get_property ("glyph");

      /*
	UGH UGH
      */
      if (scm_is_string (gl) && ly_c_equal_p (gl, scm_makfrom0str ("brace"))
	  && scm_is_string (my_gl) && ly_c_equal_p (my_gl, scm_makfrom0str ("bracket")))
	inf.grob ()->translate_axis (-0.8, X_AXIS); // ugh
      else if (scm_is_string (gl) && ly_c_equal_p (gl, scm_makfrom0str ("bracket"))
	       && scm_is_string (my_gl) && ly_c_equal_p (my_gl, scm_makfrom0str ("bracket")))
	{
	  inf.grob ()->translate_axis (-0.8, X_AXIS); // ugh
	  inf.grob ()->set_property ("arch-height",
				   scm_make_real (scm_to_double (inf.grob ()->get_property
								 ("arch-height")) + 0.5));
	}
    }
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
    {
      delim_->set_bound (RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
    }
}

ADD_TRANSLATOR (System_start_delimiter_engraver,
		/* descr */ "Creates a system start delimiter (ie. SystemStart@{Bar, Brace, Bracket@} spanner",
		/* creats*/ "SystemStartBar SystemStartBrace SystemStartBracket",
		/* accepts */ "",
		/* acks  */ "system-start-delimiter-interface staff-symbol-interface",
		/* reads */ "systemStartDelimiter",
		/* write */ "");
