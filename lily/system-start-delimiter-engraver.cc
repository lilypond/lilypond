/*   
  system-start-delimiter-engraver.cc -- implement System_start_delimiter_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "system-start-delimiter.hh"
#include "engraver.hh"
#include "staff-symbol.hh"
#include "group-interface.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "spanner.hh"

class System_start_delimiter_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS(System_start_delimiter_engraver);

protected:
  Spanner * delim_;
  virtual void acknowledge_grob (Grob_info);
  virtual void initialize ();
  virtual void finalize ();
};



void
System_start_delimiter_engraver::acknowledge_grob (Grob_info inf)
{
  if (Staff_symbol::has_interface (inf.grob_l_))
    {
      /*
	don't add as Axis_group_interface::add_element (delim_,),
	because that would set the parent as well */
	  
      Pointer_group_interface::add_grob (delim_, ly_symbol2scm ("elements"),  inf.grob_l_);
    }
  else if (System_start_delimiter::has_interface (inf.grob_l_))
    {
      SCM gl = inf.grob_l_->get_grob_property ("glyph");
      SCM my_gl = delim_->get_grob_property ("glyph");

      /*
	UGH UGH
       */
      if (gh_string_p (gl) && gh_equal_p (gl, ly_str02scm  ("brace"))
	  && gh_string_p (my_gl) && gh_equal_p (my_gl, ly_str02scm  ("bracket")))
	inf.grob_l_->translate_axis (-0.8, X_AXIS); // ugh
      else if (gh_string_p (gl) && gh_equal_p (gl, ly_str02scm  ("bracket"))
	       && gh_string_p (my_gl) && gh_equal_p (my_gl, ly_str02scm  ("bracket")))
       {
         inf.grob_l_->translate_axis ( -0.8, X_AXIS); // ugh
         inf.grob_l_->set_grob_property ("arch-height",
           gh_double2scm(gh_scm2double(inf.grob_l_->get_grob_property
                                       ("arch-height"))+0.5));
       }
    }
}

System_start_delimiter_engraver::System_start_delimiter_engraver ()
{
  delim_ = 0;
}

void
System_start_delimiter_engraver::initialize ()
{
  SCM delim_name =get_property ("systemStartDelimiter");
  delim_ = new Spanner (internal_get_property (delim_name));

  delim_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));
  announce_grob (delim_, SCM_EOL);
}

void
System_start_delimiter_engraver::finalize ()
{
  delim_->set_bound (RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
  typeset_grob (delim_);
}

ENTER_DESCRIPTION(System_start_delimiter_engraver,
/* descr */       "Creates a system start delimiter (ie. SystemStart@{Bar,Brace,Bracket@} spanner",
/* creats*/       "SystemStartBar SystemStartBrace SystemStartBracket",
/* acks  */       "system-start-delimiter-interface staff-symbol-interface",
/* reads */       "systemStartDelimiter",
/* write */       "");
