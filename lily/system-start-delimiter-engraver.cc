/*   
  system-start-delimiter-engraver.cc -- implement System_start_delimiter_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  VIRTUAL_COPY_CONS(Translator);
  System_start_delimiter_engraver();

protected:
  Spanner * delim_;
  virtual void acknowledge_grob (Grob_info);
  virtual void initialize ();
  virtual void finalize ();
};

ADD_THIS_TRANSLATOR(System_start_delimiter_engraver);

void
System_start_delimiter_engraver::acknowledge_grob (Grob_info inf)
{
  if (Staff_symbol::has_interface (inf.elem_l_))
    {
      /*
	don't add as Axis_group_interface::add_element (delim_, ),
	because that would set the parent as well */
	  
      Pointer_group_interface::add_element (delim_, "elements", inf.elem_l_);
    }
  else if (System_start_delimiter::has_interface (inf.elem_l_))
    {
      SCM gl = inf.elem_l_->get_grob_property ("glyph");
      SCM my_gl = delim_->get_grob_property ("glyph");

      /*
	UGH UGH
       */
      if (gh_symbol_p (gl) && gl  == ly_symbol2scm ("brace")
	  && gh_symbol_p (my_gl) && my_gl == ly_symbol2scm ("bracket"))
	inf.elem_l_->translate_axis ( -1.0, X_AXIS); // ugh
    }

}

System_start_delimiter_engraver::System_start_delimiter_engraver()
{
  delim_ = 0;
}

void
System_start_delimiter_engraver::initialize()
{
  delim_ = new Spanner (get_property ("SystemStartDelimiter"));

  delim_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));


  announce_grob (delim_,0);
}

void
System_start_delimiter_engraver::finalize ()
{
  delim_->set_bound (RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
  typeset_grob (delim_);
}

