/*   
  font-size-grav.cc --  implement Font_size_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "font-size-engraver.hh"
#include "score-element.hh"
#include "lily-guile.hh"

Font_size_engraver::Font_size_engraver ()
{
  size_ = SCM_EOL;
}

void
Font_size_engraver::do_process_requests ()
{
  SCM s (get_property ("fontSize", 0));
  
  if (gh_number_p(s))
    {
      size_ = gh_scm2int (s);
    }
  else
    {
      size_ = SCM_EOL;
    }
}

void
Font_size_engraver::acknowledge_element (Score_element_info e)
{
  if (size_ != SCM_EOL
      && e.elem_l_->get_elt_property (fontsize_scm_sym) == SCM_BOOL_F)
    {
      e.elem_l_->set_elt_property (fontsize_scm_sym, size_);
    }
}

ADD_THIS_TRANSLATOR (Font_size_engraver);

