/*   
  font-size-engraver.cc --  implement Font_size_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2001--2004  Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "grob.hh"
#include "engraver.hh"

class Font_size_engraver : public Engraver
{
  
  TRANSLATOR_DECLARATIONS (Font_size_engraver);
protected:
  virtual void acknowledge_grob (Grob_info gi);
private:
};


Font_size_engraver::Font_size_engraver ()
{

}

void
Font_size_engraver::acknowledge_grob (Grob_info gi)
{
  SCM sz = get_property ("fontSize");

  /*
    We only want to process a grob once.
   */
  if (gi.origin_trans_->daddy_context_ != daddy_context_)
    return ;
  
  if (ly_number_p (sz) && ly_scm2double (sz))
    {
      Real font_size = ly_scm2double (sz);
      
      font_size +=  robust_scm2double (gi.grob_->get_property ("font-size"), 0);
      gi.grob_->set_property ("font-size", scm_make_real (font_size));
    }
}


ENTER_DESCRIPTION (Font_size_engraver,
/* descr */       "Puts fontSize into font-relative-size grob property.",
/* creats*/       "",
/* accepts */     "",
/* acks  */      "font-interface",
/* reads */       "fontSize",
/* write */       "");
