/*   
  font-size-engraver.cc --  implement 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2001--2002  Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "grob.hh"
#include "engraver.hh"

class Font_size_engraver : public Engraver
{
  
  TRANSLATOR_DECLARATIONS(Font_size_engraver);
protected:
  virtual void acknowledge_grob (Grob_info gi);
private:
};


Font_size_engraver::Font_size_engraver ()
{

}

/*
  TODO: use fontSize = NUMBER as a scaling constant: find the closest
  integer design size, and use magnification to do the fractional bit.
*/
void
Font_size_engraver::acknowledge_grob (Grob_info gi)
{
  SCM sz = get_property ("fontSize");

  if (gh_number_p (sz)
      && gh_scm2int (sz)
      && !gh_number_p (gi.grob_->get_grob_property ("font-relative-size")))
    {
      gi.grob_->set_grob_property ("font-relative-size", sz);
    }
}



ENTER_DESCRIPTION(Font_size_engraver,
/* descr */       "Puts fontSize into font-relative-size grob property.",
/* creats*/       "",
/* accepts */     "",
/* acks  */      "font-interface",
/* reads */       "fontSize",
/* write */       "");
