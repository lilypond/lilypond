/*   
  font-size-engraver.cc --  implement 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "grob.hh"
#include "engraver.hh"

class Font_size_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS(Translator);
  Font_size_engraver ();
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

  if (gh_number_p (sz)
      && gh_scm2int (sz)
      && !gh_number_p (gi.elem_l_->get_grob_property ("font-relative-size")))
    {
      gi.elem_l_->set_grob_property ("font-relative-size", sz);
    }
}

ADD_THIS_TRANSLATOR(Font_size_engraver);
