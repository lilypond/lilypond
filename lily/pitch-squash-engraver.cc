/*   
  pitch-squash-grav.cc --  implement Pitch_squash_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "staff-symbol-referencer.hh"
#include "note-head.hh"
#include "rhythmic-head.hh"
#include "engraver.hh"

class Pitch_squash_engraver : public Engraver {
public:
  VIRTUAL_COPY_CONS (Translator);
  virtual void acknowledge_grob (Grob_info);
};


void
Pitch_squash_engraver::acknowledge_grob (Grob_info i)
{
  SCM newpos = get_property ("squashedPosition");
  if (Note_head::has_interface (i.elem_l_))
    {
      i.elem_l_->set_grob_property ("staff-position", newpos);
    }
}

ADD_THIS_TRANSLATOR (Pitch_squash_engraver);


