/*   
  pitch-squash-grav.cc --  implement Pitch_squash_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "staff-symbol-referencer.hh"
#include "note-head.hh"
#include "rhythmic-head.hh"
#include "engraver.hh"

class Pitch_squash_engraver : public Engraver {
public:
  TRANSLATOR_DECLARATIONS (Pitch_squash_engraver);
  virtual void acknowledge_grob (Grob_info);
};


void
Pitch_squash_engraver::acknowledge_grob (Grob_info i)
{
  SCM newpos = get_property ("squashedPosition");
  if (scm_is_number (newpos)
      && Note_head::has_interface (i.grob_))
    {
      i.grob_->set_property ("staff-position", newpos);
    }
}



Pitch_squash_engraver::Pitch_squash_engraver ()
{
}

ADD_TRANSLATOR (Pitch_squash_engraver,
/* descr */       
		  "Set the vertical position of noteheads to "
		  "@code{squashedPosition}, if that "
		  "property is set. "
		  "This can be used to make  a single line staff "
		  "demonstrating the rhythm of a melody.",

/* creats*/       "",
/* accepts */     "",
/* acks  */      "note-head-interface",
/* reads */       "squashedPosition",
/* write */       "");
