/*   
  pitch-squash-grav.cc --  implement Pitch_squash_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "staff-symbol-referencer.hh"
#include "note-head.hh"
#include "rhythmic-head.hh"
#include "engraver.hh"

class Pitch_squash_engraver : public Engraver {
public:
  TRANSLATOR_DECLARATIONS(Pitch_squash_engraver);
  virtual void acknowledge_grob (Grob_info);
};


void
Pitch_squash_engraver::acknowledge_grob (Grob_info i)
{
  SCM newpos = get_property ("squashedPosition");
  if (Note_head::has_interface (i.grob_))
    {
      i.grob_->set_grob_property ("staff-position", newpos);
    }
}



Pitch_squash_engraver::Pitch_squash_engraver()
{
}

ENTER_DESCRIPTION(Pitch_squash_engraver,
/* descr */       "Treat all pitches as middle C.  Note that the notes move, but
the locations of accidentals stay the same. 
Set the position field of all note heads to zero. This useful for
making a single line staff that demonstrates the rhythm of a melody.",
/* creats*/       "",
/* accepts */     "general-music",
/* acks  */      "note-head-interface",
/* reads */       "squashedPosition",
/* write */       "");
