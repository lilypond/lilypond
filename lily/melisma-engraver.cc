/*   
  melisma-engraver.cc --  implement Melisma_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "event.hh"
#include "grob.hh"
#include "translator-group.hh"

/**
   Signal existence of melismas.
 */
class Melisma_engraver:public Engraver
{
public:
  TRANSLATOR_DECLARATIONS(Melisma_engraver);
  bool try_music (Music *);
};


bool
Melisma_engraver::try_music (Music *m) 
{
  SCM melisma_properties = get_property ("melismaBusyProperties");
  bool busy = false;

  for (; gh_pair_p (melisma_properties);
       melisma_properties = gh_cdr (melisma_properties))

    busy = busy || to_boolean (internal_get_property (gh_car (melisma_properties)));

  /*
    for the phrasing engraver we also need this.
   */
  daddy_trans_->set_property ("melismaEngraverBusy",gh_bool2scm (busy));
  return busy;
}

Melisma_engraver::Melisma_engraver()
{
}

ENTER_DESCRIPTION(Melisma_engraver,
/* descr */       "",
/* creats*/       "",
/* accepts */     "melisma-playing-event",
/* acks  */      "",
/* reads */       "melismaBusy melismaBusyProperties slurMelismaBusy tieMelismaBusy beamMelismaBusy",
/* write */       "melismaEngraverBusy");
