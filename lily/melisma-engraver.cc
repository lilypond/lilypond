/*   
  melisma-engraver.cc --  implement Melisma_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
Melisma_engraver::try_music (Music *) 
{
  /*
    This can only be melisma-playing-event.
   */
  return melisma_busy (this);
}

Melisma_engraver::Melisma_engraver()
{
}

ENTER_DESCRIPTION(Melisma_engraver,
/* descr */       "This engraver collects melisma information about ties, beams, and user settings (@code{melismaBusy}, and signals it to the @code{\addlyrics} code.  ",
/* creats*/       "",
/* accepts */     "melisma-playing-event",
/* acks  */      "",
/* reads */       "melismaBusy melismaBusyProperties slurMelismaBusy tieMelismaBusy beamMelismaBusy",
/* write */       "");
