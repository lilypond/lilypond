
/*   
  melisma-performer.cc --  implement Melisma_performer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

/*
  copy of melisma-engraver - see there.
 */
#include "performer.hh"
#include "event.hh"
#include "grob.hh"
#include "translator-group.hh"

/**
   Signal existence of melismas.
 */
class Melisma_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS(Melisma_performer);
  bool try_music (Music *);
};


bool
Melisma_performer::try_music (Music *) 
{
  /*
    This can only be melisma-playing-event.
   */
  return melisma_busy (this);
}

Melisma_performer::Melisma_performer()
{
}

ENTER_DESCRIPTION(Melisma_performer,
/* descr */       "This performer collects melisma information about ties, beams, and user settings (@code{melismaBusy}, and signals it to the @code{\addlyrics} code.  ",
/* creats*/       "",
/* accepts */     "melisma-playing-event",
/* acks  */      "",
/* reads */       "melismaBusy melismaBusyProperties slurMelismaBusy tieMelismaBusy beamMelismaBusy",
/* write */       "");
