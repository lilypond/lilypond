/*   
  melisma-engraver.cc --  implement Melisma_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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


/*
  HUH ?

  how's this supposed to work?
 */
bool
Melisma_engraver::try_music (Music *m) 
{
  SCM plain (get_property ("melismaBusy"));
  SCM slur (get_property ("slurMelismaBusy"));
  SCM tie (get_property ("tieMelismaBusy"));
  SCM beam (get_property ("beamMelismaBusy"));
      
  if ((to_boolean (plain))
      || (to_boolean (slur))
      || (to_boolean (tie))
      || (to_boolean (beam)))
    {

      daddy_trans_->set_property ("melismaEngraverBusy",SCM_BOOL_T);
      return true;
    }
  else
    {
      daddy_trans_->set_property ("melismaEngraverBusy",SCM_BOOL_F);
      return false;
    }
}

Melisma_engraver::Melisma_engraver()
{
}

ENTER_DESCRIPTION(Melisma_engraver,
/* descr */       "",
/* creats*/       "",
/* accepts */     "melisma-playing-event",
/* acks  */      "",
/* reads */       "melismaBusy slurMelismaBusy tieMelismaBusy beamMelismaBusy",
/* write */       "");
