/*   
  melisma-engraver.cc --  implement Melisma_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "grob.hh"
#include "context.hh"

/**
   Signal existence of melismas.
 */
class Melisma_translator : public Translator
{
public:
  TRANSLATOR_DECLARATIONS (Melisma_translator);
protected:
  virtual bool try_music (Music *);
  virtual void process_music ();
  virtual void start_translation_timestep ();
  Music * event_;
};


bool
Melisma_translator::try_music (Music *m) 
{
  if (m->is_mus_type ("melisma-playing-event"))
    {
      return melisma_busy (context ());
    }
  else if (m->is_mus_type ("melisma-span-event"))
    {
      event_ = m;
      return true;
    }

  return false;
}

void
Melisma_translator::process_music ()
{
  if (event_)
    {
      SCM sd = event_->get_property ("span-direction");
      Direction d = to_dir (sd);
      if (d == START)
	context ()->set_property ("melismaBusy", SCM_BOOL_T);
      else
	context ()->unset_property (ly_symbol2scm ("melismaBusy"));
    }
      
}

void
Melisma_translator::start_translation_timestep ()
{
  event_ = 0;
}
  
Melisma_translator::Melisma_translator ()
{
  event_ = 0;
}

ADD_TRANSLATOR (Melisma_translator,
/* descr */       "This translator collects melisma information about ties, beams, and user settings (@code{melismaBusy}, and signals it to the @code{\addlyrics} code.  ",
/* creats*/       "",
/* accepts */     "melisma-playing-event melisma-span-event",
/* acks  */      "",
/* reads */       "melismaBusy melismaBusyProperties slurMelismaBusy tieMelismaBusy beamMelismaBusy",
/* write */       "");
