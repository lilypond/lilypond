/*
  stem-grav.cc -- implement Stem_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "staff-symbol-referencer.hh"
#include "rhythmic-head.hh"
#include "stem.hh"
#include "event.hh"
#include "misc.hh"
#include "stem-tremolo.hh"
#include "item.hh"
#include "context.hh"

#include "engraver.hh"



/**
  Make stems upon receiving noteheads.
 */
class Stem_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Stem_engraver);
protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual bool try_music (Music*);
  
private:
  Grob  *stem_;
  Grob *tremolo_;
  Music *rhythmic_ev_;
  Music* tremolo_ev_;
};

Stem_engraver::Stem_engraver ()
{
  tremolo_ev_ = 0;
  stem_ = 0;
  tremolo_ = 0;
  rhythmic_ev_ =0;
}


void
Stem_engraver::acknowledge_grob (Grob_info i)
{
  Grob* h = i.grob_;
  if (Rhythmic_head::has_interface (h))
    {
      if (Rhythmic_head::get_stem (h))
	return;

      /* Reverted to the old method so chord tremolos work again. /MB 
      */
      int duration_log = 0;

      Music * m = i.music_cause ();
      if (m->is_mus_type ("rhythmic-event"))
	duration_log = unsmob_duration (m->get_property ("duration"))-> duration_log (); 
      
      if (!stem_) 
	{
	  stem_ = make_item ("Stem");

	  stem_->set_property ("duration-log", scm_int2num (duration_log));

	  if (tremolo_ev_)
	    {
	      /*
		Stem tremolo is never applied to a note by default,
		is must me evuested.  But there is a default for the
		tremolo value:

		   c4:8 c c:

		the first and last (quarter) note bothe get one tremolo flag.
	       */
	      int requested_type = ly_scm2int (tremolo_ev_->get_property ("tremolo-type"));
	      SCM f = get_property ("tremoloFlags");
	      if (!requested_type)
		if (ly_c_number_p (f))
		  requested_type = ly_scm2int (f);
		else
		  requested_type = 8; 
	      else
		get_parent_context ()->set_property ("tremoloFlags", scm_int2num (requested_type));

	      int tremolo_flags = intlog2 (requested_type) - 2
		- (duration_log > 2 ? duration_log - 2 : 0);
	      if (tremolo_flags <= 0)
		{
		  tremolo_ev_->origin ()->warning (_("tremolo duration is too long"));
		  tremolo_flags = 0;
		}

	      if (tremolo_flags)
		{
		  tremolo_ = make_item ("StemTremolo");
		  announce_grob (tremolo_, tremolo_ev_->self_scm ());

		  /*
		    The number of tremolo flags is the number of flags of
		    the tremolo-type minus the number of flags of the note
		    itself.
		   */
		  tremolo_->set_property ("flag-count",
					       scm_int2num (tremolo_flags));
		  tremolo_->set_parent (stem_, X_AXIS);
		  stem_->set_property ("tremolo-flag", tremolo_->self_scm ());
		  tremolo_->set_property ("stem",
					  stem_->self_scm ());
		}
	    }

	  /*
	    We announce the cause of the head as cause of the stem.
	    The stem needs a rhythmic structure to fit it into a beam.  */
	  announce_grob (stem_, i.music_cause ()->self_scm ());
	}

      if (Stem::duration_log (stem_) != duration_log)
	{
	  i.music_cause ()->origin ()->warning (_f ("Adding note head to incompatible stem (type = %d)", 1 <<  Stem::duration_log (stem_))
						+ _f ("Don't you want polyphonic voices instead?")
						);
	}

      Stem::add_head (stem_,h);
    }
}

void
Stem_engraver::stop_translation_timestep ()
{
  if (tremolo_)
    {
      typeset_grob (tremolo_);
      tremolo_ = 0;
    }

  if (stem_)
    {
      /*
	toDO: junk these properties.
       */
      SCM prop = get_property ("stemLeftBeamCount");
      if (ly_c_number_p (prop))
	{
	  Stem::set_beaming (stem_,ly_scm2int (prop),LEFT);
	  get_parent_context ()->unset_property (ly_symbol2scm ("stemLeftBeamCount"));
	}
      prop = get_property ("stemRightBeamCount");
      if (ly_c_number_p (prop))
	{
	  Stem::set_beaming (stem_,ly_scm2int (prop), RIGHT);
	  get_parent_context ()->unset_property (ly_symbol2scm ("stemRightBeamCount"));
	}

      typeset_grob (stem_);
      stem_ = 0;
    }


  tremolo_ev_ = 0;
}

bool
Stem_engraver::try_music (Music* r)
{
  if (r->is_mus_type ("tremolo-event"))
    {
      tremolo_ev_ = r;
      return true;
    }
  return false;
}

ENTER_DESCRIPTION (Stem_engraver,
/* descr */       "Create stems and single-stem tremolos.  It also works together with "
"the beam engraver for overriding beaming.",
/* creats*/       "Stem StemTremolo",
/* accepts */     "tremolo-event",
/* acks  */      "rhythmic-head-interface",
/* reads */       "tremoloFlags stemLeftBeamCount stemRightBeamCount",
/* write */       "");
