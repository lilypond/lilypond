/*
  stem-grav.cc -- implement Stem_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "staff-symbol-referencer.hh"
#include "rhythmic-head.hh"
#include "stem.hh"
#include "musical-request.hh"
#include "misc.hh"
#include "stem-tremolo.hh"
#include "item.hh"
#include "translator-group.hh"
#include "engraver.hh"



/**
  Make stems upon receiving noteheads.
 */
class Stem_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS(Stem_engraver);
protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual bool try_music (Music*);
  
private:
  Grob  *stem_p_;
  Grob *tremolo_p_;
  Rhythmic_req *rhythmic_req_l_;
  Tremolo_req* tremolo_req_l_;
};

Stem_engraver::Stem_engraver ()
{
  tremolo_req_l_ = 0;
  stem_p_ = 0;
  tremolo_p_ = 0;
  rhythmic_req_l_ =0;
}


void
Stem_engraver::acknowledge_grob (Grob_info i)
{
  Grob* h = i.grob_l_;
  if (Rhythmic_head::has_interface (h))
    {
      if (Rhythmic_head::stem_l (h))
	return;

      /* Reverted to the old method so chord tremolos work again. /MB 
      */
      int duration_log = 0;
      Rhythmic_req *rhythmic_req = dynamic_cast <Rhythmic_req *> (i.music_cause ()); 
      if (rhythmic_req)
	duration_log = unsmob_duration (rhythmic_req->get_mus_property ("duration"))-> duration_log (); 
      
      if (!stem_p_) 
	{
	  stem_p_ = new Item (get_property ("Stem"));

	  stem_p_->set_grob_property ("duration-log", gh_int2scm (duration_log));

	  if (tremolo_req_l_)
	    {
	      /*
		Stem tremolo is never applied to a note by default,
		is must me requested.  But there is a default for the
		tremolo value:

		   c4:8 c c:

		the first and last (quarter) note bothe get one tremolo flag.
	       */
	      int requested_type = gh_scm2int (tremolo_req_l_->get_mus_property ("tremolo-type"));
	      
	      SCM f = get_property ("tremoloFlags");
	      if (!requested_type && gh_number_p (f))
		requested_type = gh_scm2int (f);
	      else
		daddy_trans_l_->set_property ("tremoloFlags", gh_int2scm (requested_type));

	      if (requested_type)
		{
		  tremolo_p_ = new Item (get_property ("StemTremolo"));
		  announce_grob(tremolo_p_, tremolo_req_l_->self_scm());

		  /*
		    The number of tremolo flags is the number of flags of
		    the tremolo-type minus the number of flags of the note
		    itself.
		   */
		  int tremolo_flags = intlog2 (requested_type) - 2
		    - (duration_log > 2 ? duration_log - 2 : 0);
		  if (tremolo_flags < 0)
		    tremolo_flags = 0;
		  tremolo_p_->set_grob_property ("flag-count",
						gh_int2scm (tremolo_flags));
		  tremolo_p_->set_parent (stem_p_, X_AXIS);
		}
	    }

	  /*
	    We announce the cause of the head as cause of the stem.
	    The stem needs a rhythmic structure to fit it into a beam.  */
	  announce_grob(stem_p_, i.music_cause ()->self_scm());
	}

      if (Stem::duration_log (stem_p_) != duration_log)
	{
	  i.music_cause ()->origin ()->warning (_f ("Adding note head to incompatible stem (type = %d)", 1 <<  Stem::duration_log (stem_p_)));
	}

      Stem::add_head (stem_p_,h);
    }
}

void
Stem_engraver::stop_translation_timestep ()
{
  if (tremolo_p_)
    {
      Stem_tremolo::set_stem (tremolo_p_, stem_p_);
      typeset_grob (tremolo_p_);
      tremolo_p_ = 0;
    }

  if (stem_p_)
    {
      SCM prop = get_property ("stemLeftBeamCount");
      if (gh_number_p (prop))
	{
	  Stem::set_beaming (stem_p_,gh_scm2int (prop),LEFT);
	  daddy_trans_l_->unset_property (ly_symbol2scm ("stemLeftBeamCount"));
	}
      prop = get_property ("stemRightBeamCount");
      if (gh_number_p (prop))
	{
	  Stem::set_beaming (stem_p_,gh_scm2int (prop), RIGHT);
	  daddy_trans_l_->unset_property (ly_symbol2scm ("stemRightBeamCount"));
	}

      
      // UGH. Should mark non-forced instead.
      /*
	 aargh: I don't get it.  direction is being set (and then set
	 to forced), if we have a Chord_tremolo.
       */

      /*
	Why the separate check for forced directions? --hwn.

	(docme)
       */
      SCM dir = stem_p_->get_grob_property ("direction");
      if (gh_number_p (dir) && to_dir (dir))
	{
	  stem_p_->set_grob_property ("dir-forced", SCM_BOOL_T);	  
	}

      typeset_grob (stem_p_);
      stem_p_ = 0;
    }


  tremolo_req_l_ = 0;
}

bool
Stem_engraver::try_music (Music* r)
{
  if (Tremolo_req* a = dynamic_cast <Tremolo_req *> (r))
    {
      tremolo_req_l_ = a;
      return true;
    }
  return false;
}

ENTER_DESCRIPTION(Stem_engraver,
/* descr */       "Create stems and single-stem tremolos.  It also works together with
the beam engraver for overriding beaming.",
/* creats*/       "Stem StemTremolo",
/* acks  */       "rhythmic-head-interface",
/* reads */       "tremoloFlags stemLeftBeamCount stemRightBeamCount",
/* write */       "");
