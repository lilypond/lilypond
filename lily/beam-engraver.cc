/*   
  beam-engraver.cc --  implement Beam_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "engraver-group-engraver.hh"
#include "engraver.hh"
#include "musical-request.hh"
#include "beam.hh"
#include "stem.hh"
#include "warn.hh"
#include "beaming.hh"
#include "score-engraver.hh"
#include "rest.hh"
#include "drul-array.hh"
#include "item.hh"
#include "spanner.hh"

class Beam_engraver : public Engraver
{
protected:  
  Drul_array<Span_req*> reqs_drul_;
  
  Spanner *finished_beam_;
  Spanner *beam_;
  Span_req * prev_start_req_;

  Beaming_info_list * beam_info_;
  Beaming_info_list * finished_beam_info_;  

  /// location  within measure where beam started.
  Moment beam_start_location_;

  /// moment (global time) where beam started.
  Moment beam_start_mom_;

  bool subdivide_beams_;
  Moment beat_length_;

  void typeset_beam ();
  void set_melisma (bool);

  Moment last_stem_added_at_;
  


  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void finalize ();

  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music*);
  virtual void process_music ();

  virtual bool valid_start_moment();
  virtual bool valid_end_moment ();
  
public:
  TRANSLATOR_DECLARATIONS(  Beam_engraver );
};


/*
  Hmm. this isn't necessary, since grace beams and normal beams are
  always nested.
 */
bool
Beam_engraver::valid_start_moment()
{
  Moment n = now_mom ();

  return n.grace_part_ == Rational (0);
}

bool
Beam_engraver::valid_end_moment()
{
  return last_stem_added_at_.grace_part_ == Rational(0);
}


Beam_engraver::Beam_engraver ()
{
  beam_ = 0;
  finished_beam_ =0;
  finished_beam_info_=0;
  beam_info_ =0;
  reqs_drul_[LEFT] = reqs_drul_[RIGHT] =0;
  prev_start_req_ =0;
  
}

bool
Beam_engraver::try_music (Music *m)
{
  if (Span_req * c = dynamic_cast<Span_req*> (m))
    {
      if (scm_equal_p (c->get_mus_property ("span-type"),
		       scm_makfrom0str ("abort")) == SCM_BOOL_T)
	{
	  reqs_drul_[START] = 0;
	  reqs_drul_[STOP] = 0;
	  if (beam_)
	    beam_->suicide ();
	  beam_ = 0;
	}
      else if (scm_equal_p (c->get_mus_property ("span-type"),
			    scm_makfrom0str ("beam")) == SCM_BOOL_T)
	{
	  Direction d =c->get_span_dir ();


      	  if (d == STOP && !valid_end_moment())
	    return false;

	  if (d == START && !valid_start_moment ())
	    return false;
	  
	  if (d == STOP)
	    {
	      SCM m = get_property ("automaticMelismata");
	      SCM b = get_property ("autoBeaming");
	      if (to_boolean (m) && !to_boolean (b))
		{
		  set_melisma (false);
		}
	    }

	  reqs_drul_[d ] = c;
	  return true;
	}
    }
  return false;
}

void
Beam_engraver::set_melisma (bool m)
{
  daddy_trans_->set_property ("beamMelismaBusy", m ? SCM_BOOL_T :SCM_BOOL_F);
}

void
Beam_engraver::process_music ()
{
  if (reqs_drul_[STOP])
    {
      prev_start_req_ =0;
      finished_beam_ = beam_;
      finished_beam_info_ = beam_info_;

      beam_info_ =0;
      beam_ = 0;
    }


  if (beam_)
    {
      top_engraver ()->forbid_breaks ();
    }
  if (reqs_drul_[START])
    {
      if (beam_)
	{
	  reqs_drul_[START]->origin ()->warning (_ ("already have a beam"));
	  return;
	}

      prev_start_req_ = reqs_drul_[START];
      beam_ = new Spanner (get_property ("Beam"));
      SCM smp = get_property ("measurePosition");
      Moment mp = (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);

      beam_start_location_ = mp;
      beam_start_mom_ = now_mom ();
      
      beam_info_ = new Beaming_info_list;
      
      /* urg, must copy to Auto_beam_engraver too */
 
      announce_grob(beam_, reqs_drul_[START]->self_scm());
    }

}


void
Beam_engraver::typeset_beam ()
{
  if (finished_beam_)
    {
      finished_beam_info_->beamify(beat_length_, subdivide_beams_);
      Beam::set_beaming (finished_beam_, finished_beam_info_);
      typeset_grob (finished_beam_);
      delete finished_beam_info_;
      finished_beam_info_ =0;
      finished_beam_ = 0;
    }
}

void
Beam_engraver::start_translation_timestep ()
{
  reqs_drul_ [START] =0;
  reqs_drul_[STOP] = 0;
  
  if (beam_)
    {
      SCM m = get_property ("automaticMelismata");
      SCM b = get_property ("autoBeaming");
      if (to_boolean (m) && !to_boolean (b))
	{
	  set_melisma (true);
	}
      subdivide_beams_ = to_boolean(get_property("subdivideBeams"));
      beat_length_ = *unsmob_moment (get_property ("beatLength"));
    }
}

void
Beam_engraver::stop_translation_timestep ()
{
  typeset_beam ();
}

void
Beam_engraver::finalize ()
{
  typeset_beam ();
  if (beam_)
    {
      prev_start_req_->origin ()->warning (_ ("unterminated beam"));

      /*
	we don't typeset it, (we used to, but it was commented
	out. Reason unknown) */
      beam_->suicide ();
      delete beam_info_;
    }
}

void
Beam_engraver::acknowledge_grob (Grob_info info)
{
  if (beam_)
    {
      if (Rest::has_interface (info.grob_))
	{
	  info.grob_->add_offset_callback (Beam::rest_collision_callback_proc, Y_AXIS);
	}
      else if (Stem::has_interface (info.grob_))
	{
	  Moment now = now_mom();

	  if (!valid_start_moment ())
	    return ;
	  
	  Item *stem = dynamic_cast<Item*> (info.grob_);
	  if (Stem::get_beam (stem))
	    return;

	  Rhythmic_req *rhythmic_req = dynamic_cast <Rhythmic_req *> (info.music_cause ());
	  if (!rhythmic_req)
	    {
	      String s = _ ("stem must have Rhythmic structure");
	      if (info.music_cause ())
		info.music_cause ()->origin ()->warning (s);
	      else
		::warning (s);
	  
	      return;
	    }


	  last_stem_added_at_ = now;
	  int durlog  = unsmob_duration (rhythmic_req->get_mus_property ("duration"))-> duration_log ();
	  if (durlog <= 2)
	    {
	      rhythmic_req->origin ()->warning (_ ("stem doesn't fit in beam"));
	      prev_start_req_->origin ()->warning (_ ("beam was started here"));
	      /*
		don't return, since

		[r4 c8] can just as well be modern notation.
	      */
	    }

	  stem->set_grob_property ("duration-log",
				    gh_int2scm (durlog));
	  Moment stem_location = now - beam_start_mom_ + beam_start_location_;
	  beam_info_->add_stem (stem_location,
 (durlog- 2) >? 0);
	  Beam::add_stem (beam_, stem);
	}
    }
}





ENTER_DESCRIPTION(Beam_engraver,
/* descr */       "Handles Beam_requests by engraving Beams.    If omitted, then notes will be
printed with flags instead of beams.",
/* creats*/       "Beam",
/* acks  */       "stem-interface rest-interface",
/* reads */       "beamMelismaBusy beatLength subdivideBeams",
/* write */       "");


class Grace_beam_engraver : public Beam_engraver
{
public:
  TRANSLATOR_DECLARATIONS(Grace_beam_engraver);  

protected:
  virtual bool valid_start_moment();
  virtual bool valid_end_moment ();
};

Grace_beam_engraver::Grace_beam_engraver()
{
}

bool
Grace_beam_engraver::valid_start_moment()
{
  Moment n = now_mom ();

  return n.grace_part_ != Rational (0);
}


bool
Grace_beam_engraver::valid_end_moment ()
{
  return beam_ && last_stem_added_at_.grace_part_ != Rational(0);
}



ENTER_DESCRIPTION(Grace_beam_engraver,
/* descr */       "Handles Beam_requests by engraving Beams.  If omitted, then notes will
be printed with flags instead of beams. Only engraves beams when we
are at grace points in time.
",
/* creats*/       "Beam",
/* acks  */       "stem-interface rest-interface",
/* reads */       "beamMelismaBusy beatLength subdivideBeams",
/* write */       "");

