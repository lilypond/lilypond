/*   
  beam-engraver.cc --  implement Beam_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  Drul_array<Span_req*> reqs_drul_;

  Link_array<Stem> stems_;
  
  
  Spanner *finished_beam_p_;
  Spanner *beam_p_;
  Span_req * prev_start_req_;

  Beaming_info_list * beam_info_p_;
  Beaming_info_list * finished_beam_info_p_;  

  /// location  within measure where beam started.
  Moment beam_start_location_;

  /// moment (global time) where beam started.
  Moment beam_start_mom_;
  
  void typeset_beam ();
  void set_melisma (bool);
protected:
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void finalize ();
  virtual void create_grobs ();
  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music*);
  virtual void process_music ();

public:
  TRANSLATOR_DECLARATIONS(  Beam_engraver );
};


Beam_engraver::Beam_engraver ()
{
  beam_p_ = 0;
  finished_beam_p_ =0;
  finished_beam_info_p_=0;
  beam_info_p_ =0;
  reqs_drul_[LEFT] = reqs_drul_[RIGHT] =0;
  prev_start_req_ =0;
}

bool
Beam_engraver::try_music (Music *m)
{
  if (Span_req * c = dynamic_cast<Span_req*> (m))
    {
      if (scm_equal_p (c->get_mus_property ("span-type"),
		       ly_str02scm ("abort")) == SCM_BOOL_T)
	{
	  reqs_drul_[START] = 0;
	  reqs_drul_[STOP] = 0;
	  if (beam_p_)
	    beam_p_->suicide ();
	  beam_p_ = 0;
	}
      else if (scm_equal_p (c->get_mus_property ("span-type"),
		       ly_str02scm ("beam")) == SCM_BOOL_T)
	{
      
	  Direction d =c->get_span_dir ();

	  if (d == STOP && !beam_p_)
	    {
	      m->origin ()->warning (_ ("can't find start of beam"));
	      return false;
	    }

	  if (d == STOP)
	    {
	      SCM m = get_property ("automaticMelismata");
	      SCM b = get_property ("noAutoBeaming");
	      if (to_boolean (m) && to_boolean (b))
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
  daddy_trans_l_->set_property ("beamMelismaBusy", m ? SCM_BOOL_T :SCM_BOOL_F);
}

void
Beam_engraver::process_music ()
{
  if (reqs_drul_[STOP])
    {
      if (!beam_p_)
	reqs_drul_[STOP]->origin ()->warning (_ ("can't find start of beam"));
      prev_start_req_ =0;
      finished_beam_p_ = beam_p_;
      finished_beam_info_p_ = beam_info_p_;

      beam_info_p_ =0;
      beam_p_ = 0;
    }


  if (beam_p_)
    {
      Score_engraver * e = 0;
      Translator * t  =  daddy_grav_l ();
      for (; !e && t;  t = t->daddy_trans_l_)
	{
	  e = dynamic_cast<Score_engraver*> (t);
	}
      
      if (!e)
	programming_error ("No score engraver!");
      else
	e->forbid_breaks ();
    }
}


void
Beam_engraver::create_grobs ()
{
  if (reqs_drul_[START])
    {
      if (beam_p_)
	{
	  reqs_drul_[START]->origin ()->warning (_ ("already have a beam"));
	  return;
	}

      prev_start_req_ = reqs_drul_[START];
      beam_p_ = new Spanner (get_property ("Beam"));
      SCM smp = get_property ("measurePosition");
      Moment mp = (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);

      beam_start_location_ = mp;
      beam_start_mom_ = now_mom ();
      
      beam_info_p_ = new Beaming_info_list;
      
      /* urg, must copy to Auto_beam_engraver too */
 
      announce_grob (beam_p_, reqs_drul_[START]);
    }
  reqs_drul_[STOP] = 0;
  reqs_drul_[START] = 0;
}

void
Beam_engraver::typeset_beam ()
{
  if (finished_beam_p_)
    {
      finished_beam_info_p_->beamify ();
      
      Beam::set_beaming (finished_beam_p_, finished_beam_info_p_);
      typeset_grob (finished_beam_p_);
      delete finished_beam_info_p_;
      finished_beam_info_p_ =0;
      finished_beam_p_ = 0;
    
      reqs_drul_[STOP] = 0;
    }
}

void
Beam_engraver::start_translation_timestep ()
{
  reqs_drul_ [START] =0;
  if (beam_p_) {
    SCM m = get_property ("automaticMelismata");
    SCM b = get_property ("noAutoBeaming");
    if (to_boolean (m) && to_boolean (b)) {
      set_melisma (true);
    }
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
  if (beam_p_)
    {
      prev_start_req_->origin ()->warning (_ ("unterminated beam"));

      /*
	we don't typeset it, (we used to, but it was commented
	out. Reason unknown) */
      beam_p_->suicide ();
      delete beam_info_p_;
    }
}

void
Beam_engraver::acknowledge_grob (Grob_info info)
{
  if (beam_p_)
    {
      if (Rest::has_interface (info.grob_l_))
	{
	  info.grob_l_->add_offset_callback (Beam::rest_collision_callback_proc, Y_AXIS);
	}
      else if (Stem::has_interface (info.grob_l_))
	{
	  Moment now = now_mom();

	  if(bool (now.grace_part_ ) != bool (beam_start_mom_.grace_part_))
	    return ;
	  
	  Item *stem_l = dynamic_cast<Item*> (info.grob_l_);
	  if (Stem::beam_l (stem_l))
	    return;

	  Rhythmic_req *rhythmic_req = dynamic_cast <Rhythmic_req *> (info.req_l_);
	  if (!rhythmic_req)
	    {
	      String s = _ ("stem must have Rhythmic structure");
	      if (info.req_l_)
		info.req_l_->origin ()->warning (s);
	      else
		::warning (s);
	  
	      return;
	    }

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

	  stem_l->set_grob_property ("duration-log",
				    gh_int2scm (durlog));
	  Moment stem_location = now - beam_start_mom_ + beam_start_location_;
	  beam_info_p_->add_stem (stem_location,
 (durlog- 2) >? 1);
	  Beam::add_stem (beam_p_, stem_l);
	}
    }
}





ENTER_DESCRIPTION(Beam_engraver,
/* descr */       "Handles Beam_requests by engraving Beams.    If omitted, then notes will be
printed with flags instead of beams.",
/* creats*/       "Beam",
/* acks  */       "stem-interface rest-interface",
/* reads */       "beamMelismaBusy",
/* write */       "");
