/*   
  beam-engraver.cc --  implement Beam_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void do_removal_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual bool do_try_music (Music*);
  virtual void do_process_music ();
public:
  Beam_engraver ();
  VIRTUAL_COPY_CONS (Translator);
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
Beam_engraver::do_try_music (Music *m)
{
  if (Span_req * c = dynamic_cast<Span_req*>(m))
    {
      if (c->span_type_str_ != "beam")
	return false;
      
      Direction d =c->span_dir_;

      if (d == STOP && !beam_p_)
	{
	  m->origin ()->warning  (_ ("can't find start of beam"));
	  return false;
	}

      if(d == STOP)
	{
	  SCM m = get_property ("automaticMelismata");
	  SCM b = get_property("noAutoBeaming");
	  if (to_boolean (m) && to_boolean(b))
	    {
	      set_melisma (false);
	    }
	}

      reqs_drul_[d ] = c;
      return true;
    }
  return false;
}

void
Beam_engraver::set_melisma (bool m)
{
  daddy_trans_l_->set_property ("beamMelismaBusy", m ? SCM_BOOL_T :SCM_BOOL_F);
}


void
Beam_engraver::do_process_music ()
{
  if (reqs_drul_[STOP])
    {
      if (!beam_p_)
	reqs_drul_[STOP]->origin ()->warning (_("can't find start of beam"));
      prev_start_req_ =0;
      finished_beam_p_ = beam_p_;
      finished_beam_info_p_ = beam_info_p_;

      beam_info_p_ =0;
      beam_p_ = 0;
    }


  if (beam_p_  &&  !to_boolean (get_property ("weAreGraceContext")))
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
  
  if (reqs_drul_[START])
    {
      if (beam_p_)
	{
	  reqs_drul_[START]->origin ()->warning (_ ("already have a beam"));
	  return;
	}

      prev_start_req_ = reqs_drul_[START];
      beam_p_ = new Spanner (get_property ("basicBeamProperties"));
      Beam::set_interface (beam_p_);
      
      SCM smp = get_property ("measurePosition");
      Moment mp =  (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);

      beam_start_location_ = mp;
      beam_start_mom_ = now_mom();
      beam_info_p_ = new Beaming_info_list;
      
      
      /* urg, must copy to Auto_beam_engraver too */
 
      announce_element (beam_p_, reqs_drul_[START]);
    }
}

void
Beam_engraver::typeset_beam ()
{
  if (finished_beam_p_)
    {
      finished_beam_info_p_->beamify ();
      
      Beam::set_beaming (finished_beam_p_, finished_beam_info_p_);
      typeset_element (finished_beam_p_);
      delete finished_beam_info_p_;
      finished_beam_info_p_ =0;
      finished_beam_p_ = 0;
    
      reqs_drul_[STOP] = 0;
    }
}

void
Beam_engraver::do_post_move_processing ()
{
  reqs_drul_ [START] =0;
  if(beam_p_) {
    SCM m = get_property ("automaticMelismata");
    SCM b = get_property("noAutoBeaming");
    if (to_boolean (m) && to_boolean(b)) {
      set_melisma (true);
    }
  }
}

void
Beam_engraver::do_pre_move_processing ()
{
  typeset_beam ();
}

void
Beam_engraver::do_removal_processing ()
{
  typeset_beam ();
  if (beam_p_)
    {
      prev_start_req_->origin ()->warning (_ ("unterminated beam"));
#if 0
      finished_beam_p_ = beam_p_;
      finished_beam_info_p_ = beam_info_p_;
      typeset_beam ();
#else
      beam_p_->suicide ();
      delete beam_info_p_;
#endif
    }
}

void
Beam_engraver::acknowledge_element (Score_element_info info)
{
  if (beam_p_)
    {
      if (Rest::has_interface (info.elem_l_))
	{
	  info.elem_l_->add_offset_callback (Beam::rest_collision_callback, Y_AXIS);
	}
      else if (Stem::has_interface (info.elem_l_))
	{
	  Item *stem_l = dynamic_cast<Item*> (info.elem_l_);
	  if (Stem::beam_l (stem_l))
	    return;

	  bool stem_grace = stem_l->get_elt_property ("grace") == SCM_BOOL_T;

	  SCM wg =get_property ("weAreGraceContext");
	  bool wgb= to_boolean (wg);

	  if (wgb!= stem_grace)
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

	  if (rhythmic_req->duration_.durlog_i_<= 2)
	    {
	      rhythmic_req->origin ()->warning (_ ("stem doesn't fit in beam"));
	      prev_start_req_->origin ()->warning (_ ("beam was started here"));
	      /*
		don't return, since

		[r4 c8] can just as well be modern notation.
	      */
	    }

	  stem_l->set_elt_property ("duration-log",
				    gh_int2scm (rhythmic_req->duration_.durlog_i_));
	  Moment stem_location = now_mom () - beam_start_mom_ + beam_start_location_;
	  beam_info_p_->add_stem (stem_location,
				  (rhythmic_req->duration_.durlog_i_ - 2) >? 1);
	  Beam::add_stem (beam_p_, stem_l);
	}
    }
}



ADD_THIS_TRANSLATOR(Beam_engraver);

