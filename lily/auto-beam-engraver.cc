/*   
  auto-beam-engraver.cc --  implement Auto_beam_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "beaming.hh"
#include "musical-request.hh"
#include "beam.hh"
#include "stem.hh"
#include "debug.hh"
#include "engraver-group-engraver.hh"
#include "bar.hh"
#include "rest.hh"
#include "engraver.hh"
#include "item.hh"
#include "spanner.hh"

/*
  TODO: figure what to do in grace?

  TODO: documentme.
 */
class Auto_beam_engraver : public Engraver
{
public:
  Auto_beam_engraver ();
  VIRTUAL_COPY_CONS (Translator);

protected:
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void do_removal_processing ();
  virtual void acknowledge_grob (Grob_info);
  virtual void create_grobs ();

private:
  void begin_beam ();
  void consider_end_and_begin (Moment test_mom);
  Spanner* create_beam_p ();
  void end_beam ();
  void junk_beam ();
  bool same_grace_state_b (Grob* e);
  void typeset_beam ();

  /*
    shortest_mom is the shortest note in the beam.
   */
  Moment shortest_mom_;
  Spanner *finished_beam_p_;
  Link_array<Item>* stem_l_arr_p_;


  bool first_b_;
  Moment last_add_mom_;

  /*
    Projected ending of the  beam we're working on.
   */
  Moment extend_mom_;
  Moment beam_start_moment_;
  Moment beam_start_location_;
  
  // We act as if beam were created, and start a grouping anyway.
  Beaming_info_list*grouping_p_;  
  Beaming_info_list*finished_grouping_p_;
};

ADD_THIS_TRANSLATOR (Auto_beam_engraver);

Auto_beam_engraver::Auto_beam_engraver ()
{
  first_b_ = true;
  stem_l_arr_p_ = 0;
  shortest_mom_ = Moment (1, 8);
  finished_beam_p_ = 0;
  finished_grouping_p_ = 0;
  grouping_p_ = 0;
}

/*
  rename me: consider_end_or_begin () ? 
 */
void
Auto_beam_engraver::consider_end_and_begin (Moment test_mom)
{
  SCM wild = gh_list (ly_symbol2scm ("*"), ly_symbol2scm ("*"), SCM_UNDEFINED);
  SCM b = gh_list (ly_symbol2scm ("begin"), SCM_UNDEFINED);
  SCM e = gh_list (ly_symbol2scm ("end"), SCM_UNDEFINED);

  Moment one_beat = *unsmob_moment( get_property ("beatLength"));
  int num = *unsmob_moment (get_property("measureLength")) / one_beat;
  int den = one_beat.den_i ();
  SCM time = gh_list (gh_int2scm (num), gh_int2scm (den), SCM_UNDEFINED);

  SCM type = gh_list (gh_int2scm (test_mom.num_i ()),
		      gh_int2scm (test_mom.den_i ()), SCM_UNDEFINED);

  SCM settings = get_property ("autoBeamSettings");
  
  /*
    Determine end moment for auto beaming (and begin, mostly 0==anywhere) 
    In order of increasing priority:

    i.   every beat
    ii.  end   *    <num> <den>
    iii. end <type> <num> <den>

    iv.  end   *      *     *
    v.   end <type>   *     *


    Rationale:

    [to be defined in config file]
    i.   easy catch-all rule
    ii.  exceptions for time signature
    iii. exceptions for time signature, for specific duration type

    [user override]
    iv.  generic override
    v.   override for specific duration type

  */
  

  /*
    first guess: begin beam at any position
  */
  Moment begin_mom (0);
  /*
    first guess: end beam at end of beat
  */
  SCM one (get_property ("beatLength"));

  Moment end_mom;
  if (unsmob_moment (one))
    end_mom = *unsmob_moment (one);

  /*
    second guess: property generic time exception
  */
  SCM begin = gh_assoc (gh_append3 (b, wild, time), settings);
  
  if (begin != SCM_BOOL_F && unsmob_moment (gh_cdr (begin)))
    begin_mom = * unsmob_moment (gh_cdr (begin));

  SCM end = gh_assoc (gh_append3 (e, wild, time), settings);
  if (end != SCM_BOOL_F && unsmob_moment (gh_cdr (end)))
    end_mom = * unsmob_moment (gh_cdr (end));

  /*
    third guess: property time exception, specific for duration type
  */
  SCM begin_mult = gh_assoc (gh_append3 (b, type, time), settings);
  if (begin_mult != SCM_BOOL_F && unsmob_moment (gh_cdr (begin_mult)))
    begin_mom = * unsmob_moment (gh_cdr (begin_mult));
  
  SCM end_mult = gh_assoc (gh_append3 (e, type, time), settings);
  if (end_mult != SCM_BOOL_F && unsmob_moment (gh_cdr (end_mult)))
    end_mom = * unsmob_moment (gh_cdr (end_mult));

  /*
    fourth guess [user override]: property plain generic
  */
  begin = gh_assoc (gh_append3 (b, wild, wild), settings);
  if (begin != SCM_BOOL_F && unsmob_moment (gh_cdr (begin)))
    begin_mom = * unsmob_moment (gh_cdr (begin));

  end = gh_assoc (gh_append3 (e, wild, wild), settings);
  if (end != SCM_BOOL_F && unsmob_moment (gh_cdr (end)))
    end_mom = * unsmob_moment (gh_cdr (end));

  /*
    fifth guess [user override]: property plain, specific for duration type
  */
  begin_mult = gh_assoc (gh_append3 (b, type, wild), settings);
  if (begin_mult != SCM_BOOL_F && unsmob_moment (gh_cdr (begin_mult)))
    begin_mom = * unsmob_moment (gh_cdr (begin_mult));
  
  end_mult = gh_assoc (gh_append3 (e, type, wild), settings);
  if (end_mult != SCM_BOOL_F && unsmob_moment (gh_cdr (end_mult)))
    end_mom = * unsmob_moment (gh_cdr (end_mult));

  Rational r;
  if (end_mom)
    r = unsmob_moment (get_property ("measurePosition"))->mod_rat (end_mom);
  else
    r = Moment (1);

  if (stem_l_arr_p_ && stem_l_arr_p_->size () > 1 && !r)
    end_beam ();

  /*
    Allow already started autobeam to end
   */
  SCM on = get_property ("noAutoBeaming");
  if (to_boolean (on))
    return;

  if (begin_mom)
    r = unsmob_moment (get_property ("measurePosition"))->mod_rat (begin_mom);
  if (!stem_l_arr_p_ && (!begin_mom || !r))
    begin_beam ();
}
      
Spanner*
Auto_beam_engraver::create_beam_p ()
{
  Spanner* beam_p = new Spanner (get_property ("Beam"));
  Beam::set_interface (beam_p);

  for (int i = 0; i < stem_l_arr_p_->size (); i++)
    {
      /*
	watch out for stem tremolos and abbreviation beams
       */
      if (Stem::beam_l ((*stem_l_arr_p_)[i]))
	{
	  scm_unprotect_object (beam_p->self_scm ());
	  return 0;
	}
      Beam::add_stem (beam_p,(*stem_l_arr_p_)[i]);
    }
  
  announce_grob (beam_p, 0);

  return beam_p;
}

void
Auto_beam_engraver::begin_beam ()
{
  assert (!stem_l_arr_p_);
  stem_l_arr_p_ = new Link_array<Item>;
  assert (!grouping_p_);
  grouping_p_ = new Beaming_info_list;
  beam_start_moment_ = now_mom ();
  beam_start_location_ = *unsmob_moment (get_property ("measurePosition"));
}


void
Auto_beam_engraver::junk_beam () 
{
  assert (stem_l_arr_p_);
  
  delete stem_l_arr_p_;
  stem_l_arr_p_ = 0;
  delete grouping_p_;
  grouping_p_ = 0;
  shortest_mom_ = Moment (1, 8);
}

void
Auto_beam_engraver::end_beam ()
{
  if (stem_l_arr_p_->size () < 2)
    {
      junk_beam ();
    }
  else
    {
      finished_beam_p_ = create_beam_p ();
      if (finished_beam_p_)
	finished_grouping_p_ = grouping_p_;
      delete stem_l_arr_p_;
      stem_l_arr_p_ = 0;
      grouping_p_ = 0;
      shortest_mom_ = Moment (1, 8);
    }
}

void
Auto_beam_engraver::typeset_beam ()
{
  if (finished_beam_p_)
    {
      finished_grouping_p_->beamify ();
      Beam::set_beaming (finished_beam_p_, finished_grouping_p_);
      typeset_grob (finished_beam_p_);
      finished_beam_p_ = 0;
    
      delete finished_grouping_p_;
      finished_grouping_p_= 0;
    }
}

void
Auto_beam_engraver::start_translation_timestep ()
{
  first_b_ =true;
  /*
    don't beam over skips
   */
  if (stem_l_arr_p_)
    {
      Moment now = now_mom ();
      if (extend_mom_ < now)
	{
	  end_beam ();
	}
    }
}

void
Auto_beam_engraver::stop_translation_timestep ()
{
  
  typeset_beam ();
}

void
Auto_beam_engraver::do_removal_processing ()
{
  /* finished beams may be typeset */
  typeset_beam ();
  /* but unfinished may need another announce/acknowledge pass */
  if (stem_l_arr_p_)
    junk_beam ();
}

bool
Auto_beam_engraver::same_grace_state_b (Grob* e)
{
  bool gr = e->get_grob_property ("grace") == SCM_BOOL_T;
  SCM wg =get_property ("weAreGraceContext");
  return (to_boolean (wg)) == gr;
}

void
Auto_beam_engraver::acknowledge_grob (Grob_info info)
{
  if (!same_grace_state_b (info.elem_l_))
    return;
  
  if (stem_l_arr_p_)
    {
      if (Beam::has_interface (info.elem_l_))
	{
	  end_beam ();
	}
      else if (Bar::has_interface (info.elem_l_))
	{
	  end_beam ();
	}
      else if (Rest::has_interface (info.elem_l_))
	{
	  end_beam ();
	}
    }
  
  if (Stem::has_interface (info.elem_l_))
    {
      Item* stem_l = dynamic_cast<Item *> (info.elem_l_);
				       
      Rhythmic_req *rhythmic_req = dynamic_cast <Rhythmic_req *> (info.req_l_);
      if (!rhythmic_req)
	{
	  programming_error ("Stem must have rhythmic structure");
	  return;
	}
      
      /*
	Don't (start) auto-beam over empty stems; skips or rests
	*/
      if (!Stem::heads_i (stem_l))
	{
	  if (stem_l_arr_p_)
	    end_beam ();
	  return;
	}

      if (Stem::beam_l (stem_l))
	{
	  if (stem_l_arr_p_)
	    junk_beam ();
	  return ;
	}
	      
      int durlog  = unsmob_duration (rhythmic_req->get_mus_property ("duration"))->duration_log ();
      
      if (durlog <= 2)
	{
	  if (stem_l_arr_p_)
	    end_beam ();
	  return;
	}

      /*
	if shortest duration would change
	reconsider ending/starting beam first.
      */
      Moment dur = unsmob_duration (rhythmic_req->get_mus_property ("duration"))->length_mom ();
      consider_end_and_begin (dur);
      if (!stem_l_arr_p_)
	return;
      
      if (dur < shortest_mom_)
	{
	  shortest_mom_ = dur;
	  if (stem_l_arr_p_->size ())
	    {
	      shortest_mom_ = dur;
	      consider_end_and_begin (shortest_mom_);
	      if (!stem_l_arr_p_)
		return;
	    }
	}
      
      Moment now = now_mom ();
      
      grouping_p_->add_stem (now - beam_start_moment_ + beam_start_location_,
			     durlog - 2);
      stem_l_arr_p_->push (stem_l);
      last_add_mom_ = now;
      extend_mom_ = extend_mom_ >? now + rhythmic_req->length_mom ();
    }
}

void
Auto_beam_engraver::create_grobs ()
{
  if (first_b_)
    {
      first_b_ = false;
      consider_end_and_begin (shortest_mom_);
    }
  else
    {
      if (stem_l_arr_p_)
	{
	  Moment now = now_mom ();
	  if ((extend_mom_ < now)
	      || ((extend_mom_ == now) && (last_add_mom_ != now )))
	    {
	      end_beam ();
	    }
	  else if (!stem_l_arr_p_->size ())
	    {
	      junk_beam ();
	    }
	}    
    }  
}
