/*   
  auto-beam-engraver.cc --  implement Auto_beam_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */
#include "beaming.hh"
#include "auto-beam-engraver.hh"
#include "musical-request.hh"
#include "bar.hh"
#include "beam.hh"
#include "chord-tremolo.hh"
#include "rest.hh"
#include "stem.hh"
#include "debug.hh"
#include "timing-engraver.hh"
#include "engraver-group-engraver.hh"

ADD_THIS_TRANSLATOR (Auto_beam_engraver);

Auto_beam_engraver::Auto_beam_engraver ()
{
  stem_l_arr_p_ = 0;
  shortest_mom_ = Moment (1, 8);
  finished_beam_p_ = 0;
  finished_grouping_p_ = 0;
  grouping_p_ = 0;
  timer_l_ =0;
}

void
Auto_beam_engraver::do_creation_processing ()
{
  Translator * t = daddy_grav_l  ()->get_simple_translator ("Timing_engraver");
  timer_l_ = dynamic_cast<Timing_engraver*> (t);
}

bool
Auto_beam_engraver::do_try_music (Music*) 
{
  return false;
} 

void
Auto_beam_engraver::do_process_requests ()
{
  consider_end_and_begin (shortest_mom_);
}

void
Auto_beam_engraver::consider_end_and_begin (Moment test_mom)
{
  if (!timer_l_)
      return;
  
  Time_description const *time = &timer_l_->time_;
  int num = time->whole_per_measure_ / time->one_beat_;
  int den = time->one_beat_.den_i ();
  String time_str = String ("time") + to_str (num) + "_" + to_str (den);

  String type_str;
  if (test_mom.num () != 1)
    type_str = to_str (test_mom.num ());
  if (test_mom.den () != 1)
    type_str = type_str + "_" + to_str (test_mom.den ());

  /*
    Determine end moment for auto beaming (and begin, mostly 0==anywhere) 
    In order of increasing priority:

    i.   every beat <den>
    ii.  time<num>_<den>beamAutoEnd
    iii. time<num>_<den>beamAutoEnd<type>
    iv.  beamAutoEnd
    v.   beamAutoEnd<type>


    Rationale:

    [to be defined in config file]
    i.   easy catch-all rule
    ii.  exceptions for time signature
    iii. exceptions for time signature, for specific duration type

    [user override]
    iv.  generic override
    v.   override for specific duration type

    The user overrides should be required for common cases.
   */
  
  /*
    first guess: begin beam at any position
  */
  Moment begin_mom (0);
  /*
    first guess: end beam at end of beat
  */
  Moment end_mom = time->one_beat_;

  /*
    second guess: property generic time exception
  */
  Scalar begin = get_property (time_str + "beamAutoBegin", 0);
  if (begin.length_i ())
    begin_mom = begin.to_rat ();

  Scalar end = get_property (time_str + "beamAutoEnd", 0);
  if (end.length_i ())
    end_mom = end.to_rat ();

  /*
    third guess: property time exception, specific for duration type
  */
  if (type_str.length_i ())
    {
      Scalar end_mult = get_property (time_str + "beamAutoEnd" + type_str, 0);
      if (end_mult.length_i ())
	end_mom = end_mult.to_rat ();
      Scalar begin_mult = get_property (time_str + "beamAutoBegin" + type_str, 0);
      if (begin_mult.length_i ())
	begin_mom = begin_mult.to_rat ();
    }

  /*
    fourth guess [user override]: property plain generic
  */
  begin = get_property ("beamAutoBegin", 0);
  if (begin.length_i ())
    begin_mom = begin.to_rat ();
  
  end = get_property ("beamAutoEnd", 0);
  if (end.length_i ())
    end_mom = end.to_rat ();

  /*
    fifth guess [user override]: property plain, specific for duration type
  */
  if (type_str.length_i ())
    {
      Scalar end_mult = get_property (String ("beamAutoEnd") + type_str, 0);
      if (end_mult.length_i ())
	end_mom = end_mult.to_rat ();
      Scalar begin_mult = get_property (String ("beamAutoBegin") + type_str, 0);
      if (begin_mult.length_i ())
	begin_mom = begin_mult.to_rat ();
    }

  Rational r;
  if (end_mom)
    r = time->whole_in_measure_.mod_rat (end_mom);
  else
    r = Moment (1);

  if (stem_l_arr_p_ && !r)
    end_beam ();
     
  /*
    Allow already started autobeam to end
   */
  Scalar on = get_property ("noAutoBeaming", 0);
  if (on.to_bool ())
    return;

  if (begin_mom)
    r = time->whole_in_measure_.mod_rat (begin_mom);
  if (!stem_l_arr_p_ && (!begin_mom || !r))
    begin_beam ();
}

      
void
Auto_beam_engraver::begin_beam ()
{
  assert (!stem_l_arr_p_);
  stem_l_arr_p_ = new Array<Stem*>;
  assert (!grouping_p_);
  grouping_p_ = new Beaming_info_list;
  beam_start_moment_ = now_mom ();
  beam_start_location_ = timer_l_->time_.whole_in_measure_;
}

Beam*
Auto_beam_engraver::create_beam_p ()
{
  Beam* beam_p = new Beam;

  for (int i = 0; i < stem_l_arr_p_->size (); i++)
    {
      /*
	watch out for stem tremolos and abbreviation beams
       */
      if ((*stem_l_arr_p_)[i]->beam_l_)
	{
	  delete beam_p;
	  return 0;
	}
      beam_p->add_stem ((*stem_l_arr_p_)[i]);
    }
  
  /* urg, copied from Beam_engraver */
  Scalar prop = get_property ("beamslopedamping", 0);
  if (prop.isnum_b ()) 
    beam_p->set_elt_property (damping_scm_sym, gh_int2scm( prop));

  prop = get_property ("beamquantisation", 0);
  if (prop.isnum_b ()) 
    beam_p->quantisation_ = (Beam::Quantisation)(int)prop;
 
  announce_element (Score_element_info (beam_p, 0));
  return beam_p;
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
      finished_beam_p_->set_beaming (finished_grouping_p_);
      typeset_element (finished_beam_p_);
      finished_beam_p_ = 0;
    
      delete finished_grouping_p_;
      finished_grouping_p_= 0;
    }
}

void
Auto_beam_engraver::do_post_move_processing ()
{
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
Auto_beam_engraver::do_pre_move_processing ()
{
  typeset_beam ();
}

void
Auto_beam_engraver::do_removal_processing ()
{
  /* finished beams may be typeset */
  typeset_beam ();
  /* but unfinished may need another announce/acknoledge pass */
  if (stem_l_arr_p_)
    junk_beam ();
}

bool
Auto_beam_engraver::same_grace_state_b (Score_element* e)
{
  bool gr = (e->get_elt_property (grace_scm_sym) != SCM_BOOL_F) ;

  return gr == get_property ("weAreGraceContext",0).to_bool ();
}

void
Auto_beam_engraver::acknowledge_element (Score_element_info info)
{
  if (!same_grace_state_b (info.elem_l_) || !timer_l_)
    return;
  
  if (stem_l_arr_p_)
    {
      if (Beam *b = dynamic_cast<Beam *> (info.elem_l_))
	{
	  end_beam ();
	}
      else if (Chord_tremolo *b = dynamic_cast<Chord_tremolo*> (info.elem_l_))
	{
	  end_beam ();
	}
      else if (Bar *b = dynamic_cast<Bar *> (info.elem_l_))
	{
	  end_beam ();
	}
      else if (Rest* rest_l = dynamic_cast<Rest *> (info.elem_l_))
	{
	  end_beam ();
	}
    }
  
  if (Stem* stem_l = dynamic_cast<Stem *> (info.elem_l_))
    {
      Rhythmic_req *rhythmic_req = dynamic_cast <Rhythmic_req *> (info.req_l_);
      if (!rhythmic_req)
	{
	  programming_error ("Stem must have rhythmic structure");
	  return;
	}
      
      /*
	Don't (start) auto-beam over empty stems; skips or rests
	*/
      if (!stem_l->head_l_arr_.size ())
	{
	  if (stem_l_arr_p_)
	    end_beam ();
	  return;
	}

      if (stem_l->beam_l_)
	{
	  if (stem_l_arr_p_)
	    junk_beam ();
	  return ;
	}
	      
      int durlog  =rhythmic_req->duration_.durlog_i_;
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
      Moment mom = rhythmic_req->duration_.length_mom ();
      consider_end_and_begin (mom);
      if (!stem_l_arr_p_)
	return;
      if (mom < shortest_mom_)
	{
	  if (stem_l_arr_p_->size ())
	    {
	      shortest_mom_ = mom;
	      consider_end_and_begin (shortest_mom_);
	      if (!stem_l_arr_p_)
		return;
	    }
	  shortest_mom_ = mom;
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
Auto_beam_engraver::process_acknowledged ()
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
