
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

class Auto_beam_engraver : public Engraver
{
public:
  Auto_beam_engraver ();
  VIRTUAL_COPY_CONS (Translator);

protected:
  virtual bool do_try_music (Music*);
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void do_removal_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual void do_process_music ();
  virtual void process_acknowledged ();
private:
  void begin_beam ();
  void consider_end_and_begin (Moment test_mom);
  Spanner* create_beam_p ();
  void end_beam ();
  void junk_beam ();
  bool same_grace_state_b (Score_element* e);
  void typeset_beam ();

  Moment shortest_mom_;
  Spanner *finished_beam_p_;
  Link_array<Item>* stem_l_arr_p_;
  
  Moment last_add_mom_;
  Moment extend_mom_;
  Moment beam_start_moment_;
  Moment beam_start_location_;
  
  // We act as if beam were created, and start a grouping anyway.
  Beaming_info_list*grouping_p_;  
  Beaming_info_list*finished_grouping_p_;
};




ADD_THIS_TRANSLATOR (Auto_beam_engraver);


/*
  TODO: remove all references to Timing_engraver; should read properties.
  
 */
Auto_beam_engraver::Auto_beam_engraver ()
{
  stem_l_arr_p_ = 0;
  shortest_mom_ = Moment (1, 8);
  finished_beam_p_ = 0;
  finished_grouping_p_ = 0;
  grouping_p_ = 0;
}


bool
Auto_beam_engraver::do_try_music (Music*) 
{
  return false;
} 

void
Auto_beam_engraver::do_process_music ()
{
  consider_end_and_begin (shortest_mom_);
}

void
Auto_beam_engraver::consider_end_and_begin (Moment test_mom)
{
  Moment one_beat = *unsmob_moment( get_property ("beatLength"));

  int num = *unsmob_moment (get_property("measureLength")) / one_beat;
  int den = one_beat.den_i ();

  
  String time_str = String ("time") + to_str (num) + "_" + to_str (den);

  String type_str;
  if (test_mom.num () != 1)
    type_str = to_str (test_mom.num ());
  if (test_mom.den () != 1)
    type_str = type_str + "_" + to_str (test_mom.den ());

  /*
    URG
    
    FIXME: SHOULD USE ALIST
    
   */

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
  SCM one (get_property ("beatLength"));

  Moment end_mom;
  if (unsmob_moment (one))
    end_mom = *unsmob_moment (one);

  /*
    second guess: property generic time exception
  */
  SCM begin = get_property ((time_str + "beamAutoBegin").ch_C());
  if (unsmob_moment (begin))
    begin_mom = * unsmob_moment (begin);

  SCM end = get_property ((time_str + "beamAutoEnd").ch_C());
  if (unsmob_moment (end))
    end_mom = * unsmob_moment (end);

  /*
    third guess: property time exception, specific for duration type
  */
  if (type_str.length_i ())
    {
      SCM end_mult = get_property ((time_str + "beamAutoEnd" + type_str).ch_C());
      if (unsmob_moment (end_mult))
	end_mom = * unsmob_moment (end_mult);

      SCM begin_mult = get_property ((time_str + "beamAutoBegin" + type_str).ch_C());
      if (unsmob_moment (begin_mult))
	begin_mom = * unsmob_moment (begin_mult);
    }

  /*
    fourth guess [user override]: property plain generic
  */
  begin = get_property ("beamAutoBegin");
  if (unsmob_moment (begin))
    begin_mom = * unsmob_moment (begin);


  
  end = get_property ("beamAutoEnd");
  if (unsmob_moment (end))
    end_mom = * unsmob_moment (end);

  /*
    fifth guess [user override]: property plain, specific for duration type
  */
  if (type_str.length_i ())
    {
      SCM end_mult = get_property ((String ("beamAutoEnd") + type_str).ch_C());
      if (unsmob_moment (end_mult))
	end_mom = * unsmob_moment (end_mult);

      SCM begin_mult = get_property ((String ("beamAutoBegin") + type_str).ch_C());
      if (unsmob_moment (begin_mult))
	begin_mom = * unsmob_moment (begin_mult);
    }

  Rational r;
  if (end_mom)
    r = unsmob_moment (get_property ("measurePosition"))->mod_rat (end_mom);
  else
    r = Moment (1);

  if (stem_l_arr_p_ && !r)
    end_beam ();
     
  /*
    Allow already started autobeam to end
   */
  SCM on = get_property ("noAutoBeaming");
  if (to_boolean (on))
    return;

  if (begin_mom)
    r =     unsmob_moment (get_property ("measurePosition"))->mod_rat (begin_mom);
  if (!stem_l_arr_p_ && (!begin_mom || !r))
    begin_beam ();
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
	  return 0;
	}
      Beam::add_stem (beam_p,(*stem_l_arr_p_)[i]);
    }
  
  announce_element (beam_p, 0);

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
      Beam::set_beaming (finished_beam_p_, finished_grouping_p_);
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
  /* but unfinished may need another announce/acknowledge pass */
  if (stem_l_arr_p_)
    junk_beam ();
}

bool
Auto_beam_engraver::same_grace_state_b (Score_element* e)
{
  bool gr = e->get_elt_property ("grace") == SCM_BOOL_T;
  SCM wg =get_property ("weAreGraceContext");
  return (to_boolean (wg)) == gr;
}

void
Auto_beam_engraver::acknowledge_element (Score_element_info info)
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

