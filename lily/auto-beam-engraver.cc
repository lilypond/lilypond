/*   
  auto-beam-engraver.cc --  implement Auto_beam_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "beaming.hh"
#include "request.hh"
#include "beam.hh"
#include "stem.hh"
#include "warn.hh"
#include "engraver-group-engraver.hh"
#include "bar-line.hh"
#include "rest.hh"
#include "engraver.hh"
#include "item.hh"
#include "spanner.hh"
#include "duration.hh"

/*
  TODO: figure what to do in grace?

  TODO: documentme.
 */
class Auto_beam_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS(Auto_beam_engraver);
protected:
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void finalize ();
  virtual void acknowledge_grob (Grob_info);
  virtual void process_acknowledged_grobs ();

private:
  bool test_moment (Direction, Moment);
  void consider_begin (Moment);
  void consider_end (Moment);
  Spanner* create_beam ();
  void begin_beam ();
  void end_beam ();
  void junk_beam ();
  bool same_grace_state_b (Grob* e);
  void typeset_beam ();

  /*
    shortest_mom is the shortest note in the beam.
   */
  Moment shortest_mom_;
  Spanner *finished_beam_;
  Link_array<Item>* stems_;


  int count_;
  Moment last_add_mom_;
  /*
    Projected ending of the  beam we're working on.
   */
  Moment extend_mom_;
  Moment beam_start_moment_;
  Moment beam_start_location_;

  bool subdivide_beams_;
  Moment beat_length_;
  
  // We act as if beam were created, and start a grouping anyway.
  Beaming_info_list*grouping_;
  SCM beam_settings_ ;		// ugh. should protect ? 
  
  Beaming_info_list*finished_grouping_;
};



Auto_beam_engraver::Auto_beam_engraver ()
{
  count_ = 0;
  stems_ = 0;
  shortest_mom_ = Moment (Rational (1, 8));
  finished_beam_ = 0;
  finished_grouping_ = 0;
  grouping_ = 0;
  beam_settings_ = SCM_EOL;  
}

/*
  Determine end moment for auto beaming (or begin moment, but mostly
  0==anywhere) In order of increasing priority:
  
  i.   begin anywhere, end at every beat
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
bool
Auto_beam_engraver::test_moment (Direction dir, Moment test_mom)
{
  Moment now = now_mom();
  if (dir == START
      && now.grace_part_)
    {
      return false;
    }
  
  SCM wild = scm_list_n (ly_symbol2scm ("*"), ly_symbol2scm ("*"), SCM_UNDEFINED);
  SCM function;
  if (dir == START)
    function = scm_list_n (ly_symbol2scm ("begin"), SCM_UNDEFINED);
  else
    function = scm_list_n (ly_symbol2scm ("end"), SCM_UNDEFINED);

  Moment one_beat = *unsmob_moment (get_property ("beatLength"));
  int num = int ((*unsmob_moment (get_property ("measureLength")) / one_beat).main_part_);
  int den = one_beat.den ();
  SCM time = scm_list_n (scm_int2num (num), scm_int2num (den), SCM_UNDEFINED);

  SCM type = scm_list_n (scm_int2num (test_mom.num ()),
		      scm_int2num (test_mom.den ()), SCM_UNDEFINED);

  SCM settings = get_property ("autoBeamSettings");
  
  /* first guess */
  
  /* begin beam at any position
 (and fallback for end) */
  Moment moment (0);
  
  /* end beam at end of beat */
  if (dir == STOP)
    {
      SCM beat (get_property ("beatLength"));
      
      if (unsmob_moment (beat))
	moment = *unsmob_moment (beat);
    }

  /* second guess: property generic time exception */
  SCM m = gh_assoc (gh_append3 (function, wild, time), settings);
  
  if (m != SCM_BOOL_F && unsmob_moment (ly_cdr (m)))
    moment = * unsmob_moment (ly_cdr (m));

  /* third guess: property time exception, specific for duration type */
  m = gh_assoc (gh_append3 (function, type, time), settings);
  if (m != SCM_BOOL_F && unsmob_moment (ly_cdr (m)))
    moment = * unsmob_moment (ly_cdr (m));

  /* fourth guess [user override]: property plain generic */
  m = gh_assoc (gh_append3 (function, wild, wild), settings);
  if (m != SCM_BOOL_F && unsmob_moment (ly_cdr (m)))
    moment = * unsmob_moment (ly_cdr (m));

  /* fifth guess [user override]: property plain, specific for duration type */
  m = gh_assoc (gh_append3 (function, type, wild), settings);
  if (m != SCM_BOOL_F && unsmob_moment (ly_cdr (m)))
    moment = * unsmob_moment (ly_cdr (m));
  
  Rational r;
  if (moment.to_bool ())
    {
      /* Ugh? measurePosition can be negative, when \partial
	 We may have to fix this elsewhere (timing translator)
	r = unsmob_moment (get_property ("measurePosition"))->mod_rat (moment);
      */
      Moment pos = * unsmob_moment (get_property ("measurePosition"));
      if (pos < Moment (0))
	{
	  Moment length = * unsmob_moment (get_property ("measureLength"));
	  pos = length - pos;
	}
      r = pos.main_part_.mod_rat (moment.main_part_);
    }
  else
    {
      if (dir == START)
	/* if undefined, starting is ok */
	r = 0;
      else
	/* but ending is not */
	r = 1;
    }

  return !r;
}

void
Auto_beam_engraver::consider_begin (Moment test_mom)
{
  bool on = to_boolean (get_property ("autoBeaming"));
  if (!stems_ && on)
    {
      bool b = test_moment (START, test_mom);
      if (b)
	begin_beam ();
    }
}

void
Auto_beam_engraver::consider_end (Moment test_mom)
{
  if (stems_)
    {
      /* Allow already started autobeam to end:
	 don't check for autoBeaming */
      bool b = test_moment (STOP, test_mom);
      if (b)
	end_beam ();
    }
}

Spanner*
Auto_beam_engraver::create_beam ()
{
  if (to_boolean (get_property ("skipTypesetting")))
    {
     return 0;
    }
  
  Spanner* beam = new Spanner (beam_settings_);
  for (int i = 0; i < stems_->size (); i++)
    {
      /*
	watch out for stem tremolos and abbreviation beams
       */
      if (Stem::get_beam ((*stems_)[i]))
	{
	  scm_gc_unprotect_object (beam->self_scm ());
	  return 0;
	}
      Beam::add_stem (beam, (*stems_)[i]);
    }
  
  announce_grob(beam, SCM_EOL);

  return beam;
}

void
Auto_beam_engraver::begin_beam ()
{
  assert (!stems_);
  stems_ = new Link_array<Item>;
  assert (!grouping_);
  grouping_ = new Beaming_info_list;
  beam_settings_ = get_property ("Beam");
  
  beam_start_moment_ = now_mom ();
  beam_start_location_ = *unsmob_moment (get_property ("measurePosition"));
  subdivide_beams_ = gh_scm2bool(get_property("subdivideBeams"));
  beat_length_ = *unsmob_moment (get_property ("beatLength"));
}


void
Auto_beam_engraver::junk_beam () 
{
  assert (stems_);
  
  delete stems_;
  stems_ = 0;
  delete grouping_;
  grouping_ = 0;
  beam_settings_ = SCM_EOL;
  
  shortest_mom_ = Moment (Rational (1, 8));
}

void
Auto_beam_engraver::end_beam ()
{
  if (stems_->size () < 2)
    {
      junk_beam ();
    }
  else
    
    {
      finished_beam_ = create_beam ();
      if (finished_beam_)
	finished_grouping_ = grouping_;
      delete stems_;
      stems_ = 0;
      grouping_ = 0;
      beam_settings_ = SCM_EOL;
    }

  shortest_mom_ = Moment (Rational (1, 8));
}

void
Auto_beam_engraver::typeset_beam ()
{
  if (finished_beam_)
    {
      finished_grouping_->beamify(beat_length_, subdivide_beams_);
      Beam::set_beaming (finished_beam_, finished_grouping_);
      typeset_grob (finished_beam_);
      finished_beam_ = 0;
    
      delete finished_grouping_;
      finished_grouping_= 0;
    }
}

void
Auto_beam_engraver::start_translation_timestep ()
{
  count_ = 0;
  /*
    don't beam over skips
   */
  if (stems_)
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
Auto_beam_engraver::finalize ()
{
  /* finished beams may be typeset */
  typeset_beam ();
  /* but unfinished may need another announce/acknowledge pass */
  if (stems_)
    junk_beam ();
}


void
Auto_beam_engraver::acknowledge_grob (Grob_info info)
{
  if (stems_)
    {
      if (Beam::has_interface (info.grob_))
	{
	  end_beam ();
	}
      else if (Bar_line::has_interface (info.grob_))
	{
	  end_beam ();
	}
      else if (Rest::has_interface (info.grob_))
	{
	  end_beam ();
	}
    }
  
  if (Stem::has_interface (info.grob_))
    {
      Item* stem = dynamic_cast<Item *> (info.grob_);
      Music* m = info.music_cause ();
      if (!m->is_mus_type ("rhythmic-event"))
	{
	  programming_error ("Stem must have rhythmic structure");
	  return;
	}
      
      /*
	Don't (start) auto-beam over empty stems; skips or rests
	*/
      if (!Stem::head_count (stem))
	{
	  if (stems_)
	    end_beam ();
	  return;
	}

      if (Stem::get_beam (stem))
	{
	  if (stems_)
	    junk_beam ();
	  return ;
	}
	      
      int durlog  = unsmob_duration (m->get_mus_property ("duration"))->duration_log ();
      
      if (durlog <= 2)
	{
	  if (stems_)
	    end_beam ();
	  return;
	}


      /*
	ignore grace notes.
       */
      if (bool (beam_start_location_.grace_part_) != bool (now_mom ().grace_part_))
	return ;
	
      
      Moment dur = unsmob_duration (m->get_mus_property ("duration"))->length_mom ();
      /* FIXME:

	This comment has been here since long:

	   if shortest duration would change
	    consider ending and beginning beam first. 

	but the code didn't match: */
#if 1
      consider_end (dur);
      consider_begin (dur);

      if (dur < shortest_mom_)
	shortest_mom_ = dur;
#else
      /* I very much suspect that we wanted: */

      consider_end (shortest_mom_);
      if (dur < shortest_mom_)
	{
	  shortest_mom_ = dur;
	  consider_end (shortest_mom_);
	}
      consider_begin (shortest_mom_);
#endif

      if (!stems_)
	return;
      
      Moment now = now_mom ();
      
      grouping_->add_stem (now - beam_start_moment_ + beam_start_location_,
			     durlog - 2);
      stems_->push (stem);
      last_add_mom_ = now;
      extend_mom_ = (extend_mom_ >? now) + m->length_mom ();
    }
}

void
Auto_beam_engraver::process_acknowledged_grobs ()
{
  if (!count_)
    {
      consider_end (shortest_mom_);
      consider_begin (shortest_mom_);
    }
  else if (count_ > 1)
    {
      if (stems_)
	{
	  Moment now = now_mom ();
	  if ((extend_mom_ < now)
	      || ((extend_mom_ == now) && (last_add_mom_ != now)))
	    {
	      end_beam ();
	    }
	  else if (!stems_->size ())
	    {
	      junk_beam ();
	    }
	}    
    }

  /*
    count_++ -> 

        auto-beam-engraver.cc:459: warning: value computed is not used (gcc: 2.96) */
  count_ = count_ + 1;
}

ENTER_DESCRIPTION (Auto_beam_engraver,
/* descr */       "Generate beams based on measure characteristics and observed
Stems.  Uses beatLength, measureLength and measurePosition to decide
when to start and stop a beam.  Overriding beaming is done through
@ref{Stem_engraver} properties stemLeftBeamCount and
stemRightBeamCount.
",
/* creats*/       "Beam",
/* accepts */     "",
/* acks  */      "stem-interface rest-interface beam-interface bar-line-interface",
/* reads */       "autoBeaming autoBeamSettings beatLength subdivideBeams",
/* write */       "");
