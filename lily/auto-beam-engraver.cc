/*   
  auto-beam-engraver.cc --  implement Auto_beam_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2005 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "engraver.hh"
#include "beaming.hh"
#include "beam.hh"
#include "stem.hh"
#include "warn.hh"
#include "bar-line.hh"
#include "rest.hh"
#include "item.hh"
#include "spanner.hh"
#include "context.hh"

class Auto_beam_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Auto_beam_engraver);

protected:
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void process_music ();
  virtual bool try_music (Music*);
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
  bool is_same_grace_state (Grob* e);
  void typeset_beam ();

  Music *forbid_;
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

void
Auto_beam_engraver::process_music ()
{
  if (scm_is_string (get_property ("whichBar")))
    {
      consider_end (shortest_mom_);
      junk_beam ();
    }

  if (forbid_)
    {
      consider_end (shortest_mom_);
      junk_beam ();
    }
}


Auto_beam_engraver::Auto_beam_engraver ()
{
  forbid_ = 0;
  count_ = 0;
  stems_ = 0;
  shortest_mom_ = Moment (Rational (1, 8));
  finished_beam_ = 0;
  finished_grouping_ = 0;
  grouping_ = 0;
  beam_settings_ = SCM_EOL;  
}


bool
Auto_beam_engraver::try_music (Music*m)
{
  if (m->is_mus_type  ("beam-forbid-event"))
    {
      forbid_ = m;
      return true;
    }

  return false;
}

/*
  Determine end moment for auto beaming (or begin moment, but mostly
  0== anywhere) In order of increasing priority:
  
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
  Moment now = now_mom ();
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
  
  Moment beat_length (1,4);
  if (Moment * m = unsmob_moment (get_property ("beatLength")))
    {
      beat_length = *m;
    }
  Moment measure_length (1,1);
  int num = 4;
  if (Moment* m = unsmob_moment (get_property ("measureLength")))
    {
      num = int ((*m / beat_length).main_part_);
    }
  
  int den = beat_length.den ();
  SCM time = scm_list_n (scm_int2num (num), scm_int2num (den), SCM_UNDEFINED);

  SCM type = scm_list_n (scm_int2num (test_mom.num ()),
		      scm_int2num (test_mom.den ()), SCM_UNDEFINED);

  /*
    UGH UGH.
    settings aren't grob-properties.
   */
  SCM settings = get_property ("autoBeamSettings");
  
  /* first guess */
  
  /* begin beam at any position
 (and fallback for end) */
  Moment moment (0);
  
  /* end beam at end of beat */
  if (dir == STOP)
    {
      moment = robust_scm2moment (get_property ("beatLength"), moment);
    }

  /* second guess: property generic time exception */
  SCM m = scm_assoc (ly_append3 (function, wild, time), settings);
  
  if (m != SCM_BOOL_F && unsmob_moment (scm_cdr (m)))
    moment = * unsmob_moment (scm_cdr (m));

  /* third guess: property time exception, specific for duration type */
  m = scm_assoc (ly_append3 (function, type, time), settings);
  if (m != SCM_BOOL_F && unsmob_moment (scm_cdr (m)))
    moment = * unsmob_moment (scm_cdr (m));

  /* fourth guess [user override]: property plain generic */
  m = scm_assoc (ly_append3 (function, wild, wild), settings);
  if (m != SCM_BOOL_F && unsmob_moment (scm_cdr (m)))
    moment = * unsmob_moment (scm_cdr (m));

  /* fifth guess [user override]: property plain, specific for duration type */
  m = scm_assoc (ly_append3 (function, type, wild), settings);
  if (m != SCM_BOOL_F && unsmob_moment (scm_cdr (m)))
    moment = * unsmob_moment (scm_cdr (m));
  
  Rational r;
  if (moment.to_bool ())
    {
      /* Ugh? measurePosition can be negative, when \partial
	 We may have to fix this elsewhere (timing translator)
	r = unsmob_moment (get_property ("measurePosition"))->mod_rat (moment);
      */
      Moment pos (0);
      if (Moment  *m = unsmob_moment (get_property ("measurePosition")))
	{
	  pos = *m;
	}
	  
      if (pos < Moment (0))
	{
	  Moment length(1);

	  if ( Moment *m =  unsmob_moment (get_property ("measureLength")))
	    {
	      length = *m;
	    }
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
  if (!stems_ && on
      && !forbid_)
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

Spanner *
Auto_beam_engraver::create_beam ()
{
  if (to_boolean (get_property ("skipTypesetting")))
    return 0;
  
  Spanner *beam = new Spanner (beam_settings_, context ()->get_grob_key ("Beam"));
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
  
  announce_grob (beam, (*stems_)[0]->self_scm ());

  return beam;
}

void
Auto_beam_engraver::begin_beam ()
{
  if (stems_ || grouping_ )
    {
      programming_error ("already have autobeam");
      return; 
    }
  
  stems_ = new Link_array<Item>;
  grouping_ = new Beaming_info_list;
  beam_settings_ = updated_grob_properties (context (), ly_symbol2scm ("Beam"));
  
  beam_start_moment_ = now_mom ();
  beam_start_location_ =
    robust_scm2moment (get_property ("measurePosition"), Moment(0));
  subdivide_beams_ = ly_scm2bool (get_property ("subdivideBeams"));
  beat_length_ = robust_scm2moment (get_property ("beatLength"), Moment(1,4));
}

void
Auto_beam_engraver::junk_beam () 
{
  if (!stems_)
    return ;
  
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
      finished_grouping_->beamify (beat_length_, subdivide_beams_);
      Beam::set_beaming (finished_beam_, finished_grouping_);
      finished_beam_ = 0;
    
      delete finished_grouping_;
      finished_grouping_ = 0;
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
  forbid_ = 0;
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
  /*
    Duplicated from process_music(), since
    Repeat_acknowledge_engraver::process_music() may also set whichBar
   */
  if (scm_is_string (get_property ("whichBar"))
      && beam_start_moment_ < now_mom())
    {
      consider_end (shortest_mom_);
      junk_beam ();
    }
  
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
	      
      int durlog  = unsmob_duration (m->get_property ("duration"))->duration_log ();
      
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
	
      
      Moment dur = unsmob_duration (m->get_property ("duration"))->get_length ();
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
      extend_mom_ = (extend_mom_ >? now) + m->get_length ();
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
  
  count_ ++;
}

ADD_TRANSLATOR (Auto_beam_engraver,
/* descr */       "Generate beams based on measure characteristics and observed "
"Stems.  Uses beatLength, measureLength and measurePosition to decide "
"when to start and stop a beam.  Overriding beaming is done through "
"@ref{Stem_engraver} properties @code{stemLeftBeamCount} and "
"@code{stemRightBeamCount}. "
,
/* creats*/       "Beam",
/* accepts */     "beam-forbid-event",
/* acks  */      "stem-interface rest-interface beam-interface bar-line-interface",
/* reads */       "autoBeaming autoBeamSettings beatLength subdivideBeams",
/* write */       "");
