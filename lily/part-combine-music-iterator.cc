/*   
  part-combine-music-iterator.cc -- implement  Part_combine_music_iterator

  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "part-combine-music-iterator.hh"
#include "translator-group.hh"
#include "event.hh"
#include "music-sequence.hh"
#include "lily-guile.hh"
#include "warn.hh"

Part_combine_music_iterator::Part_combine_music_iterator ()
{
  first_iter_ = 0;
  second_iter_ = 0;
  first_until_ = 0;
  second_until_ = 0;

  state_ = 0;
}

void
Part_combine_music_iterator::derived_mark () const
{
  if (first_iter_)
    scm_gc_mark (first_iter_->self_scm());
  if (second_iter_)
    scm_gc_mark(second_iter_->self_scm());
}

void
Part_combine_music_iterator::do_quit ()
{
  if (first_iter_)
    first_iter_->quit();
  if (second_iter_)
    second_iter_->quit();
}

Part_combine_music_iterator::Part_combine_music_iterator (Part_combine_music_iterator const &src)
  : Music_iterator (src)
{
  first_iter_ = 0;
  second_iter_ = 0;

  if(src.first_iter_)
    first_iter_ = src.first_iter_->clone ();
  if (src.second_iter_)
    second_iter_ = src.second_iter_->clone ();

  first_until_ = src.first_until_;
  second_until_ = src.second_until_;
  state_ = src.state_;
  suffix_ = src.suffix_;

  if (first_iter_)
    scm_gc_unprotect_object (first_iter_->self_scm());
  if (second_iter_)
    scm_gc_unprotect_object (second_iter_->self_scm());
}

Moment
Part_combine_music_iterator::pending_moment () const
{
  Moment p;
  p.set_infinite (1);
  if (first_iter_->ok ())
    p = p <? first_iter_->pending_moment ();

  if (second_iter_->ok ())
    p = p <? second_iter_->pending_moment ();
  return p;
}

bool
Part_combine_music_iterator::ok () const
{
  return first_iter_->ok () || second_iter_->ok ();
}


void
Part_combine_music_iterator::construct_children ()
{
  SCM lst = get_music ()->get_mus_property ("elements");
  
  
  first_iter_ = unsmob_iterator (get_iterator (unsmob_music (gh_car (lst))));
  second_iter_ = unsmob_iterator (get_iterator (unsmob_music (gh_cadr (lst))));
}

void
Part_combine_music_iterator::change_to (Music_iterator *it, String to_type,
					String to_id)
{
  Translator_group * current = it->report_to ();
  Translator_group * last = 0;

  /*
    Cut & Paste from from Auto_change_iterator from Change_iterator (ugh).

    TODO: abstract this function 
   */
  
  /* find the type  of translator that we're changing.
     
     If \translator Staff = bass, then look for Staff = *
   */
  while (current && current->type_string_ != to_type)
    {
      last = current;
      current = current->daddy_trans_;
    }

  if (current && current->id_string_ == to_id)
    {
      String msg;
      msg += _ ("Can't switch translators, I'm there already");
    }
  
  if (current) 
    if (last)
      {
	Translator_group * dest = 
	  it->report_to ()->find_create_translator (to_type, to_id);
	current->remove_translator (last);
	dest->add_used_group_translator (last);
      }
    else
      {
	/*
	  We could change the current translator's id, but that would make 
	  errors hard to catch
	  
	   last->translator_id_string_  = get_change ()->change_to_id_string_;
	*/
	error (_f ("I'm one myself: `%s'", to_type.to_str0 ()));
      }
  else
    error (_f ("none of these in my family: `%s'", to_id.to_str0 ()));
}


// SCM*, moet / kan dat niet met set_x ofzo?
static void
get_music_info (Moment m, Music_iterator* iter, SCM *pitches, SCM *durations)
{
  if (iter->ok ())
    {
      for (SCM i = iter->get_pending_events (m); gh_pair_p (i); i = ly_cdr (i))
	{
	  Music *m = unsmob_music (ly_car (i));
	  SCM p = m->get_mus_property ("pitch");
	  SCM d = m->get_mus_property ("duration");
	  if (unsmob_pitch (p))
	    *pitches = gh_cons (p, *pitches);
	  if (unsmob_duration (d))
	    *durations = gh_cons (d, *durations);
	}
    }
}

int
Part_combine_music_iterator::get_state (Moment)
{
  int state = UNKNOWN;
  
  Music *p = get_music ();

  String w = ly_scm2string (p->get_mus_property ("what"));
    
  
  Translator_group *first_translator = first_iter_->report_to ()->find_create_translator (w, "one" + suffix_);

  SCM s = first_translator->get_property ("changeMoment");
  if (!gh_pair_p (s))
    return state;

  Moment change_mom = *unsmob_moment (ly_car (s));
  Moment diff_mom = *unsmob_moment (ly_cdr (s));
  
  Moment now = pending_moment ();

  if (!now.main_part_.mod_rat (change_mom.main_part_))
    {
      SCM interval = SCM_BOOL_F;
      if (first_until_ < now)
	first_until_ = now;
      if (second_until_ < now)
	second_until_ = now;

      Moment first_mom = first_until_;
      Moment second_mom = second_until_;
      Moment diff_until = diff_mom + now;


      bool first = true;
      Music_iterator *first_iter = first_iter_->clone ();
      Music_iterator *second_iter = second_iter_->clone ();
      
      Moment last_pending (-1);
      Moment pending = now;
      while (now < diff_until
	      && (first_iter->ok () || second_iter->ok ())

	     // urg, this is a hack, haven't caught this case yet
	     && (pending != last_pending))
	{
	  if (!second_iter->ok ())
	    pending = first_iter->pending_moment ();
	  else if (!first_iter->ok ())
	    pending = second_iter->pending_moment ();
	  else
	    pending = first_iter->pending_moment () <? second_iter->pending_moment ();
	  last_pending = pending;

	  SCM first_pitches = SCM_EOL;
	  SCM first_durations = SCM_EOL;
	  get_music_info (pending, first_iter,
			  &first_pitches, &first_durations);
      
	  SCM second_pitches = SCM_EOL;
	  SCM second_durations = SCM_EOL;
	  get_music_info (pending, second_iter,
			  &second_pitches, &second_durations);

	  if (first_pitches != SCM_EOL && second_pitches != SCM_EOL)
	    {
	      scm_sort_list_x (first_pitches, Pitch::less_p_proc);
	      scm_sort_list_x (second_pitches, Pitch::less_p_proc);

	      interval = gh_int2scm (unsmob_pitch (ly_car (first_pitches))->steps ()
				     - unsmob_pitch (ly_car (scm_last_pair (second_pitches)))->steps ());
	    }

	  if (first_durations != SCM_EOL)
	    {
	      scm_sort_list_x (first_durations,
			       Duration::less_p_proc);
	      first_mom += unsmob_duration (ly_car (first_durations))->get_length ();
	    }
	  
	  if (second_durations != SCM_EOL)
	    {
	      scm_sort_list_x (second_durations,
			       Duration::less_p_proc);
	      second_mom += unsmob_duration (ly_car (second_durations))->get_length ();
	    }
	  
	  if (first_pitches != SCM_EOL && second_pitches == SCM_EOL
		  && ! (second_until_ > now))
	    {
	      state |= UNRELATED;
	      state &= ~UNISILENCE;
	      if (! (state & ~ (UNRELATED | SOLO1 | UNISILENCE)))
		state |= SOLO1;
	    }
	  else
	    state &= ~SOLO1;

	  if (first_pitches == SCM_EOL && second_pitches != SCM_EOL
	      && ! (first_until_ > now))
	    {
	      state |= UNRELATED;
	      state &= ~UNISILENCE;
	      if (! (state & ~ (UNRELATED | SOLO2 | UNISILENCE)))
		state |= SOLO2;
	    }
	  else
	    state &= ~SOLO2;

 	  if (gh_equal_p (first_durations, second_durations))
	    {
	      state &= ~UNISILENCE;
	      if (! (state & ~ (UNIRHYTHM | UNISON)))
		state |= UNIRHYTHM;
	    }
	  else
	    state &= ~ (UNIRHYTHM | UNISILENCE);
	  
	  if (first_pitches != SCM_EOL
	      && gh_equal_p (first_pitches, second_pitches))
	    {
	      state &= ~UNISILENCE;
	      if (! (state & ~ (UNIRHYTHM | UNISON)))
		state |= UNISON;
	    }
	  else
	    state &= ~ (UNISON);
	    
	  if (first_pitches == SCM_EOL && second_pitches == SCM_EOL)
	    {
	      if (! (state & ~ (UNIRHYTHM | UNISILENCE)))
		state |= UNISILENCE;
	    }
	  else if (!state)
	    state |= UNRELATED;
	  else
	    state &= ~ (UNISILENCE);

	  if (gh_number_p (interval))
	    {
	      SCM s = first_translator->get_property ("splitInterval");
	      int i = gh_scm2int (interval);
	      if (gh_pair_p (s)
		  && gh_number_p (ly_car (s))
		  && gh_number_p (ly_cdr (s))
		  && i >= gh_scm2int (ly_car (s))
		  && i <= gh_scm2int (ly_cdr (s)))
		{
		  if (! (state & ~ (SPLIT_INTERVAL | UNIRHYTHM | UNISON)))
		    state |= SPLIT_INTERVAL;
		}
	      else
		state &= ~ (SPLIT_INTERVAL);
	    }

	  if (first && first_pitches != SCM_EOL)
	    first_until_ = first_mom;
	  if (first && second_pitches != SCM_EOL)
	    second_until_ = second_mom;
	  first = false;

	  if (first_iter->ok ())
	    first_iter->skip (pending);
	  if (second_iter->ok ())
	    second_iter->skip (pending);
	  now = pending;
	}
      scm_gc_unprotect_object (first_iter->self_scm ());
      scm_gc_unprotect_object (second_iter->self_scm ());
    }

  return state;
}

static Music* abort_req = NULL;

void
Part_combine_music_iterator::process (Moment m)
{

  /*
    TODO:
    - Use three named contexts (be it Thread or Voice): one, two, solo.
      Let user pre-set (pushproperty) stem direction, remove
      dynamic-engraver, and such.

      **** Tried this, but won't work:

s      Consider thread switching: threads "one", "two" and "both".
      User can't pre-set the (most important) stem direction at
      thread level!
   */
 
  if (suffix_.empty_b ())
    suffix_ = first_iter_->report_to ()
      ->daddy_trans_->id_string_.cut_string (3, INT_MAX);

  int state = get_state (m);
  if (state)
    state_ = state;
  else
    state = state_;
  
  Music *p =get_music ();


  bool previously_combined_b = first_iter_->report_to ()->daddy_trans_
    == second_iter_->report_to ()->daddy_trans_;

  bool combine_b = previously_combined_b;

  if (! (state & UNIRHYTHM)
      || (state & SPLIT_INTERVAL)
      || (state & (SOLO1 | SOLO2)))
    combine_b = false;
  else if (state & (UNIRHYTHM | UNISILENCE))
    combine_b = true;

  /*
    When combining, abort all running spanners
   */

  if (!abort_req)
    {
      abort_req = make_music_by_name (ly_symbol2scm ("AbortEvent"));
    }
  
  if (combine_b && combine_b != previously_combined_b)
    {
      if (second_iter_ && second_iter_->ok ())
	second_iter_->try_music (abort_req);
     }
  String w = ly_scm2string (p->get_mus_property ("what"));
  if (combine_b != previously_combined_b)
    change_to (second_iter_, w, (combine_b ? "one" : "two")
	       + suffix_);
  
  Translator_group *first_translator = first_iter_->report_to ()->find_create_translator (w, "one" + suffix_);
  Translator_group *second_translator = second_iter_->report_to ()->find_create_translator (w, "two" + suffix_);
  

  /* Hmm */
  first_translator->set_property ("combineParts", SCM_BOOL_T);
  second_translator ->set_property ("combineParts", SCM_BOOL_T);
 
 
  /* hmm */
  SCM b = (state & UNIRHYTHM) ? SCM_BOOL_T : SCM_BOOL_F;
  first_translator->set_property ("unirhythm", b);
  second_translator->set_property ("unirhythm", b);

  b = (state & SPLIT_INTERVAL) ? SCM_BOOL_T : SCM_BOOL_F;
  first_translator->set_property ("split-interval", b);
  second_translator->set_property ("split-interval",  b);

  b = (state & UNISILENCE) ? SCM_BOOL_T : SCM_BOOL_F;
  first_translator->set_property ("unisilence", b);
  second_translator->set_property ("unisilence", b);

  // difference in definition...
  //b = ((state & UNISON) ? SCM_BOOL_T : SCM_BOOL_F;
  b = ((state & UNISON) && (state & UNIRHYTHM)) ? SCM_BOOL_T : SCM_BOOL_F;
  first_translator->set_property ("unison", b);
  second_translator->set_property ("unison", b);

  SCM b1 = (state & SOLO1) ? SCM_BOOL_T : SCM_BOOL_F;
  SCM b2 = (state & SOLO2) ? SCM_BOOL_T : SCM_BOOL_F;
  first_translator->set_property ("solo", b1);
  second_translator->set_property ("solo", b2);

  /* Can't these be computed? */
  first_translator->set_property ("othersolo", b2);
  second_translator->set_property ("othersolo", b1);

  if (first_iter_->ok ())
    first_iter_->process (m);
  
  if (second_iter_->ok ())
    second_iter_->process (m);
}

Music_iterator*
Part_combine_music_iterator::try_music_in_children (Music *m) const
{
  Music_iterator * i =  first_iter_->try_music (m);
  if (i)
    return i;
  else
    return second_iter_->try_music (m);
}


SCM
Part_combine_music_iterator::get_pending_events (Moment m)const
{
  SCM s = SCM_EOL;
  if (first_iter_)
    s = gh_append2 (s,first_iter_->get_pending_events (m));
  if (second_iter_)
    s = gh_append2 (second_iter_->get_pending_events (m),s);
  return s;
}

IMPLEMENT_CTOR_CALLBACK (Part_combine_music_iterator);
