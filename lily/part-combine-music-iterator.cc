/*   
  part-combine-music-iterator.cc -- implement  Part_combine_music_iterator

  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "part-combine-music.hh"
#include "part-combine-music-iterator.hh"
#include "translator-group.hh"
#include "musical-request.hh"
#include "music-sequence.hh"
#include "warn.hh"

Part_combine_music_iterator::Part_combine_music_iterator ()
{
  first_iter_p_ = 0;
  second_iter_p_ = 0;
  first_until_ = 0;
  second_until_ = 0;
}

Part_combine_music_iterator::~Part_combine_music_iterator ()
{
  delete second_iter_p_;
  delete first_iter_p_;
}

Moment
Part_combine_music_iterator::pending_moment () const
{
  Moment p;
  p.set_infinite (1);
  if (first_iter_p_->ok ())
    p = p <? first_iter_p_->pending_moment ();

  if (second_iter_p_->ok ())
    p = p <? second_iter_p_->pending_moment ();
  return p;
}

bool
Part_combine_music_iterator::ok () const
{
  return first_iter_p_->ok () || second_iter_p_->ok ();
}

void
Part_combine_music_iterator::construct_children ()
{
  Part_combine_music const * m = dynamic_cast<Part_combine_music const*> (music_l_);
  
  first_iter_p_ = get_iterator_p (m->first_l ());
  second_iter_p_ = get_iterator_p (m->second_l ());
}

void
Part_combine_music_iterator::change_to (Music_iterator *it, String to_type,
					String to_id)
{
  Translator_group * current = it->report_to_l ();
  Translator_group * last = 0;

  /*
    Cut & Paste from from Auto_change_iterator from Change_iterator (ugh).

    TODO: abstract this function 
   */
  
  /* find the type  of translator that we're changing.
     
     If \translator Staff = bass, then look for Staff = *
   */
  while  (current && current->type_str_ != to_type)
    {
      last = current;
      current = current->daddy_trans_l_;
    }

  if (current && current->id_str_ == to_id)
    {
      String msg;
      msg += _ ("Can't switch translators, I'm there already");
    }
  
  if (current) 
    if (last)
      {
	Translator_group * dest = 
	  it->report_to_l ()->find_create_translator_l (to_type, to_id);
	current->remove_translator_p (last);
	dest->add_group_translator (last);
      }
    else
      {
	/*
	  We could change the current translator's id, but that would make 
	  errors hard to catch
	  
	   last->translator_id_str_  = change_l ()->change_to_id_str_;
	*/
	error (_f ("I'm one myself: `%s'", to_type.ch_C ()));
      }
  else
    error (_f ("none of these in my family: `%s'", to_id.ch_C ()));
}


static void
get_music_info (Moment m, Music_iterator* iter, Array<Musical_pitch> *pitches, Array<Duration> *durations)
{
  if (iter->ok ())
    {
      for (SCM i = iter->get_music (m); gh_pair_p (i); i = gh_cdr (i))
	{
	  Music *m = unsmob_music (gh_car (i));
	  if (Melodic_req *r = dynamic_cast<Melodic_req *> (m))
	    pitches->push (r->pitch_);
	  if (Rhythmic_req *r = dynamic_cast<Rhythmic_req *> (m))
	    durations->push (r->duration_);
	}
    }
}
  
int
Part_combine_music_iterator::get_state (Moment)
{
  int state = UNKNOWN;
  Part_combine_music const *p = dynamic_cast<Part_combine_music const* > (music_l_);
  Translator_group *first_translator = first_iter_p_->report_to_l ()->find_create_translator_l (p->what_str_, "one");

  SCM s = first_translator->get_property (ly_symbol2scm ("changeMoment"));
  Moment change_mom = *unsmob_moment (gh_car (s));
  Moment diff_mom = *unsmob_moment (gh_cdr (s));
  
  Moment now = pending_moment ();

  if (!now.mod_rat (change_mom))
    {
      SCM interval = SCM_BOOL_F;
      Moment first_mom = first_until_ - now;
      Moment second_mom = second_until_ - now;

      bool first = true;
      Music_iterator *first_iter = first_iter_p_->clone ();
      Music_iterator *second_iter = second_iter_p_->clone ();
      while ((first_mom <? second_mom) < diff_mom
	     && (first_iter->ok () || second_iter->ok ()))
	{
	  Moment m;
	  if (!second_iter->ok ())
	    m = first_iter->pending_moment ();
	  else if (!first_iter->ok ())
	    m = second_iter->pending_moment ();
	  else
	    m = first_iter->pending_moment () <? second_iter->pending_moment ();

	  Array<Musical_pitch> first_pitches;
	  Array<Duration> first_durations;
	  get_music_info (m, first_iter, &first_pitches, &first_durations);
      
	  Array<Musical_pitch> second_pitches;
	  Array<Duration> second_durations;
	  get_music_info (m, second_iter, &second_pitches, &second_durations);

	  if (first_pitches.size () && second_pitches.size ())
	    {
	      first_pitches.sort (Musical_pitch::compare);
	      second_pitches.sort (Musical_pitch::compare);
	      interval = gh_int2scm (first_pitches.top ().steps ()
				     - second_pitches[0].steps ());
	    }
	  if (first_durations.size ())
	    {
	      first_durations.sort (Duration::compare);
	      first_mom += first_durations.top ().length_mom ();
	      if (first && !first_pitches.empty ())
		first_until_ = first_mom + now;
	    }

	  if (second_durations.size ())
	    {
	      second_durations.sort (Duration::compare);
	      second_mom += second_durations.top ().length_mom ();
	      if (first && !second_pitches.empty ())
		second_until_ = second_mom + now;
	    }
	  first = false;

#if 0 /* DEBUG */
	  printf ("now: %s\n", now.str ().ch_C ());
	  printf ("first: ");
	  for (int i = 0; i < first_pitches.size (); i++)
	    {
	      printf ("%s, ", first_pitches[i].str ().ch_C ());
	    }
	  printf ("\nsecond: ");
	  for (int i = 0; i < second_pitches.size (); i++)
	    {
	      printf ("%s, ", second_pitches[i].str ().ch_C ());
	    }
	  printf ("\n");
#endif

	  if (!first_pitches.empty () && second_pitches.empty ()
	      && !(second_until_ > now))
	    {
	      state |= UNRELATED;
	      state &= ~UNISILENCE;
	      if (!(state & ~(UNRELATED | SOLO1 | UNISILENCE)))
		state |= SOLO1;
	    }
	  else
	    state &= ~SOLO1;

	  if (first_pitches.empty () && !second_pitches.empty ()
	      && !(first_until_ > now))
	    {
	      state |= UNRELATED;
	      state &= ~UNISILENCE;
	      if (!(state & ~(UNRELATED | SOLO2 | UNISILENCE)))
		state |= SOLO2;
	    }
	  else
	    state &= ~SOLO2;

	  if (!compare (&first_durations, &second_durations))
	    {
	      state &= ~UNISILENCE;
	      if (!(state & ~(UNIRHYTHM | UNISON)))
		state |= UNIRHYTHM;
	    }
	  else
	    state &= ~(UNIRHYTHM | UNISILENCE);
	  
	  if (!first_pitches.empty ()
	      &&!compare (&first_pitches, &second_pitches))
	    {
	      state &= ~UNISILENCE;
	      if (!(state & ~(UNIRHYTHM | UNISON)))
		state |= UNISON;
	    }
	  else
	    state &= ~(UNISON);
	    
	  if (first_pitches.empty () && second_pitches.empty ())
	    {
	      if (!(state & ~(UNIRHYTHM | UNISILENCE)))
		state |= UNISILENCE;
	    }
	  else
	    state &= ~(UNISILENCE);

	  if (gh_number_p (interval))
	    {
	      SCM s = first_translator->get_property (ly_symbol2scm ("splitInterval"));
	      int i = gh_scm2int (interval);
	      if (gh_pair_p (s)
		  && gh_number_p (gh_car (s))
		  && gh_number_p (gh_cdr (s))
		  && i >= gh_scm2int (gh_car (s))
		  && i <= gh_scm2int (gh_cdr (s)))
		{
		  if (!(state & ~(SPLIT_INTERVAL | UNIRHYTHM | UNISON)))
		    state |= SPLIT_INTERVAL;
		}
	      else
		state &= ~(SPLIT_INTERVAL);
	    }

#if 0
	  Moment next = (first_mom <? second_mom) + now;
	  if (first_iter->ok ())
	    first_iter->skip (next);
	  if (second_iter->ok ())
	    second_iter->skip (next);
#else
	  if (first_iter->ok ())
	    first_iter->skip (first_mom + now);
	  if (second_iter->ok ())
	    second_iter->skip (second_mom + now);
#endif
	}
      delete first_iter;
      delete second_iter;
    }
  return state;
}

void
Part_combine_music_iterator::process (Moment m)
{

  /*
    TODO:
    - Use three named contexts (be it Thread or Voice): one, two, solo.
      Let user pre-set (pushproperty) stem direction, remove
      dynamic-engraver, and such.
   */
 
  int state = get_state (m);
  if (state)
    state_ = state;
  else
    state = state_;
  
  Part_combine_music const *p = dynamic_cast<Part_combine_music const* > (music_l_);

  bool combined_b = first_iter_p_->report_to_l ()->daddy_trans_l_
    == second_iter_p_->report_to_l ()->daddy_trans_l_;

  String to_id =  combined_b ? "one" : "two";
  if ((!(state & UNIRHYTHM) && combined_b)
      || ((state & SPLIT_INTERVAL) && combined_b)
      || ((state & (SOLO1 | SOLO2)) && combined_b)
      || (((state & (UNIRHYTHM | UNISILENCE))
	   && !combined_b && !(state & SPLIT_INTERVAL)
	   && !(state & (SOLO1 | SOLO2)))))
    {
      combined_b = !combined_b;
      to_id =  combined_b ? "one" : "two";
      change_to (second_iter_p_, p->what_str_, to_id);
    }


  Translator_group *first_translator = first_iter_p_->report_to_l ()->find_create_translator_l (p->what_str_, "one");
  Translator_group *second_translator = second_iter_p_->report_to_l ()->find_create_translator_l (p->what_str_, "two");

  /*
    hmm
   */
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

  if (first_iter_p_->ok ())
    first_iter_p_->process (m);
  
  if (second_iter_p_->ok ())
    second_iter_p_->process (m);
}

Music_iterator*
Part_combine_music_iterator::try_music_in_children (Music *m) const
{
  Music_iterator * i =  first_iter_p_->try_music (m);
  if (i)
    return i;
  else
    return second_iter_p_->try_music (m);
}


SCM
Part_combine_music_iterator::get_music (Moment m)const
{
  SCM s = SCM_EOL;
  if (first_iter_p_)
    s = gh_append2 (s,first_iter_p_->get_music (m));
  if (second_iter_p_)
    s = gh_append2 (second_iter_p_->get_music (m),s);
  return s;
}
