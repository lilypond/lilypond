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
  //hmm
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

void
Part_combine_music_iterator::process (Moment m)
{

  /*
    TODO:
    - Use three named contexts (be it Thread or Voice): one, two, solo.
      Let user pre-set (pushproperty) stem direction, remove
      dynamic-engraver, and such.
    - staff-combiner must switch only on per-measure basis (maybe even on
      per-line-basis, but that's not feasible).  Maybe set minimum lengths
      of matching rhythm for combine/split?
   */
 
  Part_combine_music const * p = dynamic_cast<Part_combine_music const* > (music_l_);

  Moment now = pending_moment ();

  Array<Musical_pitch> first_pitches;
  Array<Duration> first_durations;
  if (first_iter_p_->ok ())
    {
      /* get_music () also performs next, modifying iterator */
      Music_iterator *mi = first_iter_p_->clone ();
      for (SCM i = mi->get_music (now); gh_pair_p (i); i = gh_cdr (i))
	{
	  Music *m = unsmob_music (gh_car (i));
	  if (Melodic_req *r = dynamic_cast<Melodic_req *> (m))
	    first_pitches.push (r->pitch_);
	  if (Rhythmic_req *r = dynamic_cast<Rhythmic_req *> (m))
		    first_durations.push (r->duration_);
	}
      delete mi;
    }
  
  Array<Musical_pitch> second_pitches;
  Array<Duration> second_durations;
  if (second_iter_p_->ok ())
    {
      Music_iterator *mi = second_iter_p_->clone ();
      for (SCM i = mi->get_music (now); gh_pair_p (i); i = gh_cdr (i))
	{
	  Music *m = unsmob_music (gh_car (i));
	  if (Melodic_req *r = dynamic_cast<Melodic_req *> (m))
	    second_pitches.push (r->pitch_);
	  if (Rhythmic_req *r = dynamic_cast<Rhythmic_req *> (m))
	    second_durations.push (r->duration_);
	}
      delete mi;
    }
  
  SCM interval = SCM_BOOL_F;
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
      Moment new_until = now + first_durations.top ().length_mom ();
      if (new_until > first_until_)
	first_until_ = new_until;
    }

    if (second_durations.size ())
    {
      second_durations.sort (Duration::compare);
      Moment new_until = now + second_durations.top ().length_mom ();
      if (new_until > second_until_)
	second_until_ = new_until;
    }

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

  Translator_group * fir = first_iter_p_->report_to_l ();
  Translator_group * sir = second_iter_p_->report_to_l ();

  bool solo_b = (first_pitches.empty () != second_pitches.empty ())
    && !(first_until_ > now && second_until_ > now);

  bool unirhythm_b = !solo_b && !compare (&first_durations, &second_durations);
  bool unison_b = unirhythm_b && !first_pitches.empty ()
    &&!compare (&first_pitches, &second_pitches);
  bool unisilence_b = unirhythm_b && first_pitches.empty ();

  Translator_group * fd = fir->find_create_translator_l (p->what_str_, "one");
  Translator_group * sd = sir->find_create_translator_l (p->what_str_, "two");

  bool split_interval_b = false;
  if (gh_number_p (interval))
    {
      SCM s = fd->get_property (ly_symbol2scm ("splitInterval"));
      int i = gh_scm2int (interval);
      if (gh_pair_p (s)
	  && gh_number_p (gh_car (s))
	  && gh_number_p (gh_cdr (s))
	  && i >= gh_scm2int (gh_car (s))
	  && i <= gh_scm2int (gh_cdr (s)))
	split_interval_b = true;
    }

  bool combined_b = first_iter_p_->report_to_l ()->daddy_trans_l_
    == second_iter_p_->report_to_l ()->daddy_trans_l_;

  String to_id =  combined_b ? "one" : "two";
  if ((!unirhythm_b && combined_b)
      || (split_interval_b && combined_b)
      || (solo_b && combined_b)
      /*|| (unisilence_b && combined_b) */
      || ((unirhythm_b || unison_b || unisilence_b)
	  && !combined_b && !split_interval_b && !solo_b))
    {
      combined_b = !combined_b;
      to_id =  combined_b ? "one" : "two";
      change_to (second_iter_p_, p->what_str_, to_id);
    }

  if (!combined_b)
    sir = second_iter_p_->report_to_l ();

  SCM b = unirhythm_b ? SCM_BOOL_T : SCM_BOOL_F;
  fd->set_property ("unirhythm", b);
  sd->set_property ("unirhythm", b);

  b = split_interval_b ? SCM_BOOL_T : SCM_BOOL_F;
  fd->set_property ("split-interval", b);
  sd->set_property ("split-interval",  b);

  b = unisilence_b ? SCM_BOOL_T : SCM_BOOL_F;
  fd->set_property ("unisilence", b);
  sd->set_property ("unisilence", b);

  b = unison_b ? SCM_BOOL_T : SCM_BOOL_F;
  fd->set_property ("unison", b);
  sd->set_property ("unison", b);

  b = solo_b  ? SCM_BOOL_T : SCM_BOOL_F;
  if (first_pitches.size ())
    {
      fd->set_property ("solo", b);
      sd->set_property ("solo", SCM_BOOL_F);
    }

  if (second_pitches.size ())
    {
      fd->set_property ("solo", SCM_BOOL_F);
      sd->set_property ("solo", b);
    }

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
