/*   
  part-combine-music-iterator.cc -- implement  Part_combine_music_iterator

  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "part-combine-music.hh"
#include "part-combine-music-iterator.hh"
#include "translator-group.hh"
#include "musical-request.hh"
#include "warn.hh"

Part_combine_music_iterator::Part_combine_music_iterator ()
{
  combined_b_ = true;

  now_ = 0;
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
Part_combine_music_iterator::next_moment () const
{
  Moment first_next = infinity_mom;
  if (first_iter_p_->ok ())
    first_next = first_iter_p_->next_moment ();
  Moment second_next = infinity_mom;
  if (second_iter_p_->ok ())
    second_next = second_iter_p_->next_moment ();
  if (first_next == infinity_mom && second_next == infinity_mom)
    return 0;
  return first_next <? second_next;
}

bool
Part_combine_music_iterator::ok () const
{
  //hmm
  return first_iter_p_->ok () || second_iter_p_->ok ();
}

void
Part_combine_music_iterator::do_print () const
{
  first_iter_p_->print ();
  second_iter_p_->print ();
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
	dest->add_translator (last);
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

Pitch_interrogate_req* first_spanish_inquisition; // nobody expects it
Pitch_interrogate_req* second_spanish_inquisition; // won't strike twice

Rhythm_interrogate_req* first_rhythmic_inquisition;
Rhythm_interrogate_req* second_rhythmic_inquisition;

void
Part_combine_music_iterator::do_process_and_next (Moment m)
{
  Part_combine_music const * p = dynamic_cast<Part_combine_music const* > (music_l_);

  now_ = next_moment ();

  /*
    Hmm, shouldn't we check per iterator if next_moment < m?
   */
  if (first_iter_p_->ok ())
    first_iter_p_->process_and_next (m);
  
  if (second_iter_p_->ok ())
    second_iter_p_->process_and_next (m);

  Music_iterator::do_process_and_next (m);

  /*
    TODO:

    * Maybe we need a Skip_engraver?

    (check): can this all be handled now?
    
    Maybe different modes exist?

    * Wind instruments (Flute I/II)
    * Hymnals:  


      Rules for Hymnals/SATB (John Henckel <henckel@iname.com>):

      1. if S and A differ by less than a third, the stems should be up/down.
      2. else if S and A have different values, the stems should be up/down.
      3. else if A sings "la" or higher, both S and A stems should be down.
      4. else both stems should be up

    * This may get really tricky: combining voices/staffs: string instruments

   */
  
  if (!first_spanish_inquisition)
    first_spanish_inquisition = new Pitch_interrogate_req;
  first_iter_p_->try_music (first_spanish_inquisition);
  
  if (!second_spanish_inquisition)
    second_spanish_inquisition = new Pitch_interrogate_req;
  second_iter_p_->try_music (second_spanish_inquisition);

  Array<Musical_pitch>* first_pitches = &first_spanish_inquisition->pitch_arr_;
  Array<Musical_pitch>* second_pitches = &second_spanish_inquisition->pitch_arr_;

  if (!first_rhythmic_inquisition)
    first_rhythmic_inquisition = new Rhythm_interrogate_req;
  first_iter_p_->try_music (first_rhythmic_inquisition);

  if (!second_rhythmic_inquisition)
    second_rhythmic_inquisition = new Rhythm_interrogate_req;
  second_iter_p_->try_music (second_rhythmic_inquisition);

  Array<Duration>* first_durations = &first_rhythmic_inquisition->duration_arr_;
  Array<Duration>* second_durations = &second_rhythmic_inquisition->duration_arr_;

  SCM interval = SCM_BOOL_F;
  if (first_pitches->size () && second_pitches->size ())
    {
      first_pitches->sort (Musical_pitch::compare);
      second_pitches->sort (Musical_pitch::compare);
      interval = gh_int2scm (first_pitches->top ().steps ()
			     - (*second_pitches)[0].steps ());
    }
  if (first_durations->size ())
    {
      first_durations->sort (Duration::compare);
      Moment new_until = now_ + first_durations->top ().length_mom ();
      if (new_until > first_until_)
	first_until_ = new_until;
    }

    if (second_durations->size ())
    {
      second_durations->sort (Duration::compare);
      Moment new_until = now_ + second_durations->top ().length_mom ();
      if (new_until > second_until_)
	second_until_ = new_until;
    }

  Translator_group * fir = first_iter_p_->report_to_l ();
  Translator_group * sir = second_iter_p_->report_to_l ();

  bool solo_b = (first_pitches->empty () != second_pitches->empty ())
    && !(first_until_ > now_ && second_until_ > now_);

  bool unirhythm_b = !solo_b && !compare (first_durations, second_durations);
  bool unison_b = unirhythm_b && !first_pitches->empty ()
    &&!compare (first_pitches, second_pitches);
  bool unisilence_b = unirhythm_b && first_pitches->empty ();

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

  /*
    Hmm, maybe we should set/check combined_b_ against

       first_iter_p_->report_to_l () == second_iter_p_->report_to_l ()

   ? 
   */

  String to_id =  combined_b_ ? "one" : "two";
  if ((!unirhythm_b && combined_b_)
      || (split_interval_b && combined_b_)
      || (solo_b && combined_b_)
      /*|| (unisilence_b && combined_b_) */
      || ((unirhythm_b || unison_b || unisilence_b)
	  && !combined_b_ && !split_interval_b && !solo_b))
    {
      combined_b_ = !combined_b_;
      to_id =  combined_b_ ? "one" : "two";
      change_to (second_iter_p_, p->what_str_, to_id);
    }

  if (!combined_b_)
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
  if (first_pitches->size ())
    {
      fd->set_property ("solo", b);
      sd->set_property ("solo", SCM_BOOL_F);
    }

  if (second_pitches->size ())
    {
      fd->set_property ("solo", SCM_BOOL_F);
      sd->set_property ("solo", b);
    }

  first_pitches->clear ();
  second_pitches->clear ();
  first_durations->clear ();
  second_durations->clear ();
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

