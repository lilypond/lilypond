/*
  new-part-combine-music-iterator.cc -- implement Part_combine_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2004--2006 Han-Wen Nienhuys
*/

#include "context.hh"
#include "music.hh"
#include "music-sequence.hh"
#include "lily-guile.hh"
#include "warn.hh"
#include "music-iterator.hh"

class Part_combine_iterator : public Music_iterator
{
public:
  Part_combine_iterator ();

  DECLARE_SCHEME_CALLBACK (constructor, ());
protected:
  virtual void derived_substitute (Context *f, Context *t);
  virtual void derived_mark () const;
  Part_combine_iterator (Part_combine_iterator const &);

  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void do_quit ();
  virtual void process (Moment);

  virtual Music_iterator *try_music_in_children (Music *) const;

  virtual bool ok () const;

private:
  Music_iterator *first_iter_;
  Music_iterator *second_iter_;
  Moment start_moment_;

  SCM split_list_;

  enum Status
    {
      APART,
      TOGETHER,
      SOLO1,
      SOLO2,
      UNISONO,
      UNISILENCE,
    };
  Status state_;
  Status playing_state_;

  /*
    Should be SOLO1 or SOLO2
  */
  Status last_playing_;

  /*
    TODO: this is getting of hand...
  */
  Context_handle one_;
  Context_handle two_;
  Context_handle null_;
  Context_handle shared_;
  Context_handle solo_;

  void substitute_both (Context *to1,
			Context *to2);

  void kill_mmrest (Context *);
  void chords_together ();
  void solo1 ();
  void solo2 ();
  void apart (bool silent);
  void unisono (bool silent);
};

static Music *busy_playing_event;

void
Part_combine_iterator::do_quit ()
{
  if (first_iter_)
    first_iter_->quit ();
  if (second_iter_)
    second_iter_->quit ();

  null_.set_context (0);
  one_.set_context (0);
  two_.set_context (0);
  shared_.set_context (0);
  solo_.set_context (0);
}

Part_combine_iterator::Part_combine_iterator ()
{
  first_iter_ = 0;
  second_iter_ = 0;
  split_list_ = SCM_EOL;
  state_ = APART;
  playing_state_ = APART;

  if (!busy_playing_event)
    {
      busy_playing_event
	= make_music_by_name (ly_symbol2scm ("BusyPlayingEvent"));
    }
}

void
Part_combine_iterator::derived_mark () const
{
  if (first_iter_)
    scm_gc_mark (first_iter_->self_scm ());
  if (second_iter_)
    scm_gc_mark (second_iter_->self_scm ());
}

void
Part_combine_iterator::derived_substitute (Context *f,
					   Context *t)
{
  if (first_iter_)
    first_iter_->substitute_outlet (f, t);
}

Moment
Part_combine_iterator::pending_moment () const
{
  Moment p;
  p.set_infinite (1);
  if (first_iter_->ok ())
    p = min (p, first_iter_->pending_moment ());

  if (second_iter_->ok ())
    p = min (p, second_iter_->pending_moment ());
  return p;
}

bool
Part_combine_iterator::ok () const
{
  return first_iter_->ok () || second_iter_->ok ();
}

void
Part_combine_iterator::chords_together ()
{
  if (state_ == TOGETHER)
    return;
  else
    {
      playing_state_ = TOGETHER;
      state_ = TOGETHER;

      substitute_both (shared_.get_outlet (), shared_.get_outlet ());
    }
}

void
Part_combine_iterator::kill_mmrest (Context *tg)
{
  static Music *mmrest;
  if (!mmrest)
    {
      mmrest = make_music_by_name (ly_symbol2scm ("MultiMeasureRestEvent"));
      mmrest->set_property ("duration", SCM_EOL);
    }

  tg->try_music (mmrest);
}

void
Part_combine_iterator::solo1 ()
{
  if (state_ == SOLO1)
    return;
  else
    {
      state_ = SOLO1;
      substitute_both (solo_.get_outlet (),
		       null_.get_outlet ());

      kill_mmrest (two_.get_outlet ());
      kill_mmrest (shared_.get_outlet ());

      if (playing_state_ != SOLO1)
	{
	  static Music *event;
	  if (!event)
	    event = make_music_by_name (ly_symbol2scm ("SoloOneEvent"));

	  first_iter_->try_music_in_children (event);
	}
      playing_state_ = SOLO1;
    }
}

void
Part_combine_iterator::substitute_both (Context *to1,
					Context *to2)
{
  Context *tos[] = {to1, to2};
  Music_iterator *mis[] = {first_iter_, second_iter_};
  Context_handle *hs[]
    = {
    &null_,
    &one_, &two_,
    &shared_, &solo_,
    0
  };

  for (int i = 0; i < 2; i++)
    {
      for (int j = 0; hs[j]; j++)
	if (hs[j]->get_outlet () != tos[i])
	  mis[i]->substitute_outlet (hs[j]->get_outlet (), tos[i]);
    }

  for (int j = 0; hs[j]; j++)
    {
      Context *t = hs[j]->get_outlet ();
      if (t != to1 && t != to2)
	kill_mmrest (t);
    }
}

void
Part_combine_iterator::unisono (bool silent)
{
  Status newstate = (silent) ? UNISILENCE : UNISONO;

  if (newstate == state_)
    return;
  else
    {
      /*
	If we're coming from SOLO2 state, we might have kill mmrests
	in the 1st voice, so in that case, we use the second voice
	as a basis for events.
      */
      Context *c1 = (last_playing_ == SOLO2) ? null_.get_outlet () : shared_.get_outlet ();
      Context *c2 = (last_playing_ == SOLO2) ? shared_.get_outlet () : null_.get_outlet ();
      substitute_both (c1, c2);
      kill_mmrest ((last_playing_ == SOLO2)
		   ? one_.get_outlet () : two_.get_outlet ());
      kill_mmrest (shared_.get_outlet ());

      if (playing_state_ != UNISONO
	  && newstate == UNISONO)
	{
	  static Music *event;
	  if (!event)
	    event = make_music_by_name (ly_symbol2scm ("UnisonoEvent"));

	  (last_playing_ == SOLO2 ? second_iter_ : first_iter_)
	    ->try_music_in_children (event);
	  playing_state_ = UNISONO;
	}
      state_ = newstate;
    }
}

void
Part_combine_iterator::solo2 ()
{
  if (state_ == SOLO2)
    return;
  else
    {
      state_ = SOLO2;

      substitute_both (null_.get_outlet (), solo_.get_outlet ());

      if (playing_state_ != SOLO2)
	{
	  static Music *event;
	  if (!event)
	    event = make_music_by_name (ly_symbol2scm ("SoloTwoEvent"));

	  second_iter_->try_music_in_children (event);
	  playing_state_ = SOLO2;
	}
    }
}

void
Part_combine_iterator::apart (bool silent)
{
  if (!silent)
    playing_state_ = APART;

  if (state_ == APART)
    return;
  else
    {
      state_ = APART;
      substitute_both (one_.get_outlet (), two_.get_outlet ());
    }
}

void
Part_combine_iterator::construct_children ()
{
  start_moment_ = get_outlet ()->now_mom ();
  split_list_ = get_music ()->get_property ("split-list");
  SCM lst = get_music ()->get_property ("elements");

  SCM props = scm_list_n (/*
			    used to have tweaks here.
			  */

			  SCM_UNDEFINED);

  Context *tr
    = get_outlet ()->find_create_context (ly_symbol2scm ("Voice"),
					  "shared", props);

  shared_.set_context (tr);

  /*
    If we don't, we get a new staff for every Voice.
  */
  set_context (tr);

  Context *solo_tr
    = get_outlet ()->find_create_context (ly_symbol2scm ("Voice"),
					  "solo", props);

  solo_.set_context (solo_tr);

  Context *null
    = get_outlet ()->find_create_context (ly_symbol2scm ("Devnull"),
					  "", SCM_EOL);

  if (!null)
    programming_error ("no Devnull found");

  null_.set_context (null);

  Context *one = tr->find_create_context (ly_symbol2scm ("Voice"),
					  "one", props);

  one_.set_context (one);

  set_context (one);
  first_iter_ = unsmob_iterator (get_iterator (unsmob_music (scm_car (lst))));

  Context *two = tr->find_create_context (ly_symbol2scm ("Voice"),
					  "two", props);
  two_.set_context (two);
  set_context (two);
  second_iter_ = unsmob_iterator (get_iterator (unsmob_music (scm_cadr (lst))));

  set_context (tr);

  char const *syms[]
    = {
    "Stem",
    "DynamicLineSpanner",
    "Tie",
    "Dots",
    "Rest",
    "Slur",
    "TextScript",
    "Script",
    0
  };

  for (char const **p = syms; *p; p++)
    {
      SCM sym = ly_symbol2scm (*p);
      execute_pushpop_property (one, sym,
				ly_symbol2scm ("direction"), scm_from_int (1));

      execute_pushpop_property (two, sym,
				ly_symbol2scm ("direction"), scm_from_int (-1));
    }
}

void
Part_combine_iterator::process (Moment m)
{
  Moment now = get_outlet ()->now_mom ();
  Moment *splitm = 0;

  for (; scm_is_pair (split_list_); split_list_ = scm_cdr (split_list_))
    {
      splitm = unsmob_moment (scm_caar (split_list_));
      if (splitm && *splitm + start_moment_ > now)
	break;

      SCM tag = scm_cdar (split_list_);

      if (tag == ly_symbol2scm ("chords"))
	chords_together ();
      else if (tag == ly_symbol2scm ("apart")
	       || tag == ly_symbol2scm ("apart-silence")
	       || tag == ly_symbol2scm ("apart-spanner"))
	apart (tag == ly_symbol2scm ("apart-silence"));
      else if (tag == ly_symbol2scm ("unisono"))
	unisono (false);
      else if (tag == ly_symbol2scm ("unisilence"))
	unisono (true);
      else if (tag == ly_symbol2scm ("solo1"))
	solo1 ();
      else if (tag == ly_symbol2scm ("solo2"))
	solo2 ();
      else if (scm_is_symbol (tag))
	{
	  string s = "Unknown split directive: "
	    + (scm_is_symbol (tag) ? ly_symbol2string (tag) : string ("not a symbol"));
	  programming_error (s);
	}
    }

  if (first_iter_->ok ())
    {
      first_iter_->process (m);
      if (first_iter_->try_music_in_children (busy_playing_event))
	last_playing_ = SOLO1;
    }

  if (second_iter_->ok ())
    {
      second_iter_->process (m);
      if (second_iter_->try_music_in_children (busy_playing_event))
	last_playing_ = SOLO2;
    }
}

Music_iterator *
Part_combine_iterator::try_music_in_children (Music *m) const
{
  Music_iterator *i = first_iter_->try_music (m);
  if (i)
    return i;
  else
    return second_iter_->try_music (m);
}

IMPLEMENT_CTOR_CALLBACK (Part_combine_iterator);
