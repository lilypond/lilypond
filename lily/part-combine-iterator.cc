/*
  new-part-combine-music-iterator.cc -- implement Part_combine_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2004--2006 Han-Wen Nienhuys
*/

#include "context.hh"
#include "dispatcher.hh"
#include "lily-guile.hh"
#include "listener.hh"
#include "music.hh"
#include "music-iterator.hh"
#include "music-sequence.hh"
#include "warn.hh"

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

  virtual bool ok () const;

private:
  /* used by try_process */
  DECLARE_LISTENER (set_busy);
  bool busy_;
  bool notice_busy_;
  
  bool try_process (Music_iterator *i, Moment m);
  
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

  mmrest->send_to_context (tg);
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

	  event->send_to_context (first_iter_->get_outlet ());
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

	  Context *out = (last_playing_ == SOLO2 ? second_iter_ : first_iter_)
	    ->get_outlet ();
	  event->send_to_context (out);
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

	  event->send_to_context (second_iter_->get_outlet ());
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

  // Add listeners to all contexts except Devnull.
  Context *contexts[] = {one, two, solo_tr, tr, 0};
  for (int i = 0; contexts[i]; i++)
    {
      contexts[i]->event_source ()->add_listener (GET_LISTENER (set_busy), ly_symbol2scm ("MusicEvent"));
    }

  for (char const **p = syms; *p; p++)
    {
      SCM sym = ly_symbol2scm (*p);
      execute_pushpop_property (one, sym,
				ly_symbol2scm ("direction"), scm_from_int (1));

      execute_pushpop_property (two, sym,
				ly_symbol2scm ("direction"), scm_from_int (-1));
    }
}

IMPLEMENT_LISTENER (Part_combine_iterator, set_busy);
void
Part_combine_iterator::set_busy (SCM se)
{
  if (!notice_busy_)
    return;

  Stream_event *e = unsmob_stream_event (se);
  SCM mus = e->get_property ("music");
  Music *m = unsmob_music (mus);
  assert (m);

  if (m->is_mus_type ("note-event") || m->is_mus_type ("cluster-note-event"))
    busy_ = true;
}

/*
* Processes a moment in an iterator, and returns whether any new music was reported.
*/
bool
Part_combine_iterator::try_process (Music_iterator *i, Moment m)
{
  busy_ = false;
  notice_busy_ = true;

  i->process (m);
  
  notice_busy_ = false;
  return busy_;
}

void
Part_combine_iterator::process (Moment m)
{
  Moment now = get_outlet ()->now_mom ();
  Moment *splitm = 0;

  /* This is needed if construct_children was called before iteration
     started */
  if (start_moment_.main_part_.is_infinity () && start_moment_ < 0)
    start_moment_ = now;

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
      if (try_process (first_iter_, m))
        last_playing_ = SOLO1;
    }

  if (second_iter_->ok ())
    {
      if (try_process (second_iter_, m))
	last_playing_ = SOLO2;
    }
}

IMPLEMENT_CTOR_CALLBACK (Part_combine_iterator);
