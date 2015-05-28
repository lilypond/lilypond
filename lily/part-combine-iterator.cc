/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2015 Han-Wen Nienhuys

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "context.hh"
#include "dispatcher.hh"
#include "lily-guile.hh"
#include "music.hh"
#include "music-iterator.hh"
#include "music-sequence.hh"
#include "warn.hh"

static const char *const CONTEXT_ONE = "one";
static const char *const CONTEXT_TWO = "two";
static const char *const CONTEXT_SHARED = "shared";
static const char *const CONTEXT_SOLO = "solo";
static const char *const CONTEXT_NULL = "null";

class Part_combine_iterator : public Music_iterator
{
public:
  Part_combine_iterator ();

  DECLARE_SCHEME_CALLBACK (constructor, ());
protected:
  virtual void derived_substitute (Context *f, Context *t);
  virtual void derived_mark () const;

  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void do_quit ();
  virtual void process (Moment);

  virtual bool ok () const;

private:
  static const size_t NUM_PARTS = 2;
  Music_iterator *iterators_[NUM_PARTS];
  Moment start_moment_;

  SCM split_list_;

  Stream_event *mmrest_event_;

  enum Status
  {
    INITIAL,
    APART,
    TOGETHER,
    SOLO,
    UNISONO,
    UNISILENCE,
  };
  Status state_;

  // For states in which it matters, this is the relevant part,
  // e.g. 1 for Solo I, 2 for Solo II.
  int chosen_part_;

  void substitute_one (Music_iterator *iter, const char *voice_id);
  void substitute_both (const char *part1_voice_id, const char *part2_voice_id);
  bool is_active_outlet (const Context *c) const;
  void kill_mmrest (Context *c);
  void chords_together ();
  void solo1 ();
  void solo2 ();
  void apart ();
  void unisono (bool silent, int newpart);
};

const size_t Part_combine_iterator::NUM_PARTS;

void
Part_combine_iterator::do_quit ()
{
  for (size_t i = 0; i < NUM_PARTS; i++)
    if (iterators_[i])
      iterators_[i]->quit ();
}

Part_combine_iterator::Part_combine_iterator ()
{
  mmrest_event_ = 0;

  for (size_t i = 0; i < NUM_PARTS; i++)
    iterators_[i] = 0;
  split_list_ = SCM_EOL;
  state_ = INITIAL;
  chosen_part_ = 1;
}

void
Part_combine_iterator::derived_mark () const
{
  for (size_t i = 0; i < NUM_PARTS; i++)
    if (iterators_[i])
      scm_gc_mark (iterators_[i]->self_scm ());

  if (mmrest_event_)
    scm_gc_mark (mmrest_event_->self_scm ());
}

void
Part_combine_iterator::derived_substitute (Context *f,
                                           Context *t)
{
  // (Explain why just iterators_[0].)
  if (iterators_[0])
    iterators_[0]->substitute_outlet (f, t);
}

Moment
Part_combine_iterator::pending_moment () const
{
  Moment p;
  p.set_infinite (1);

  for (size_t i = 0; i < NUM_PARTS; i++)
    if (iterators_[i]->ok ())
      p = min (p, iterators_[i]->pending_moment ());

  return p;
}

bool
Part_combine_iterator::ok () const
{
  for (size_t i = 0; i < NUM_PARTS; i++)
    if (iterators_[i]->ok ())
      return true;

  return false;
}

void
Part_combine_iterator::substitute_one (Music_iterator *iter,
                                       const char *voice_id)
{
  Context *c = iter->get_outlet ();
  if (!c)
    {
      programming_error ("no context");
      return;
    }
  c = c->get_parent_context ();
  if (!c)
    {
      programming_error ("no parent context");
      return;
    }
  c = find_context_below (c, ly_symbol2scm("Voice"), voice_id);
  if (!c)
    {
      string s = "can not find Voice context: ";
      s += voice_id;
      programming_error (s);
      return;
    }
  iter->substitute_outlet (iter->get_outlet (), c);
}

void
Part_combine_iterator::substitute_both (const char *part1_voice_id,
                                        const char *part2_voice_id)
{
  // TODO: There is no good reason to tie the parts together here.
  // Factor out per-part stuff into a new class of iterator which
  // reads a part-specific list similar to the existing combined
  // "split-list".
  substitute_one(iterators_[0], part1_voice_id);
  substitute_one(iterators_[1], part2_voice_id);
}

bool Part_combine_iterator::is_active_outlet (const Context *c) const
{
  for (size_t i = 0; i < NUM_PARTS; i++)
    if (iterators_[i] && (iterators_[i]->get_outlet () == c))
      return true;

  return false;
}

void
Part_combine_iterator::kill_mmrest (Context *c)
{

  if (!mmrest_event_)
    {
      mmrest_event_ = new Stream_event
        (scm_call_1 (ly_lily_module_constant ("ly:make-event-class"),
                     ly_symbol2scm ("multi-measure-rest-event")));
      mmrest_event_->set_property ("duration", SCM_EOL);
      mmrest_event_->unprotect ();
    }

  c->event_source ()->broadcast (mmrest_event_);
}

void
Part_combine_iterator::unisono (bool silent, int newpart)
{
  Status newstate = (silent) ? UNISILENCE : UNISONO;

  if ((newstate == state_) and (newpart == chosen_part_))
    return;
  else
    {
      const char *c1 = (newpart == 2) ? CONTEXT_NULL : CONTEXT_SHARED;
      const char *c2 = (newpart == 2) ? CONTEXT_SHARED : CONTEXT_NULL;
      substitute_both (c1, c2);

      state_ = newstate;
      chosen_part_ = newpart;
    }
}

void
Part_combine_iterator::solo1 ()
{
  if ((state_ == SOLO) && (chosen_part_ == 1))
    return;
  else
    {
      state_ = SOLO;
      chosen_part_ = 1;
      substitute_both (CONTEXT_SOLO, CONTEXT_NULL);
    }
}

void
Part_combine_iterator::solo2 ()
{
  if ((state_ == SOLO) and (chosen_part_ == 2))
    return;
  else
    {
      state_ = SOLO;
      chosen_part_ = 2;
      substitute_both (CONTEXT_NULL, CONTEXT_SOLO);
    }
}

void
Part_combine_iterator::chords_together ()
{
  if (state_ == TOGETHER)
    return;
  else
    {
      state_ = TOGETHER;

      substitute_both (CONTEXT_SHARED, CONTEXT_SHARED);
    }
}

void
Part_combine_iterator::apart ()
{
  if (state_ == APART)
    return;
  else
    {
      state_ = APART;
      substitute_both (CONTEXT_ONE, CONTEXT_TWO);
    }
}

void
Part_combine_iterator::construct_children ()
{
  start_moment_ = get_outlet ()->now_mom ();
  split_list_ = get_music ()->get_property ("split-list");

  SCM lst = get_music ()->get_property ("elements");
  iterators_[0] = unsmob<Music_iterator> (get_iterator (unsmob<Music> (scm_car (lst))));
  iterators_[1] = unsmob<Music_iterator> (get_iterator (unsmob<Music> (scm_cadr (lst))));
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

  Context *prev_active_outlets[NUM_PARTS];
  for (size_t i = 0; i < NUM_PARTS; i++)
    prev_active_outlets[i] = iterators_[i]->get_outlet ();

  for (; scm_is_pair (split_list_); split_list_ = scm_cdr (split_list_))
    {
      splitm = unsmob<Moment> (scm_caar (split_list_));
      if (splitm && *splitm + start_moment_ > now)
        break;

      SCM tag = scm_cdar (split_list_);

      if (scm_is_eq (tag, ly_symbol2scm ("chords")))
        chords_together ();
      else if (scm_is_eq (tag, ly_symbol2scm ("apart"))
               || scm_is_eq (tag, ly_symbol2scm ("apart-silence"))
               || scm_is_eq (tag, ly_symbol2scm ("apart-spanner")))
        apart ();
      else if (scm_is_eq (tag, ly_symbol2scm ("unisono")))
        {
          // Continue to use the most recently used part because we might have
          // killed mmrests in the other part.
          unisono (false, (chosen_part_ == 2) ? 2 : 1);
        }
      else if (scm_is_eq (tag, ly_symbol2scm ("unisilence")))
        {
          // as for unisono
          unisono (true, (chosen_part_ == 2) ? 2 : 1);
        }
      else if (scm_is_eq (tag, ly_symbol2scm ("silence1")))
        unisono (true, 1);
      else if (scm_is_eq (tag, ly_symbol2scm ("silence2")))
        unisono (true, 2);
      else if (scm_is_eq (tag, ly_symbol2scm ("solo1")))
        solo1 ();
      else if (scm_is_eq (tag, ly_symbol2scm ("solo2")))
        solo2 ();
      else if (scm_is_symbol (tag))
        {
          string s = "Unknown split directive: "
                     + (scm_is_symbol (tag) ? ly_symbol2string (tag) : string ("not a symbol"));
          programming_error (s);
        }
    }

  bool any_outlet_changed = false;
  for (size_t i = 0; i < NUM_PARTS; i++)
    {
      if (iterators_[i]->ok ())
        iterators_[i]->process (m);

      if (prev_active_outlets[i] != iterators_[i]->get_outlet ())
          any_outlet_changed = true;
    }

  if (any_outlet_changed)
    {
      // Kill multi-measure rests in outlets that were previously active and
      // are no longer active.
      for (size_t i = 0; i < NUM_PARTS; i++)
        {
          Context *c = prev_active_outlets[i];
          if (c && !is_active_outlet (c))
              kill_mmrest (c);
        }
    }
}

IMPLEMENT_CTOR_CALLBACK (Part_combine_iterator);
