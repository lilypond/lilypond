/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2019 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "music-wrapper-iterator.hh"

#include "context.hh"
#include "dispatcher.hh"
#include "input.hh"
#include "international.hh"
#include "lily-guile.hh"
#include "music-sequence.hh"
#include "music.hh"
#include "warn.hh"

class Quote_iterator : public Music_wrapper_iterator
{
public:
  Quote_iterator ();
  Moment vector_moment (int idx) const;
  Context_handle quote_outlet_;

  Moment start_moment_;
  Moment stop_moment_;
  SCM event_vector_;
  int event_idx_;
  int end_idx_;

  SCM transposed_musics_;

  DECLARE_SCHEME_CALLBACK (constructor, ());
  bool quote_ok () const;
  bool accept_music_type (Stream_event *, bool is_cue = true) const;

protected:
  void derived_mark () const override;
  void construct_children () override;
  Moment pending_moment () const override;
  void process (Moment) override;
  void do_quit () override;
  bool ok () const override;
};

void
Quote_iterator::do_quit ()
{
  Music_wrapper_iterator::do_quit ();
  quote_outlet_.set_context (0);
}

bool
Quote_iterator::accept_music_type (Stream_event *ev, bool is_cue) const
{
  SCM accept = SCM_EOL;
  // Cue notes use the quotedCueEventTypes property, otherwise (and as fallback
  // for cue notes if quotedCueEventTypes is not set) use quotedEventTypes
  if (is_cue)
    accept = get_outlet ()->get_property ("quotedCueEventTypes");
  if (scm_is_null (accept))
    accept = get_outlet ()->get_property ("quotedEventTypes");

  for (; scm_is_pair (accept); accept = scm_cdr (accept))
    {
      if (ev->internal_in_event_class (scm_car (accept)))
        return true;
    }
  return false;
}

void
Quote_iterator::derived_mark () const
{
  Music_wrapper_iterator::derived_mark ();
  scm_gc_mark (transposed_musics_);
}

Quote_iterator::Quote_iterator ()
{
  transposed_musics_ = SCM_EOL;
  event_vector_ = SCM_EOL;
  event_idx_ = 0;
  end_idx_ = 0;
}

int
binsearch_scm_vector (SCM vec, SCM key, bool (*is_less) (SCM a, SCM b))
{
  int lo = 0;
  int hi = int (scm_c_vector_length (vec));

  /* binary search */
  do
    {
      int cmp = (lo + hi) / 2;

      SCM when = scm_caar (scm_c_vector_ref (vec, cmp));
      bool result = (*is_less) (key, when);
      if (result)
        hi = cmp;
      else
        lo = cmp;
    }
  while (hi - lo > 1);

  return lo;
}

void
Quote_iterator::construct_children ()
{
  Music_wrapper_iterator::construct_children ();

  Context *cue_context = 0;

  SCM name = get_music ()->get_property ("quoted-context-type");
  if (scm_is_symbol (name))
    {
      SCM id = get_music ()->get_property ("quoted-context-id");
      std::string c_id = robust_scm2string (id, "");
      cue_context = get_outlet ()->find_create_context (name, c_id, SCM_EOL);
      if (!cue_context)
        {
          Input *origin = get_music ()->origin ();
          origin->warning (_f ("cannot find or create context: %s",
                               Context::diagnostic_id (name, c_id).c_str ()));
        }
    }

  if (!cue_context)
    cue_context = get_outlet ()->get_default_interpreter ();
  quote_outlet_.set_context (cue_context);

  event_vector_ = get_music ()->get_property ("quoted-events");

  /*
    We have to delay initting event_idx_ , since we have to
    take starting grace notes into account. Those may offset
    event_idx_.
  */
  event_idx_ = -1;
}

bool
Quote_iterator::ok () const
{
  return
    Music_wrapper_iterator::ok ()
    || quote_ok ();
}

bool
Quote_iterator::quote_ok () const
{
  return (event_idx_ >= 0
          && scm_is_vector (event_vector_)
          && event_idx_ <= end_idx_

          /*
            Don't quote the grace notes leading to an unquoted note.
          */
          && vector_moment (event_idx_).main_part_ < stop_moment_.main_part_);
}

Moment
Quote_iterator::pending_moment () const
{
  Rational infty;
  infty.set_infinite (1);
  Moment m (infty);

  if (Music_wrapper_iterator::ok ())
    m = min (m, Music_wrapper_iterator::pending_moment ());

  /*
    In case event_idx_ < 0, we're not initted yet, and the wrapped
    music expression determines the starting moment.
  */
  if (quote_ok ())
    m = min (m, vector_moment (event_idx_) - start_moment_);

  return m;
}

Moment
Quote_iterator::vector_moment (int idx) const
{
  SCM entry = scm_c_vector_ref (event_vector_, idx);
  return *unsmob<Moment> (scm_caar (entry));
}

void
Quote_iterator::process (Moment m)
{
  if (Music_wrapper_iterator::ok ())
    Music_wrapper_iterator::process (m);

  if (!scm_is_vector (event_vector_))
    return;

  if (event_idx_ < 0)
    {
      event_idx_ = binsearch_scm_vector (event_vector_,
                                         get_outlet ()->now_mom ().smobbed_copy (),
                                         &moment_less);
      start_moment_ = get_outlet ()->now_mom () - music_start_mom ();
      stop_moment_ = start_moment_ + get_music ()->get_length ();

      end_idx_ = binsearch_scm_vector (event_vector_,
                                       stop_moment_.smobbed_copy (),
                                       &moment_less);
    }

  m += start_moment_;
  while (event_idx_ <= end_idx_)
    {
      Moment em = vector_moment (event_idx_);
      if (em > m)
        return;

      if (em == m)
        break;

      event_idx_++;
    }

  if (quote_ok ())
    {
      SCM entry = scm_c_vector_ref (event_vector_, event_idx_);
      Pitch *quote_pitch = unsmob<Pitch> (scm_cdar (entry));

      /*
        The pitch that sounds when written central C is played.
      */
      Pitch temp_pitch;
      Pitch *me_pitch = unsmob<Pitch> (get_music ()->get_property ("quoted-transposition"));
      if (!me_pitch)
        me_pitch = unsmob<Pitch> (get_outlet ()->get_property ("instrumentTransposition"));
      else
        {
          // We are not going to win a beauty contest with this one,
          // but it is slated for replacement and touches little code.
          // quoted-transposition currently has a different sign
          // convention than instrumentTransposition
          temp_pitch = me_pitch->negated ();
          me_pitch = &temp_pitch;
        }
      SCM cid = get_music ()->get_property ("quoted-context-id");
      bool is_cue = scm_is_string (cid) && (ly_scm2string (cid) == "cue");

      for (SCM s = scm_cdr (entry); scm_is_pair (s); s = scm_cdr (s))
        {
          SCM ev_acc = scm_car (s);

          Stream_event *ev = unsmob<Stream_event> (scm_car (ev_acc));
          if (!ev)
            programming_error ("no music found in quote");
          else if (accept_music_type (ev, is_cue))
            {
              /* create a transposed copy if necessary */
              if (quote_pitch || me_pitch)
                {
                  Pitch qp, mp;
                  if (quote_pitch)
                    qp = *quote_pitch;
                  if (me_pitch)
                    mp = *me_pitch;

                  Pitch diff = pitch_interval (mp, qp);
                  ev = ev->clone ();
                  ev->make_transposable ();
                  ev->transpose (diff);
                  transposed_musics_ = scm_cons (ev->unprotect (), transposed_musics_);
                }
              quote_outlet_.get_context ()->event_source ()->broadcast (ev);
            }
        }

      event_idx_++;
    }
}

IMPLEMENT_CTOR_CALLBACK (Quote_iterator);
