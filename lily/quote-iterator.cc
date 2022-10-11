/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <string>

class Quote_iterator final : public Music_wrapper_iterator
{
public:
  Quote_iterator () = default;
  Context_handle quote_handle_;

  // zero moment of this music in the timeline of the score; unknown until the
  // first call to process ()
  Moment zero_mom_ = -Moment::infinity ();
  SCM event_vector_ = SCM_EOL;
  vsize event_idx_ = 0; // left closed
  vsize end_idx_ = 0;   // right open
  bool first_time_ = true;

  SCM transposed_musics_ = SCM_EOL;

  DECLARE_SCHEME_CALLBACK (constructor, ());
  bool accept_music_type (Stream_event *, bool is_cue = true) const;

protected:
  void derived_mark () const override;
  void create_children () override;
  void create_contexts () override;
  Moment pending_moment () const override;
  void process (Moment) override;
  void do_quit () override;
};

void
Quote_iterator::do_quit ()
{
  Music_wrapper_iterator::do_quit ();
  quote_handle_ = nullptr;
}

bool
Quote_iterator::accept_music_type (Stream_event *ev, bool is_cue) const
{
  SCM accept = SCM_EOL;
  // Cue notes use the quotedCueEventTypes property, otherwise (and as fallback
  // for cue notes if quotedCueEventTypes is not set) use quotedEventTypes
  if (is_cue)
    accept = get_property (get_context (), "quotedCueEventTypes");
  if (scm_is_null (accept))
    accept = get_property (get_context (), "quotedEventTypes");

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

// lower bound: binary search returning the index of the first element that is
// not less than the key
vsize
binsearch_scm_vector (SCM vec, SCM key, bool is_less (SCM a, SCM b))
{
  vsize lo = 0;
  vsize hi = scm_c_vector_length (vec);

  while (lo < hi)
    {
      vsize cmp = (lo + hi) / 2;

      SCM when = scm_caar (scm_c_vector_ref (vec, cmp));
      if (is_less (when, key))
        lo = cmp + 1;
      else
        hi = cmp;
    }

  return lo;
}

void
Quote_iterator::create_children ()
{
  Music_wrapper_iterator::create_children ();

  event_vector_ = get_property (get_music (), "quoted-events");
}

void
Quote_iterator::create_contexts ()
{
  Music_wrapper_iterator::create_contexts ();

  Context *cue_context = 0;

  SCM name = get_property (get_music (), "quoted-context-type");
  if (scm_is_symbol (name))
    {
      SCM id = get_property (get_music (), "quoted-context-id");
      std::string c_id = robust_scm2string (id, "");
      cue_context
        = get_context ()->find_create_context (CENTER, name, c_id, SCM_EOL);
      if (!cue_context)
        {
          warning (_f ("cannot find or create context: %s",
                       Context::diagnostic_id (name, c_id).c_str ()));
        }
    }

  if (!cue_context)
    cue_context = get_context ()->get_default_interpreter ();
  quote_handle_ = cue_context;
}

Moment
Quote_iterator::pending_moment () const
{
  auto m = Music_wrapper_iterator::pending_moment ();

  if (event_idx_ < end_idx_)
    {
      SCM entry = scm_c_vector_ref (event_vector_, event_idx_);
      if (auto *const event_mom = unsmob<Moment> (scm_caar (entry)))
        {
          // If event_mom is not a moment, process () should issue a diagnostic
          // later, so just ignore it here.
          m = std::min (m, *event_mom - zero_mom_);
        }
    }

  return m;
}

void
Quote_iterator::process (Moment m)
{
  if (Music_wrapper_iterator::pending_moment () <= m)
    Music_wrapper_iterator::process (m);

  if (first_time_)
    {
      first_time_ = false;

      // start moment of this music in the timeline of the score
      const auto start_mom = get_context ()->now_mom ();

      zero_mom_ = start_mom - music_start_mom ();

      if (scm_is_vector (event_vector_))
        {
          // To quote grace notes, the user currently has to provide grace time
          // in the wrapped music.  It would be nicer to include all grace
          // notes leading into the quote automatically.  That likely requires
          // an infrastructure to precompute the start moment (at least the
          // main part) before the first call to pending_moment ().  It
          // possibly also requires improvements to handle music where the
          // grace part of the start moment is unknown prior to iteration.
          event_idx_ = binsearch_scm_vector (
            event_vector_, start_mom.smobbed_copy (), moment_less);

          // end moment of this music, excluding any grace notes leading to an
          // unquoted note
          const Moment end_mom (zero_mom_.main_part_
                                  + music_get_length ().main_part_,
                                -Rational::infinity ());

          end_idx_ = binsearch_scm_vector (
            event_vector_, end_mom.smobbed_copy (), moment_less);
        }
    }

  m = zero_mom_ + m;
  for (/**/; event_idx_ < end_idx_; ++event_idx_)
    {
      SCM entry = scm_c_vector_ref (event_vector_, event_idx_);

      if (auto *const event_mom = unsmob<Moment> (scm_caar (entry)))
        {
          if (*event_mom > m) // not time to process this entry yet
            return;
        }
      else
        {
          std::string s ("expected moment in event vector: ");
          s += ly_scm_write_string (scm_caar (entry));
          programming_error (s);
          continue;
        }

      Pitch *quote_pitch = unsmob<Pitch> (scm_cdar (entry));

      /*
        The pitch that sounds when written central C is played.
      */
      Pitch temp_pitch;
      Pitch *me_pitch
        = unsmob<Pitch> (get_property (get_music (), "quoted-transposition"));
      if (!me_pitch)
        me_pitch = unsmob<Pitch> (
          get_property (get_context (), "instrumentTransposition"));
      else
        {
          // We are not going to win a beauty contest with this one,
          // but it is slated for replacement and touches little code.
          // quoted-transposition currently has a different sign
          // convention than instrumentTransposition
          temp_pitch = me_pitch->negated ();
          me_pitch = &temp_pitch;
        }
      SCM cid = get_property (get_music (), "quoted-context-id");
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
                  transposed_musics_
                    = scm_cons (ev->unprotect (), transposed_musics_);
                }
              quote_handle_->event_source ()->broadcast (ev);
            }
        }
    }
}

IMPLEMENT_CTOR_CALLBACK (Quote_iterator);
