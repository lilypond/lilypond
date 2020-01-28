/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "grob.hh"
#include "input.hh"
#include "international.hh"
#include "listener.hh"
#include "music-iterator.hh"
#include "music.hh"

using std::string;

/*
  This iterator is hairy.  It tracks both lyric and melody contexts,
  and has a complicated communication route, reading/writing
  properties in both.

  In the future, this should rather be done with

     \interpretAsMelodyFor { MUSIC } { LYRICS LYRICS LYRICS }

  This can run an interpret step on MUSIC, generating a stream.  Then
  the stream can be perused at leisure to apply durations to all of
  the LYRICS.
*/

class Lyric_combine_music_iterator : public Music_iterator
{
public:
  Lyric_combine_music_iterator ();
  Lyric_combine_music_iterator (Lyric_combine_music_iterator const &src);
  DECLARE_SCHEME_CALLBACK (constructor, ());

protected:
  void construct_children () override;
  Moment pending_moment () const override;
  void do_quit () override;
  void process (Moment) override;
  bool run_always () const override;
  bool ok () const override;
  void derived_mark () const override;
  void derived_substitute (Context *, Context *) override;
  void set_music_context (Context *to);

private:
  bool start_new_syllable () const;
  Context *find_voice ();
  void set_busy (SCM);
  void check_new_context (SCM);

  bool music_found_;
  bool lyrics_found_;
  Context *lyrics_context_;
  Context *music_context_;
  SCM lyricsto_voice_name_;
  SCM lyricsto_voice_type_;

  Moment busy_moment_;
  Moment pending_grace_moment_;

  Music_iterator *lyric_iter_;
};

Lyric_combine_music_iterator::Lyric_combine_music_iterator ()
{
  music_found_ = false;
  lyrics_found_ = false;
  pending_grace_moment_.set_infinite (1);
  lyric_iter_ = 0;
  music_context_ = 0;
  lyrics_context_ = 0;
  busy_moment_.set_infinite (-1);
  lyricsto_voice_name_ = SCM_UNDEFINED;
  lyricsto_voice_type_ = SCM_UNDEFINED;
}

/*
  It's dubious whether we can ever make this fully work.  Due to
  associatedVoice switching, this routine may be triggered for
  the wrong music_context_
 */
void
Lyric_combine_music_iterator::set_busy (SCM se)
{
  Stream_event *e = unsmob<Stream_event> (se);

  if ((e->in_event_class ("note-event")
       || e->in_event_class ("cluster-note-event"))
      && music_context_)

    busy_moment_ = std::max (music_context_->now_mom (), busy_moment_);
}

void
Lyric_combine_music_iterator::set_music_context (Context *to)
{
  if (music_context_)
    {
      music_context_->events_below ()->remove_listener (
          GET_LISTENER (Lyric_combine_music_iterator, set_busy),
          ly_symbol2scm ("rhythmic-event"));
    }

  music_context_ = to;
  if (to)
    {
      to->events_below ()->add_listener (
          GET_LISTENER (Lyric_combine_music_iterator, set_busy),
          ly_symbol2scm ("rhythmic-event"));
    }
}

bool
Lyric_combine_music_iterator::start_new_syllable () const
{
  if (busy_moment_ < music_context_->now_mom ())
    return false;

  if (!lyrics_context_)
    return false;

  if (!to_boolean (lyrics_context_->get_property ("ignoreMelismata")))
    {
      bool m = melisma_busy (music_context_);
      if (m)
        return false;
    }

  return true;
}

Moment
Lyric_combine_music_iterator::pending_moment () const
{
  Moment m;

  m.set_infinite (1);

  return m;
}

bool
Lyric_combine_music_iterator::run_always () const
{
  return true;
}

bool
Lyric_combine_music_iterator::ok () const
{
  return lyric_iter_ && lyric_iter_->ok ()
         && !(music_context_ && music_context_->is_removable ());
}

void
Lyric_combine_music_iterator::derived_mark () const
{
  if (lyric_iter_)
    scm_gc_mark (lyric_iter_->self_scm ());
  if (lyrics_context_)
    scm_gc_mark (lyrics_context_->self_scm ());
  if (music_context_)
    scm_gc_mark (music_context_->self_scm ());
  scm_gc_mark (lyricsto_voice_name_);
  scm_gc_mark (lyricsto_voice_type_);
}

void
Lyric_combine_music_iterator::derived_substitute (Context *f, Context *t)
{
  if (lyric_iter_)
    lyric_iter_->substitute_outlet (f, t);
  if (lyrics_context_ && lyrics_context_ == f)
    lyrics_context_ = t;
  if (music_context_ && music_context_ == f)
    set_music_context (t);
}

void
Lyric_combine_music_iterator::construct_children ()
{
  Music *m = unsmob<Music> (get_music ()->get_property ("element"));
  lyric_iter_ = unsmob<Music_iterator> (get_iterator (m));
  if (!lyric_iter_)
    return;
  lyrics_context_ = find_context_below (lyric_iter_->get_outlet (),
                                        ly_symbol2scm ("Lyrics"), "");

  if (!lyrics_context_)
    {
      m->origin ()->warning (
          _ ("argument of \\lyricsto should contain Lyrics context"));
    }

  lyricsto_voice_name_ = get_music ()->get_property ("associated-context");
  lyricsto_voice_type_ = get_music ()->get_property ("associated-context-type");
  if (!scm_is_symbol (lyricsto_voice_type_))
    lyricsto_voice_type_ = ly_symbol2scm ("Voice");

  Context *voice = find_voice ();
  if (voice)
    set_music_context (voice);

  /*
    Wait for a Create_context event. If this isn't done, lyrics can be
    delayed when voices are created implicitly.
  */
  Context *g = find_top_context (get_outlet ());
  g->events_below ()->add_listener (
      GET_LISTENER (Lyric_combine_music_iterator, check_new_context),
      ly_symbol2scm ("CreateContext"));

  /*
    We do not create a Lyrics context, because the user might
    create one with a different name, and then we will not find that
    one.
  */
}

void Lyric_combine_music_iterator::check_new_context (SCM /*sev*/)
{
  if (!ok ())
    return;

  // Search for a possible candidate voice to attach the lyrics to. If none
  // is found, we'll try next time again.
  Context *voice = find_voice ();
  if (voice)
    {
      set_music_context (voice);
    }
}

/*
  Look for a suitable voice to align lyrics to.

  Returns 0 if nothing should change; i.e., if we already listen to the
  right voice, or if we don't yet listen to a voice but no appropriate
  voice could be found.
*/
Context *
Lyric_combine_music_iterator::find_voice ()
{
  SCM voice_name = lyricsto_voice_name_;
  SCM running = lyrics_context_
                    ? lyrics_context_->get_property ("associatedVoice")
                    : SCM_EOL;
  SCM voice_type = lyricsto_voice_type_;
  if (scm_is_string (running))
    {
      voice_name = running;
      voice_type = lyrics_context_->get_property ("associatedVoiceType");
    }

  if (scm_is_string (voice_name)
      && (!music_context_
          || ly_scm2string (voice_name) != music_context_->id_string ())
      && scm_is_symbol (voice_type))
    {
      return find_context_below (find_top_context (get_outlet ()), voice_type,
                                 ly_scm2string (voice_name));
    }

  return 0;
}

void Lyric_combine_music_iterator::process (Moment /* when */)
{
  /* see if associatedVoice has been changed */
  Context *new_voice = find_voice ();
  if (new_voice)
    set_music_context (new_voice);

  lyrics_found_ = true;
  if (!music_context_)
    return;

  if (!music_context_->get_parent_context ())
    {
      /*
        The melody has died.
        We die too.
      */
      if (lyrics_context_)
        lyrics_context_->unset_property (
            ly_symbol2scm ("associatedVoiceContext"));
      lyric_iter_ = 0;
      set_music_context (0);
    }

  if (music_context_
      && (start_new_syllable () || (busy_moment_ >= pending_grace_moment_))
      && lyric_iter_->ok ())
    {
      Moment now = music_context_->now_mom ();
      if (now.grace_part_
          && !to_boolean (lyrics_context_->get_property ("includeGraceNotes")))
        {
          pending_grace_moment_ = now;
          pending_grace_moment_.grace_part_ = Rational (0);
          return;
        }
      else
        {
          pending_grace_moment_.set_infinite (1);
        }

      Moment m = lyric_iter_->pending_moment ();
      lyrics_context_->set_property (ly_symbol2scm ("associatedVoiceContext"),
                                     music_context_->self_scm ());
      lyric_iter_->process (m);

      music_found_ = true;
    }

  new_voice = find_voice ();
  if (new_voice)
    set_music_context (new_voice);
}

void
Lyric_combine_music_iterator::do_quit ()
{
  /* Don't print a warning for empty lyrics (in which case we don't try
     to find the proper voice, so it will not be found) */
  if (lyrics_found_ && !music_found_)
    {
      Music *m = get_music ();

      // ugh: defaults are repeated elsewhere
      SCM voice_type = m->get_property ("associated-context-type");
      if (!scm_is_symbol (voice_type))
        voice_type = ly_symbol2scm ("Voice");

      string id
          = robust_scm2string (m->get_property ("associated-context"), "");

      Input *origin = m->origin ();
      origin->warning (_f ("cannot find context: %s",
                           Context::diagnostic_id (voice_type, id).c_str ()));
    }

  if (lyric_iter_)
    lyric_iter_->quit ();
}

IMPLEMENT_CTOR_CALLBACK (Lyric_combine_music_iterator);
