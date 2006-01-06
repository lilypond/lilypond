/*
  new-lyric-combine-iterator.cc -- implement Lyric_combine_music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2004--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context.hh"
#include "music.hh"
#include "grob.hh"
#include "music-iterator.hh"
#include "input.hh"

class Lyric_combine_music_iterator : public Music_iterator
{
public:
  Lyric_combine_music_iterator ();
  Lyric_combine_music_iterator (Lyric_combine_music_iterator const &src);
  DECLARE_SCHEME_CALLBACK (constructor, ());
protected:
  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void do_quit ();
  virtual void process (Moment);
  virtual Music_iterator *try_music_in_children (Music *) const;
  virtual bool run_always ()const;
  virtual bool ok () const;
  virtual void derived_mark () const;
  virtual void derived_substitute (Context *, Context *);
private:
  bool start_new_syllable ();
  void find_voice ();

  bool music_found_;
  bool made_association_;
  Context *lyrics_context_;
  Context *music_context_;
  SCM lyricsto_voice_name_;

  Music_iterator *lyric_iter_;
};

/*
  Ugh, why static?
*/
static Music *busy_ev;
static Music *start_ev;
static Music *melisma_playing_ev;

Lyric_combine_music_iterator::Lyric_combine_music_iterator ()
{
  music_found_ = false;
  made_association_ = false;
  lyric_iter_ = 0;
  music_context_ = 0;
  lyrics_context_ = 0;

  /*
    Ugh. out of place here.
  */
  if (!busy_ev)
    {
      busy_ev
	= make_music_by_name (ly_symbol2scm ("BusyPlayingEvent"));
      start_ev
	= make_music_by_name (ly_symbol2scm ("StartPlayingEvent"));
      melisma_playing_ev
	= make_music_by_name (ly_symbol2scm ("MelismaPlayingEvent"));
    }
}

bool
Lyric_combine_music_iterator::start_new_syllable ()
{
  bool b = music_context_->try_music (busy_ev);

  if (!b)
    return false;

  if (!lyrics_context_)
    return false;

  if (!to_boolean (lyrics_context_->get_property ("ignoreMelismata")))
    {
      bool m = music_context_->try_music (melisma_playing_ev);
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
  return lyric_iter_ && lyric_iter_->ok ();
}

void
Lyric_combine_music_iterator::derived_mark ()const
{
  if (lyric_iter_)
    scm_gc_mark (lyric_iter_->self_scm ());
  if (lyrics_context_)
    scm_gc_mark (lyrics_context_->self_scm ());
  if (music_context_)
    scm_gc_mark (music_context_->self_scm ());
}

void
Lyric_combine_music_iterator::derived_substitute (Context *f, Context *t)
{
  if (lyric_iter_)
    lyric_iter_->substitute_outlet (f, t);
  if (lyrics_context_ && lyrics_context_ == f)
    lyrics_context_ = t;
  if (music_context_ && music_context_ == f)
    music_context_ = t;
}

void
Lyric_combine_music_iterator::construct_children ()
{
  Music *m = unsmob_music (get_music ()->get_property ("element"));
  lyric_iter_ = unsmob_iterator (get_iterator (m));

  lyricsto_voice_name_ = get_music ()->get_property ("associated-context");

  find_voice ();

  if (lyric_iter_)
    lyrics_context_ = find_context_below (lyric_iter_->get_outlet (),
					  ly_symbol2scm ("Lyrics"), "");

  /*
    We do not create a Lyrics context, because the user might
    create one with a different name, and then we will not find that
    one.
  */
}

void
Lyric_combine_music_iterator::find_voice ()
{
  SCM voice_name = lyricsto_voice_name_;
  SCM running = lyrics_context_
    ? lyrics_context_->get_property ("associatedVoice")
    : SCM_EOL;

  if (scm_is_string (running))
    voice_name = running;

  if (scm_is_string (voice_name)
      && (!music_context_ || ly_scm2string (voice_name) != music_context_->id_string ()))
    {
      /*
	(spaghettini).

	Need to set associatedVoiceContext again
      */
      if (music_context_)
	made_association_ = false;

      Context *t = get_outlet ();
      while (t && t->get_parent_context ())
	t = t->get_parent_context ();

      String name = ly_scm2string (voice_name);
      Context *voice = find_context_below (t, ly_symbol2scm ("Voice"), name);

      if (voice)
	music_context_ = voice;
    }

  if (lyrics_context_ && music_context_)
    {
      if (!made_association_)
	{
	  made_association_ = true;
	  lyrics_context_->set_property ("associatedVoiceContext",
					 music_context_->self_scm ());
	}
    }
}

void
Lyric_combine_music_iterator::process (Moment)
{
  find_voice ();
  if (!music_context_)
    return;

  if (!music_context_->get_parent_context ())
    {
      /*
	The melody has died.
	We die too.
      */
      if (lyrics_context_)
	lyrics_context_->unset_property (ly_symbol2scm ("associatedVoiceContext"));
      lyric_iter_ = 0;
      music_context_ = 0;
    }

  if (music_context_
      && start_new_syllable () && lyric_iter_->ok ())
    {
      Moment m = lyric_iter_->pending_moment ();
      lyric_iter_->process (m);

      music_found_ = true;
    }
}

void
Lyric_combine_music_iterator::do_quit ()
{
  if (!music_found_)
    {
      SCM voice_name = get_music ()->get_property ("associated-context");

      String name;
      if (scm_is_string (voice_name))
	name = ly_scm2string (voice_name);

      get_music ()->origin ()->warning (_f ("cannot find Voice `%s'",
					    name.to_str0 ()) + "\n");
    }

  if (lyric_iter_)
    lyric_iter_->quit ();
}

Music_iterator *
Lyric_combine_music_iterator::try_music_in_children (Music *m) const
{
  return lyric_iter_->try_music (m);
}

IMPLEMENT_CTOR_CALLBACK (Lyric_combine_music_iterator);
