/*
  new-lyric-combine-iterator.cc -- implement Lyric_combine_music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2004--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "dispatcher.hh"
#include "global-context.hh"
#include "grob.hh"
#include "input.hh"
#include "international.hh"
#include "listener.hh"
#include "music-iterator.hh"
#include "music.hh"

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
  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void do_quit ();
  virtual void process (Moment);
  virtual bool run_always ()const;
  virtual bool ok () const;
  virtual void derived_mark () const;
  virtual void derived_substitute (Context *, Context *);
  void set_music_context (Context *to);
private:
  bool start_new_syllable () const;
  Context *find_voice ();
  DECLARE_LISTENER (set_busy);
  DECLARE_LISTENER (check_new_context);

  bool music_found_;
  Context *lyrics_context_;
  Context *music_context_;
  SCM lyricsto_voice_name_;

  Moment busy_moment_;
  Moment pending_grace_moment_;
  
  Music_iterator *lyric_iter_;
};

Lyric_combine_music_iterator::Lyric_combine_music_iterator ()
{
  music_found_ = false;
  pending_grace_moment_.set_infinite (1);
  lyric_iter_ = 0;
  music_context_ = 0;
  lyrics_context_ = 0;
  busy_moment_.set_infinite (-1);
}


/*
  It's dubious whether we can ever make this fully work.  Due to
  associatedVoice switching, this routine may be triggered for 
  the wrong music_context_ 
 */
IMPLEMENT_LISTENER (Lyric_combine_music_iterator, set_busy)
void
Lyric_combine_music_iterator::set_busy (SCM se)
{
  Stream_event *e = unsmob_stream_event (se);

  if ((e->in_event_class ("note-event") || e->in_event_class ("cluster-note-event"))
      && music_context_)
    
    busy_moment_ = max (music_context_->now_mom (),
			busy_moment_);
  
}

void
Lyric_combine_music_iterator::set_music_context (Context *to)
{
  if (music_context_)
    {
      music_context_->event_source ()->
	remove_listener (GET_LISTENER (set_busy), ly_symbol2scm ("music-event"));
    }

  music_context_ = to;
  if (to)
    {
      to->event_source ()->add_listener (GET_LISTENER (set_busy),
					 ly_symbol2scm ("music-event"));
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
    set_music_context (t);
}

void
Lyric_combine_music_iterator::construct_children ()
{
  Music *m = unsmob_music (get_music ()->get_property ("element"));
  lyric_iter_ = unsmob_iterator (get_iterator (m));
  if (!lyric_iter_)
    return;
  lyrics_context_ = find_context_below (lyric_iter_->get_outlet (),
					ly_symbol2scm ("Lyrics"), "");

  if (!lyrics_context_)
    {
      m->origin ()->warning ("argument of \\lyricsto should contain Lyrics context");
    }
  
  lyricsto_voice_name_ = get_music ()->get_property ("associated-context");

  Context *voice = find_voice ();
  if (voice)
    set_music_context (voice);

  /*
    Wait for a Create_context event. If this isn't done, lyrics can be 
    delayed when voices are created implicitly.
  */
  Global_context *g = get_outlet ()->get_global_context ();
  g->events_below ()->add_listener (GET_LISTENER (check_new_context), ly_symbol2scm ("CreateContext"));

  /*
    We do not create a Lyrics context, because the user might
    create one with a different name, and then we will not find that
    one.
  */
}

IMPLEMENT_LISTENER (Lyric_combine_music_iterator, check_new_context)
void
Lyric_combine_music_iterator::check_new_context (SCM sev)
{
  if (!ok ())
    return ;
  
  // TODO: Check first if type=Voice and if id matches
  Stream_event * ev = unsmob_stream_event (sev);
  if (ev->get_property ("type") != ly_symbol2scm ("Voice"))
    return ;
  
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

  if (scm_is_string (running))
    voice_name = running;

  if (scm_is_string (voice_name)
      && (!music_context_ || ly_scm2string (voice_name) != music_context_->id_string ()))
    {
      Context *t = get_outlet ();
      while (t && t->get_parent_context ())
	t = t->get_parent_context ();

      string name = ly_scm2string (voice_name);
      return find_context_below (t, ly_symbol2scm ("Voice"), name);
    }

  return 0;
}

void
Lyric_combine_music_iterator::process (Moment when)
{
  (void) when;
  
  /* see if associatedVoice has been changed */
  Context *new_voice = find_voice ();
  if (new_voice)
    set_music_context (new_voice);

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
      set_music_context (0);
    }


  if (music_context_
      && (start_new_syllable () ||
	  (busy_moment_ >= pending_grace_moment_))
      && lyric_iter_->ok ())
    {
      Moment now = music_context_->now_mom ();
      if (now.grace_part_)
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
  if (!music_found_)
    {
      SCM voice_name = get_music ()->get_property ("associated-context");

      string name;
      if (scm_is_string (voice_name))
	name = ly_scm2string (voice_name);

      get_music ()->origin ()->warning (_f ("cannot find Voice `%s'",
					    name.c_str ()) + "\n");
    }

  if (lyric_iter_)
    lyric_iter_->quit ();
}

IMPLEMENT_CTOR_CALLBACK (Lyric_combine_music_iterator);
