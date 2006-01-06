/*
  lyric-combine-music-iterator.cc -- implement Old_lyric_combine_music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context.hh"
#include "music.hh"
#include "note-head.hh"
#include "grob.hh"
#include "music-iterator.hh"

class Old_lyric_combine_music_iterator : public Music_iterator
{
public:
  Old_lyric_combine_music_iterator ();
  Old_lyric_combine_music_iterator (Old_lyric_combine_music_iterator const &src);
  DECLARE_SCHEME_CALLBACK (constructor, ());
protected:
  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void do_quit ();
  virtual void process (Moment);
  virtual Music_iterator *try_music_in_children (Music *) const;

  virtual bool ok () const;
  virtual void derived_mark () const;
  virtual void derived_substitute (Context *, Context *);
private:
  bool get_busy_status () const;
  bool melisma_busy ();
  Music *get_combine_lyrics () const;
  Music *get_combine_music () const;

  Music_iterator *music_iter_;
  Music_iterator *lyric_iter_;
};

bool
melisma_busy (Context *tr)
{
  SCM melisma_properties = tr->get_property ("melismaBusyProperties");
  bool busy = false;

  for (; scm_is_pair (melisma_properties);
       melisma_properties = scm_cdr (melisma_properties))
    busy = busy || to_boolean (tr->internal_get_property (scm_car (melisma_properties)));

  return busy;
}

/*
  Ugh, why static?
*/
Music *busy_req;
Music *melisma_playing_req;

Old_lyric_combine_music_iterator::Old_lyric_combine_music_iterator ()
{
  music_iter_ = 0;
  lyric_iter_ = 0;

  if (!busy_req)
    {
      busy_req
	= make_music_by_name (ly_symbol2scm ("BusyPlayingEvent"));
      melisma_playing_req
	= make_music_by_name (ly_symbol2scm ("MelismaPlayingEvent"));
    }
}

Moment
Old_lyric_combine_music_iterator::pending_moment () const
{
  Moment musnext = music_iter_->pending_moment ();
  return musnext;
}

bool
Old_lyric_combine_music_iterator::ok () const
{
  return music_iter_->ok ();
}

void
Old_lyric_combine_music_iterator::derived_mark ()const
{
  if (music_iter_)
    scm_gc_mark (music_iter_->self_scm ());
  if (lyric_iter_)
    scm_gc_mark (lyric_iter_->self_scm ());
}

void
Old_lyric_combine_music_iterator::derived_substitute (Context *f, Context *t)
{
  if (music_iter_)
    music_iter_->substitute_outlet (f, t);
  if (lyric_iter_)
    lyric_iter_->substitute_outlet (f, t);
}

Music *
Old_lyric_combine_music_iterator::get_combine_music () const
{
  SCM l = get_music ()->get_property ("elements");
  if (!scm_is_pair (l))
    return 0;
  return unsmob_music (scm_car (l));
}

Music *
Old_lyric_combine_music_iterator::get_combine_lyrics () const
{
  SCM l = get_music ()->get_property ("elements");
  if (!scm_is_pair (l))
    return 0;
  l = scm_cdr (l);
  if (!scm_is_pair (l))
    return 0;
  return unsmob_music (scm_car (l));
}

void
Old_lyric_combine_music_iterator::construct_children ()
{
  music_iter_ = unsmob_iterator (get_iterator (get_combine_music ()));
  lyric_iter_ = unsmob_iterator (get_iterator (get_combine_lyrics ()));
}

bool
Old_lyric_combine_music_iterator::get_busy_status () const
{
  /*
    We have to use both the event and the busyGrobs queue.  The
    busyGrobs queue doesn't contain any notes that have started this
    instant.  */
  if (try_music (busy_req))
    return true;

  Context *tr = music_iter_->get_outlet ();

  SCM grobs = tr->get_property ("busyGrobs");
  Moment now = tr->now_mom ();
  for (; scm_is_pair (grobs); grobs = scm_cdr (grobs))
    {
      SCM grob = scm_cdar (grobs);
      Moment end = *unsmob_moment (scm_caar (grobs));

      /*
	This is slightly ugh: we are now confunding the frontend
	(iterators) and the backend (note heads) */
      if (end > now
	  && Note_head::has_interface (unsmob_grob (grob)))
	return true;
    }

  return false;
}

bool
Old_lyric_combine_music_iterator::melisma_busy ()
{
  /* We cannot read the property, since music_iter_->get_outlet () might
     not be the context that sets the melisma properties, but rather a
     parent context.  */
  return music_iter_->try_music (melisma_playing_req);
}

void
Old_lyric_combine_music_iterator::process (Moment m)
{
  Moment my_next = music_iter_->pending_moment ();
  if (my_next > m)
    return;

  music_iter_->process (m);

  if (get_busy_status () && !melisma_busy () && lyric_iter_->ok ())
    {
      Moment m = lyric_iter_->pending_moment ();
      lyric_iter_->process (m);
    }
}

void
Old_lyric_combine_music_iterator::do_quit ()
{
  if (music_iter_)
    music_iter_->quit ();
  if (lyric_iter_)
    lyric_iter_->quit ();
}

Music_iterator *
Old_lyric_combine_music_iterator::try_music_in_children (Music *m) const
{
  Music_iterator *i = music_iter_->try_music (m);
  if (i)
    return i;
  else
    return lyric_iter_->try_music (m);
}

IMPLEMENT_CTOR_CALLBACK (Old_lyric_combine_music_iterator);
