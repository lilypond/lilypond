/*   
  lyric-combine-music-iterator.cc --  implement Lyric_combine_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "translator-group.hh"
#include "lyric-combine-music.hh"
#include "event.hh"
#include "note-head.hh"
#include "grob.hh"
#include "music-iterator.hh"

class Lyric_combine_music_iterator : public Music_iterator
{
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  Lyric_combine_music_iterator ();
  Lyric_combine_music_iterator (Lyric_combine_music_iterator const&src);
  DECLARE_SCHEME_CALLBACK(constructor, ());
protected:
  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void do_quit(); 
  virtual void process (Moment);
  virtual Music_iterator *try_music_in_children (Music *) const;

  virtual bool ok () const;
  virtual void derived_mark () const;
private:
  bool get_busy_status ()const ;
  bool  melisma_busy (); 
  

  Music_iterator * music_iter_;
  Music_iterator * lyric_iter_;
};



/*
  Ugh, why static?
 */
Music *busy_req;
Music *melisma_playing_req;

Lyric_combine_music_iterator::Lyric_combine_music_iterator ()
{
  music_iter_ =0;
  lyric_iter_ =0;

  if (!busy_req)
    {
      busy_req
	= make_music_by_name (ly_symbol2scm ("BusyPlayingEvent"));
      melisma_playing_req
	= make_music_by_name (ly_symbol2scm ("MelismaPlayingEvent"));
    }
}

Moment
Lyric_combine_music_iterator::pending_moment () const
{
  Moment musnext = music_iter_->pending_moment ();
  return musnext;
}

bool
Lyric_combine_music_iterator::ok () const
{
  return music_iter_->ok ();
}

void
Lyric_combine_music_iterator::derived_mark()const
{
  if (music_iter_)
    scm_gc_mark (music_iter_->self_scm());
  if (lyric_iter_)
    scm_gc_mark (lyric_iter_->self_scm());
}

void
Lyric_combine_music_iterator::construct_children ()
{
  Lyric_combine_music const * m = dynamic_cast<Lyric_combine_music const*> (get_music ());
  
  music_iter_ = unsmob_iterator (get_iterator (m->get_music ()));
  lyric_iter_ = unsmob_iterator (get_iterator (m->get_lyrics ()));
}

bool
Lyric_combine_music_iterator::get_busy_status () const
{
  /*
    We have to use both the event and the busyGrobs queue.  The
    busyGrobs queue doesn't contain any notes that have started this
    instant.  */
  if (try_music (busy_req))
    return true;
  
  Translator_group * tr = music_iter_->report_to ();

  SCM grobs = tr->get_property ("busyGrobs");
  Moment now = tr->now_mom();
  for (; gh_pair_p (grobs); grobs = gh_cdr (grobs))
    {
      SCM grob = gh_cdar (grobs);
      Moment end  =*unsmob_moment (gh_caar (grobs));

      
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
Lyric_combine_music_iterator::melisma_busy ()
{
  /*
    We can not read the property, since music_iter_->report_to() might
    not be the context that sets the melisma properties, but rather a
    parent context.
   */
  return music_iter_->try_music (melisma_playing_req);
}

void
Lyric_combine_music_iterator::process (Moment m)
{
  Moment my_next = music_iter_->pending_moment ();
  if (my_next > m)
    return;
  
  music_iter_->process (m);

  if (get_busy_status () && !melisma_busy () && lyric_iter_->ok ())
    {
      Moment m= lyric_iter_->pending_moment ();
      lyric_iter_->process (m);
    }
}

void
Lyric_combine_music_iterator::do_quit ()
{
  if (music_iter_)
    music_iter_->quit();
  if (lyric_iter_)
    lyric_iter_->quit();
  
}
Lyric_combine_music_iterator::Lyric_combine_music_iterator (Lyric_combine_music_iterator const & src)
    : Music_iterator (src)
{
  lyric_iter_ = 0;
  music_iter_ = 0;

  if (src.lyric_iter_)
    lyric_iter_ =  src.lyric_iter_->clone ();
  if (src.music_iter_)
    music_iter_ =  src.music_iter_->clone ();

  if (lyric_iter_)
    scm_gc_unprotect_object (lyric_iter_->self_scm());
  if (music_iter_)
    scm_gc_unprotect_object (music_iter_->self_scm());
}

Music_iterator*
Lyric_combine_music_iterator::try_music_in_children (Music *m) const
{
  Music_iterator * i =  music_iter_->try_music (m);
  if (i)
    return i;
  else
    return lyric_iter_->try_music (m);
}


IMPLEMENT_CTOR_CALLBACK (Lyric_combine_music_iterator);
