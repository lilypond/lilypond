/*   
  lyric-combine-music-iterator.cc --  implement Lyric_combine_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "translator-group.hh"
#include "lyric-combine-music-iterator.hh"
#include "lyric-combine-music.hh"
#include "musical-request.hh"
#include "note-head.hh"
#include "grob.hh"


/*
  Ugh, why static?
 */
Busy_playing_req *busy_req;
Melisma_req *melisma_start_req;
Melisma_req *melisma_stop_req;
Melisma_playing_req * melisma_playing_req;

Lyric_combine_music_iterator::Lyric_combine_music_iterator ()
{
  if (!busy_req)
    {
      busy_req = new Busy_playing_req;
      melisma_playing_req = new Melisma_playing_req;
      melisma_stop_req = new Melisma_req;
      melisma_start_req = new Melisma_req;      
    }
  melisma_start_req->set_span_dir (START);
  melisma_stop_req->set_span_dir (STOP);
  
  music_iter_p_ =0;
  lyric_iter_p_ =0;
}

Moment
Lyric_combine_music_iterator::pending_moment () const
{
  Moment musnext = music_iter_p_->pending_moment ();
  return musnext;
}

bool
Lyric_combine_music_iterator::ok () const
{
  return music_iter_p_->ok ();
}


void
Lyric_combine_music_iterator::construct_children ()
{
  Lyric_combine_music const * m = dynamic_cast<Lyric_combine_music const*> (music_l ());
  
  music_iter_p_ = get_iterator_p (m->music_l ());
  lyric_iter_p_ = get_iterator_p (m->lyrics_l ());
}

bool
Lyric_combine_music_iterator::get_busy_status () const
{
  /*
    We have to use both the request and the busyGrobs queue.  The
    busyGrobs queue doesn't contain any notes that have started this
    instant.  */
  if (try_music (busy_req))
    return true;
  
  Translator_group * tr = music_iter_p_->report_to_l ();

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

void
Lyric_combine_music_iterator::process (Moment m)
{
  Moment my_next = music_iter_p_->pending_moment ();
  if (my_next > m)
    return;
  
  music_iter_p_->process (m);

  if ( get_busy_status ())
    {
      bool melisma_b = try_music (melisma_playing_req);
      if (!melisma_b)
	{
	  if (lyric_iter_p_->ok ())
	    {
	      // FIXME
#if 0				// devise a new way for this
	      if (melisma_b && !melisma_started_b_)
		lyric_iter_p_->try_music (melisma_start_req);
	      else if (melisma_started_b_)
		lyric_iter_p_->try_music (melisma_stop_req);
#endif
	      
	      Moment m= lyric_iter_p_->pending_moment ();
	      lyric_iter_p_->process (m);
	    }
	}
    }
  
}

Lyric_combine_music_iterator::~Lyric_combine_music_iterator ()
{
  delete lyric_iter_p_;
  delete music_iter_p_;
}

Lyric_combine_music_iterator::Lyric_combine_music_iterator (Lyric_combine_music_iterator const & src)
    : Music_iterator (src)
{
  lyric_iter_p_ = src.lyric_iter_p_ ? src.lyric_iter_p_->clone () : 0;
  music_iter_p_ = src.music_iter_p_ ? src.music_iter_p_->clone () : 0;  
}

Music_iterator*
Lyric_combine_music_iterator::try_music_in_children (Music *m) const
{
  Music_iterator * i =  music_iter_p_->try_music (m);
  if (i)
    return i;
  else
    return lyric_iter_p_->try_music (m);
}


IMPLEMENT_CTOR_CALLBACK (Lyric_combine_music_iterator);
