/*   
  lyric-combine-music-iterator.cc --  implement Lyric_combine_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "translator-group.hh"
#include "lyric-combine-music-iterator.hh"
#include "lyric-combine-music.hh"
#include "musical-request.hh"

Busy_playing_req busy_req;
Melisma_req melisma_start_req;
Melisma_req melisma_stop_req;
Melisma_playing_req melisma_playing_req;


Lyric_combine_music_iterator::Lyric_combine_music_iterator ()
{
  melisma_start_req.span_dir_ = START;
  melisma_stop_req.span_dir_ = STOP;
  
  music_iter_p_ =0;
  lyric_iter_p_ =0;
}

Moment
Lyric_combine_music_iterator::next_moment () const
{
  Moment musnext = music_iter_p_->next_moment ();
  return musnext;
}

bool
Lyric_combine_music_iterator::ok () const
{
  return music_iter_p_->ok ();
}

void
Lyric_combine_music_iterator::do_print () const
{
  music_iter_p_->print ();
  lyric_iter_p_->print ();
}

void
Lyric_combine_music_iterator::construct_children ()
{
  Lyric_combine_music const * m = dynamic_cast<Lyric_combine_music const*> (music_l_);
  
  music_iter_p_ = get_iterator_p (m->music_l ());
  lyric_iter_p_ = get_iterator_p (m->lyrics_l ());
}

void
Lyric_combine_music_iterator::do_process_and_next (Moment m)
{
  Moment my_next = music_iter_p_->next_moment ();
  if (my_next > m)
    return;
  
  music_iter_p_->process_and_next (m);

  bool busy = try_music (&busy_req);
  if (busy)
    {
      bool melisma_b = try_music (&melisma_playing_req);
      if (!melisma_b)
	{
	  if (lyric_iter_p_->ok ())
	    {
#if 0				// devise a new way for this
	      if (melisma_b && !melisma_started_b_)
		lyric_iter_p_->try_music (&melisma_start_req);
	      else if (melisma_started_b_)
		lyric_iter_p_->try_music (&melisma_stop_req);
#endif
	      
	      Moment m= lyric_iter_p_->next_moment ();
	      lyric_iter_p_->process_and_next (m);
	    }
	}
    }
  

  
  Music_iterator::do_process_and_next (m);
}

Lyric_combine_music_iterator::~Lyric_combine_music_iterator ()
{
  delete lyric_iter_p_;
  delete music_iter_p_;
}

Music_iterator*
Lyric_combine_music_iterator::try_music_in_children (Music const *m) const
{
  Music_iterator * i =  music_iter_p_->try_music (m);
  if (i)
    return i;
  else
    return lyric_iter_p_->try_music (m);
}

