/*   
  compressed-music-iterator.hh -- declare Compressed_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef COMPRESSED_MUSIC_ITERATOR_HH
#define COMPRESSED_MUSIC_ITERATOR_HH

#include "music-wrapper-iterator.hh"

class Compressed_music_iterator : public Music_wrapper_iterator
{
public:
  Bracket_req* start_req_p_;
  Bracket_req* stop_req_p_;
  DECLARE_MY_RUNTIME_TYPEINFO;
  Compressed_music_iterator();
  ~Compressed_music_iterator ();
  virtual void do_process_and_next (Moment);
  virtual void construct_children ();
  Compressed_music *compressed_l ()const;
};


#endif /* COMPRESSED_MUSIC_ITERATOR_HH */

