/*   
  music-constructor.cc --  implement Music_constructor
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2001--2002  Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include <map>			// UGH.
#include "music-constructor.hh"
typedef Music* (*Music_ctor) ();

static std::map<String,Music_ctor> *ctors_map_;

void
add_music_ctor (String s,  Music_ctor c)
{
  if (!ctors_map_)
    ctors_map_ = new std::map<String, Music_ctor>;
  
 (*ctors_map_)[s] = c;
}


Music_ctor
get_music_ctor (String s)
{
  if (ctors_map_->find (s) == ctors_map_->end ())
    return 0;

  return (* ctors_map_)[s];
}

Music* 
get_music (String s)
{
  return (*get_music_ctor (s)) () ;
}

