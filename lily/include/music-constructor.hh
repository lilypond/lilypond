/*   
  music-constructor.hh -- declare Music_constructor
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef MUSIC_CONSTRUCTOR_HH
#define MUSIC_CONSTRUCTOR_HH

#include "lily-proto.hh"
#include "string.hh"
#include "global-ctor.hh"

#define ADD_MUSIC(type) \
Music * _ ## type ## _ctor ()\
{\
  return new type;\
}\
static void  _ ## type ## _adder () {\
      add_music_ctor (#type, & _ ## type ## _ctor);\
}\
ADD_GLOBAL_CTOR (_ ## type ## _adder);

void add_music_ctor (String, Music* (*) ());
Music*make_music (String);





#endif /* MUSIC_CONSTRUCTOR_HH */

