/*   
  output-property-music-iterator.hh -- declare Output_property_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef OUTPUT_PROPERTY_MUSIC_ITERATOR_HH
#define OUTPUT_PROPERTY_MUSIC_ITERATOR_HH

#include "simple-music-iterator.hh"

class Output_property_music_iterator : public Simple_music_iterator
{
public:  
  VIRTUAL_COPY_CONS (Music_iterator);
  static SCM constructor_cxx_function;
  /* construction */
protected:
  virtual void process (Moment);
};


#endif /* OUTPUT_PROPERTY_MUSIC_ITERATOR_HH */

