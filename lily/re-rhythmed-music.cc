/*   
  re-rhythmed-music.cc --  implement Re_rhythmed_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>

 */

#include "re-rhythmed-music.hh"
#include "music-iterator.hh"
#include "global-translator.hh"

// urg
class Foo_translator : public Global_translator 
{
};

Music_iterator*
Re_rhythmed_music::to_rhythm (Music_iterator* r)
{
  return r;
}

Re_rhythmed_music::Re_rhythmed_music (Music* m, Music* r)
  : Music_wrapper (m)
{
  Music_iterator* i = Music_iterator::static_get_iterator_p (r);
  Global_translator*t = new Foo_translator ();
  i->init_translator (r, t);
  i->construct_children ();
  element_l ()->to_rhythm (i);
}

void
Re_rhythmed_music::do_print () const
{
  Music_wrapper::do_print ();
}


