/*
  chord-tremolo-iterator.hh -- declare Chord_tremolo_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef CHORD_TREMOLO_ITERATOR_HH
#define CHORD_TREMOLO_ITERATOR_HH

#include "sequential-iterator.hh"

class Chord_tremolo_iterator : public Sequential_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  /* construction */
  DECLARE_CLASSNAME(Chord_tremolo_iterator);
  Chord_tremolo_iterator ();
protected:
  virtual SCM get_music_list () const;
private:
};

#endif /* CHORD_TREMOLO_ITERATOR_HH */

