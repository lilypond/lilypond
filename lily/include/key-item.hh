/*
  key-item.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef KEYITEM_HH
#define KEYITEM_HH

#include "item.hh"
#include "array.hh"


/**
  A group of  accidentals.

  Properties:

  c0-position -- integer indicating the position of central C?

  old-accidentals -- list of (pitch, accidental) pairs

  new-accidentals -- list of (pitch, accidental) pairs
 */
class Key_item :public  Item
{
  int calculate_position(SCM pair) const;

public:
  VIRTUAL_COPY_CONS(Score_element);
  Key_item (SCM);
  void add (int pitch, int acc);
  void add_old (int pitch, int acc);

protected:
  virtual Molecule do_brew_molecule() const;
};

#endif // KEYITEM_HH
