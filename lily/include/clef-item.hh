/*
  clef-item.hh -- declare Clef_item

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef CLEFITEM_HH
#define CLEFITEM_HH
#include "item.hh"

#include "direction.hh"


/**
  Set a clef in a staff.

  properties:

  non-default -- not set because of existence of a bar?

  change -- is this a change clef (smaller size)?

  glyph -- a string determining what glyph is typeset
  
 */
class Clef_item : public Item
{
public:
  SCM member_before_line_breaking ();
  static SCM before_line_breaking (SCM);

  VIRTUAL_COPY_CONS(Score_element);
  Clef_item (SCM);
};

#endif // CLEFITEM_HH


