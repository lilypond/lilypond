/*
  clef-item.hh -- declare Clef_item

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef CLEFITEM_HH
#define CLEFITEM_HH
#include "item.hh"

#include "direction.hh"
#include "pointer.hh"
#include "staff-symbol-referencer.hh"
/**
  Set a clef in a staff.

  properties:

  nondefault: not set because of existence of a bar

  octave_dir: should we print an octave symbol (8), and where? (up=1, down=-1)?

  change: is this a change clef (smaller size)?
  
 */
class Clef_item : public Item, public Staff_symbol_referencer {
protected:
  virtual void do_pre_processing();
  virtual Molecule* do_brew_molecule_p() const;
  virtual void do_add_processing ();
public:
  
  String symbol_;

  VIRTUAL_COPY_CONS(Score_element);
  Clef_item();
};

#endif // CLEFITEM_HH


