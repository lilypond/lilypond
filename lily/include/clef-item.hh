/*
  clef-item.hh -- declare Clef_item

  (c) 1996--1998 Han-Wen Nienhuys
*/

#ifndef CLEFITEM_HH
#define CLEFITEM_HH
#include "item.hh"
#include "text-def.hh"
#include "direction.hh"
#include "pointer.hh"

/**
  Set a clef in a staff.
 */
class Clef_item : public Item {
protected:
  virtual void do_pre_processing();
  virtual Molecule* do_brew_molecule_p() const;
  virtual void do_add_processing ();
public:
    
  String symbol_;
  int y_position_i_;

  /// is this a change clef (smaller size)?
  bool change_b_;
    
  /// set because of existence of a bar
  bool default_b_;

  /// should we print an octave symbol (8), and where? (up=1, down=-1)?
  Direction octave_dir_;

  VIRTUAL_COPY_CONS(Score_element);
  Clef_item();
};

#endif // CLEFITEM_HH


