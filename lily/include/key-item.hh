/*
  key-item.hh -- part of GNU LilyPond

  (c) 1996--1998 Han-Wen Nienhuys
*/

#ifndef KEYITEM_HH
#define KEYITEM_HH

#include "item.hh"
#include "array.hh"


/// An item which places accidentals at the start of the line
struct Key_item : Item {
  Array<int> pitch_arr_;
  Array<int> acc_arr_;
  Array<int> old_pitch_arr_;
  Array<int> old_acc_arr_;

  // ugh.  Naming 
  int c_position;
  // see above.
  int c0_position;
  bool default_b_;
  bool multi_octave_b_;
    
  
  VIRTUAL_COPY_CONS(Score_element);

  Key_item ();
  void add (int pitch, int acc);
  void add_old (int pitch, int acc);
  void set_c_position (int);
  int Key_item::calculate_position(int p, int a) const;

protected:
  virtual void do_pre_processing();
  virtual Molecule* do_brew_molecule_p() const;
};

#endif // KEYITEM_HH
