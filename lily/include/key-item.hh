/*
  key-item.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef KEYITEM_HH
#define KEYITEM_HH

#include "item.hh"
#include "array.hh"


/// An item which places accidentals at the start of the line
class Key_item :public  Item
{
  Array<int> pitch_arr_;
  Array<int> acc_arr_;
  Array<int> old_pitch_arr_;
  Array<int> old_acc_arr_;
  int c0_position_;


public:
  bool multi_octave_b_;  

  int get_c_position () const;
    
  
  VIRTUAL_COPY_CONS(Score_element);

  Key_item ();
  void add (int pitch, int acc);
  void add_old (int pitch, int acc);
  void set_c_position (int);
  int calculate_position(int p, int a) const;

protected:
  virtual Molecule do_brew_molecule() const;
};

#endif // KEYITEM_HH
