/*
  bar.hh -- part of GNU LilyPond

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef BAR_HH
#define BAR_HH
#include "item.hh"

/**
  A vertical bar.
 */
class Bar:public Item {
public:
  String type_str_;
  bool at_line_start_b_;

  
  VIRTUAL_COPY_CONS(Score_element);
  Bar ();

protected:
  virtual void do_pre_processing ();
  virtual Molecule* do_brew_molecule_p () const;
  virtual Real get_bar_size () const;
private:
  void do_print () const;
};
#endif // BAR_HH

