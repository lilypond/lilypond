/*
  bar.hh -- part of GNU LilyPond

  (c) 1996--1998 Han-Wen Nienhuys
*/

#ifndef BAR_HH
#define BAR_HH
#include "item.hh"
/**
  A vertical bar.
  
  TODO: connections with pre and postbreak
 */
class Bar:public Item {
public:
  String type_str_;

  
  VIRTUAL_COPY_CONS(Score_element);
  Bar ();

protected:
  virtual void do_pre_processing ();
  Molecule* brew_molecule_p () const;

private:
  void do_print () const;
};
#endif // BAR_HH

