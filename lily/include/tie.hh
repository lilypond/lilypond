/*
  tie.hh -- declare Tie

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TIE_HH
#define TIE_HH

#include "bow.hh"
#include "rod.hh"

/**
  Connect two noteheads.
  */
class Tie : public Bow
{
public:
  Tie ();
  void set_head (Direction, Note_head*head_l);
  VIRTUAL_COPY_CONS(Score_element);

  Note_head* head (Direction) const;
protected:
  virtual void do_add_processing ();
  virtual void do_post_processing ();
  virtual Direction get_default_dir() const;

  virtual Array<Rod> get_rods () const;

  Array<Offset> get_controls () const;
};

#endif // TIE_HH
