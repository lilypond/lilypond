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
  
  bool same_pitch_b_;
  Drul_array<Note_head *> head_l_drul_;

protected:
  virtual void do_add_processing ();
  virtual void do_post_processing ();
  virtual void set_default_dir();
  virtual void do_substitute_dependency (Score_element*,Score_element*);
  virtual Interval do_width () const;
  Array<Rod> get_rods () const;

  Array<Offset> get_controls () const;
};

#endif // TIE_HH
