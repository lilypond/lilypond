/*
  span-bar.hh -- declare Span_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SPAN_BAR_HH
#define SPAN_BAR_HH

#include "bar.hh"

class Span_bar : public Bar
{
  Link_array<Score_element> spanning_l_arr_;
  Interval get_spanned_interval () const;
public:
  Real extra_x_off_;
  bool no_width_b_;
  Span_bar();
    
  VIRTUAL_COPY_CONS(Score_element);
  void add_bar (Bar*);
  void set_align (Align_element *);

protected:
  void evaluate_empty ();
  virtual Interval do_width() const;
  virtual void do_pre_processing();
  virtual void do_post_processing();
  virtual Interval do_height () const;
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
  virtual Molecule * do_brew_molecule_p() const;
};

#endif // SPAN_BAR_HH
