/*
  staff-side.hh -- declare Staff_side

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STAFF_SIDE_HH
#define STAFF_SIDE_HH

#include "score-elem.hh"
#include "interval.hh"
#include "direction.hh"

/** A symbol which sits along a staff.

  */
class Staff_side : virtual Score_elem
{
public:

  /**
    Vertical dir of symbol relative to staff. -1 = below staff?
    */
  Direction dir_;
  Interval sym_int_;
    
  Real y_;


  void set_staffsym (Staff_symbol * );
  Staff_side ();
  void add_support (Score_elem*);
  DECLARE_MY_RUNTIME_TYPEINFO;
    
protected:
  virtual Interval symbol_height () const;
  virtual Real get_position_f () const;
  virtual void do_substitute_dependency (Score_elem *, Score_elem*);
  virtual void do_post_processing ();
  Interval support_height () const;

private:
  Link_array<Score_elem> support_l_arr_;
//  Interval support_height () const;
};
#endif // STAFF_SIDE_HH
