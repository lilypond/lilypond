/*
  staff-side.hh -- declare Staff_side

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef STAFF_SIDE_HH
#define STAFF_SIDE_HH

#include "score-element.hh"
#include "interval.hh"
#include "direction.hh"

/**

   A symbol which sits either below or above "something" (usually, a
   staff).

*/
class Staff_side : virtual Score_element
{
public:

  /**
    Vertical dir of symbol relative to staff. -1 = below staff?
    */
  Direction dir_;
  Axis axis_;
  Interval sym_int_;
    
  Real coordinate_offset_f_;

  /**
     Add extra vertical space to the support symbols.
   */
  Real padding_f_;

  Staff_side ();
  void add_support (Score_element*);
  
    
protected:
  virtual Interval symbol_height () const;
  Interval symbol_extent () const;
  virtual Real get_position_f () const;
  virtual void do_substitute_dependency (Score_element *, Score_element*);
  virtual void do_pre_processing ();
  virtual void do_post_processing ();
  Interval support_extent () const;

private:
  void do_side_processing ();
  Link_array<Score_element> support_l_arr_;
};

#endif // STAFF_SIDE_HH
