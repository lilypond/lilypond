/*
  line-spacer.hh -- declare Line_spacer

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef LINE_SPACER_HH
#define LINE_SPACER_HH
#include "lily-proto.hh"
#include "array.hh"
#include "interval.hh"

/**
  abstract interface to Line spacing.

  TODO
  add estimate of "force" or energy.
 */

class Line_spacer 
{
    
public:
  Real indent_f_;
  Real line_len_f_;
  Real default_space_f_;

  Line_spacer();
    
  /** solve the spacing problem
   */
  virtual void solve (Column_x_positions *) const=0;

  /**
    Approximate the spacing problem:
    return a lower bound on the energy
    */
  virtual void lower_bound_solution (Column_x_positions *) const=0;
   

  /**
     Define the problem. LINELEN < 0 signifies natural width spacing.
   */

  virtual void add_columns (Link_array<Paper_column>)=0;
  virtual void OK() const{}
  virtual void print() const{}
    
  /**
    Call after construction before solving
    */
  virtual void prepare(){}
  virtual ~Line_spacer ();
};

#endif
