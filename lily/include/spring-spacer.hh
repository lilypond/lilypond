/*
  spring-spacer.hh -- declare Spring_spacer

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SPRING_SPACER_HH
#define SPRING_SPACER_HH

#include "line-spacer.hh"
#include "plist.hh"
#include "col-info.hh"
#include "colhpos.hh"
#include "moment.hh"

/** 
  Determine positions of columns connected by springs and held apart by rods.
  
  
    Generate a spacing which
    \begin{itemize}
    \item
    Satisfies spacing constraints (notes can't be printed through each other)
    \item
    Looks good, ie tries to conform to  an ideal spacing as much as possible.
    \end{itemize}
  
    This is converted by regarding idealspacing as "springs" attached
    to columns. The equilibrium of one spring is the ideal
    distance. The columns have a size, this imposes "hard" constraints
    on the distances. This transforms the problem into a quadratic
    programming problem with linear constraints.

    The quality is given by the total potential energy in the
    springs. The lower the energy, the better the configuration.

    TODO: make item widths work per Staff.

*/

class Spring_spacer : public Line_spacer {
  friend class Durations_iter;
    
  Pointer_list<Idealspacing *> ideal_p_list_;
  Array<Colinfo> cols;
  Array<Colinfo> loose_col_arr_;
    
  /// mark column #i# as being loose.
  void loosen_column (int i);
  /// the index of #c# in #cols#
  int col_id (Paper_column const *c) const;

  /// generate an (nonoptimal) solution
  Vector find_initial_solution() const;

  /// does #this# contain the column #w#? 
  bool contains (Paper_column const *w);

  /// make the energy function
  void make_matrices (Matrix &quad, Vector &lin,Real&) const;
  void get_ruling_durations(Array<Moment>&, Array<Moment>&);

  /// generate the LP constraints
  void make_constraints (Mixed_qp& lp) const;


  void handle_loose_cols();
  void position_loose_cols (Vector &) const;
  Vector try_initial_solution() const;
  void calc_idealspacing();
  void set_fixed_cols (Mixed_qp&) const;

  Score_column* scol_l (int);
  void connect (int i,int j, Real,Real);
  Line_of_cols error_pcol_l_arr() const;
  Real calculate_energy_f (Vector) const;
public:
  static Line_spacer *constructor();

  virtual void solve (Col_hpositions*) const;
  virtual void lower_bound_solution (Col_hpositions*) const;
  virtual void add_column (Paper_column  *, bool fixed=false, Real fixpos=0.0);
 

  virtual Vector default_solution() const;
  virtual bool check_constraints (Vector v) const;
  virtual void OK() const;
  virtual void print() const;
  virtual void prepare();
  virtual ~Spring_spacer(){}
};

#endif // SPRING_SPACER_HH
