/*
  linespace.hh -- declare Colinfo, Spacing_problem

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#error

#ifndef LINESPACE_HH
#define LINESPACE_HH

#include "plist.hh"
#include "varray.hh"
#include "vector.hh"
#include "interval.hh"
#include "pointer.hh"


/** the problem, given by the columns (which include constraints) and
    intercolumn spacing. The problem is:

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

    TODO: make item widths work per pstaff.

*/
class Spacing_problem {
    PointerList<Idealspacing *> ideal_p_list_;
    Array<Colinfo> cols;
    Array<Colinfo> loose_col_arr_;
    
    /// mark column #i# as being loose.
    void loosen_column (int i);
    /// the index of #c# in #cols#
    int col_id (Paper_column const *c) const;

    /// generate an (nonoptimal) solution
    Vector find_initial_solution() const;

    /// check if problem is too tight
    bool check_feasible() const;

    /// does #this# contain the column #w#? 
    bool contains (Paper_column const *w);

    /// make the energy function
    void make_matrices (Matrix &quad, Vector &lin,Real&) const;

    /// generate the LP constraints
    void make_constraints (Mixed_qp& lp) const;


    void handle_loose_cols();
    void position_loose_cols (Vector &) const;
    void print_ideal (Idealspacing const *) const; 
    Vector try_initial_solution() const;
    void calcideal();

    Score_column* scol_l (int);
    void connect (int i,int j, Real,Real);
public:
    static Line_spacer *constructor() {
	return new Line_spacer;
    }
    Array<Paper_column*> error_pcol_l_arr() const;

    virtual   Array<Real> solve() const;
    virtual  void add_column (Paper_column  *, bool fixed=false, Real fixpos=0.0);
 

    virtual Vector default_solution() contains { 
	return try_initial_solution() ; 
    }
    virtual   bool check_constraints (Vector v) const;
    virtual    void OK() const;
    virtual    void print() const;
    virtual    void prepare();
};
