/*
  linespace.hh -- declare Colinfo, Spacing_problem

  source file of the LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef LINESPACE_HH
#define LINESPACE_HH
#include "glob.hh"
#include "plist.hh"
#include "varray.hh"
#include "vector.hh"
#include "interval.hh"
#include "pointer.hh"

/// helper struct for #Spacing_problem#
struct Colinfo {
    PCol *pcol_l_;
    P<Real> fixpos_p_;
    Interval width;
    int rank_i_;
    /// did some tricks to make this column come out.
    bool ugh_b_;		
    /* *************** */
    Colinfo();
    Colinfo(PCol *,Real const *);

    void print() const;
    bool fixed() const { return fixpos_p_.get_C();}
    Real fixed_position()const { return *fixpos_p_; }
    Real minright() const { return width.right; }
    Real minleft() const { return -width.left; }
};


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
    Array<Idealspacing const *> ideals;
    Array<Colinfo> cols;
    Array<Colinfo> loose_col_arr_;
    
    /// mark column #i# as being loose.
    void loosen_column(int i);
    /// the index of #c# in #cols#
    int col_id(PCol const *c) const;

    /// generate an (nonoptimal) solution
    Vector find_initial_solution() const;

    /// check if problem is too tight
    bool check_feasible() const;

    /// does #this# contain the column #w#? 
    bool contains(PCol const *w);

    /// make the energy function
    void make_matrices(Matrix &quad, Vector &lin,Real&) const;

    /// generate the LP constraints
    void make_constraints(Mixed_qp& lp) const;


    void handle_loose_cols();
    void position_loose_cols(Vector &) const;
public:
    Array<PCol*> error_pcol_l_arr() const;

    /** solve the spacing problem
      
      @return the column positions, and the energy (last element)

      */
    Array<Real> solve() const;

    
    /**
       add a idealspacing to the problem.
      
    One pair of columns can have no, one or more idealspacings,
    since they can be "summed" if the columns to which #i# refers are
    not in this problem, the spacing is ignored.
    */
    void add_ideal(Idealspacing const *i);
    
    
    /** add a col to the problem. columns have to be added left to right. The column contains
      info on it's minimum width.
    */
    void add_column(PCol  *, bool fixed=false, Real fixpos=0.0);
 


    bool check_constraints(Vector v) const;

    Vector try_initial_solution() const;
    void OK() const;
    void print() const;
    void print_ideal(Idealspacing const *)const;
    void prepare();
};


#endif
