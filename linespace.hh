#ifndef PROBLEM_HH
#define PROBLEM_HH

#include "glob.hh"
#include "list.hh"
#include "vray.hh"
#include "pcol.hh"
#include "matrix.hh"

/// helper struct for #Spacing_problem#
struct Colinfo {
    const PCol *col;
    bool fixed;
    Real fixpos;
    Colinfo();
    void print() const;
    Real minright()const { return col->width().max; }
    Real minleft()const { return -col->width().min; }
};


/// spacing for one line.
class Spacing_problem {
    svec<const Idealspacing*> ideals;
    svec<Colinfo> cols;

    /// the index of #c# in #cols#
    int col_id(const PCol *c) const;

    /// generate an (nonoptimal) solution
    Vector find_initial_solution() const;

    /// check if problem is too tight
    bool check_feasible() const;
    /// does #this# contain the column #w#? 
    bool contains(const PCol *w);

    /// make the energy function
    void make_matrices(Matrix &quad, Vector &lin,Real&) const;

    /// generate the LP constraints
    void make_constraints(Mixed_qp& lp) const;

public:
    /// solve the spacing problem
    svec<Real> solve() const;
    /**
    return the column positions, and the energy (last element)
    */
    /// add a idealspacing to the problem.
    void add_ideal(const Idealspacing *i);
    
    /**
    One pair of columns can have no, one or more idealspacings,
    since they can be "summed" if the columns to which #i# refers are
    not in this problem, the spacing is ignored.
    */
    
    
    /// add a col to the problem
    void add_column(const PCol *, bool fixed=false, Real fixpos=0.0);
    /** columns have to be added left to right. The column contains
      info on it's minimum width.
    */


    bool check_constraints(Vector v) const;

    Vector try_initial_solution() const;
    void OK() const;
    void print() const;
    void print_ideal(const Idealspacing*)const;
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
*/
#endif
