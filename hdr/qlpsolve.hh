#include "qlp.hh"
#include "matrix.hh"


class Active_constraints {
    friend class Inactive_iter;
    

    Matrix A,H;
    svec<int> active;
    svec<int> inactive;		// actually this is a set, not an array.
    const Ineq_constrained_qp *opt;

public:
    String status()const;
    
    Vector vec(int k) const { return opt->cons[k]; }
    Real rhs(int k) const { return opt->consrhs[k]; }
    
    /// drop constraint
    void drop (int k);
    /** drop constraint k from the active set. k is the index of the
    constraint in #active#
    
    */
    
    ///     add constraint j
    void add(int j);
    /**
    add constraint j to the active set j is the index of the
    constraint in #inactive#   
    */

    /// exchange in and out.
    void exchange(int in, int out) { add(in); drop (out); }
    
    /// 
    Vector find_active_optimum(Vector g);

    /// get lagrange multipliers.
    Vector get_lagrange(Vector v);

    Active_constraints(Ineq_constrained_qp const *op);
    /** construct: no constraints active, n vars. Put the equalities
     into the constraints.  */

    /// check invariants
    void OK();
};

/**
    This class represents the set of active (binding) constraints
    which can be active while the QLP algorithm is in a feasible
    point. The active constraints are numbered.
    If the constraints are of the form

      A^T*x >= b

    then the binding constraints are those where the >= is equality.
    
  */

///
class Inactive_iter {
    int j;
    Active_constraints const* ac;
public:
    Inactive_iter(Active_constraints const &c) { ac=&c; j=0; }
    int idx() const { return j; }
    void operator ++(int) { j++; }
    int constraint_id() const { return ac->inactive[j]; }
    Vector vec() const { return ac->vec(constraint_id()); }
    Real rhs() const { return ac->rhs(constraint_id()); }
    bool ok() const { return j < ac->inactive.sz(); }
};
/**
    loop through the inactive constraints.
  */
