#include "debug.hh"
#include "qlp.hh"
#include "choleski.hh"
void
Mixed_qp::add_equality_cons(Vector v, double r)
{
    assert(false);
}
void
Mixed_qp::add_fixed_var(int i, Real r)
{
    
   eq_cons.add(i);
   eq_consrhs.add(r);
}
   void
Ineq_constrained_qp::add_inequality_cons(Vector c, double r)
{
    cons.add(c);
    consrhs.add(r);
}

Ineq_constrained_qp::Ineq_constrained_qp(int novars):
    quad(novars),
    lin(novars)
{
}

void
Ineq_constrained_qp::OK()const
{
    assert(cons.sz() == consrhs.sz());
    Matrix Qdif= quad - quad.transposed();
    assert(Qdif.norm() < EPS);
}
     

Real
Ineq_constrained_qp::eval (Vector v)
{
    return v * quad * v + lin * v ;
}
/*
    eliminate appropriate variables, until we have a Ineq_constrained_qp
    then solve that.

    PRE
    cons should be ascending
    */
Vector
Mixed_qp::solve(Vector start) const 
{

    Ineq_constrained_qp pure(*this);
    
    for  (int i= eq_cons.sz()-1; i>=0; i--) {
	pure.eliminate_var(eq_cons[i], eq_consrhs[i]);
	start.del(eq_cons[i]);
    }
    Vector sol = pure.solve(start);
    for (int i= 0; i < eq_cons.sz(); i++) {
	sol.insert( eq_consrhs[i],eq_cons[i]);
    }
    return sol;
}

/*
    assume x(idx) == value, and adjust constraints, lin and quad accordingly
    */
void
Ineq_constrained_qp::eliminate_var(int idx, Real value)
{
    Vector row(quad.row(idx));
    row*= value;

    quad.delete_row(idx);

    quad.delete_column(idx);

    lin.del(idx);
    row.del(idx);
    lin +=row ;

   for (int i=0; i < cons.sz(); i++) {
      consrhs[i] -= cons[i](idx) *value;
      cons[i].del(idx);
   }
}




Mixed_qp::Mixed_qp(int n)
    : Ineq_constrained_qp(n)
{
}

void
Mixed_qp::OK()const
{
    Ineq_constrained_qp::OK();
    assert(eq_consrhs.sz() == eq_cons.sz());
}
void
Ineq_constrained_qp::print() const
{

    mtor << "Quad " << quad;
    mtor << "lin " << lin <<"\n";
    for (int i=0; i < cons.sz(); i++) {
	mtor << "constraint["<<i<<"]: " << cons[i] << " >= " << consrhs[i];
	mtor << "\n";
    }
}
void
Mixed_qp::print() const
{
    Ineq_constrained_qp::print();
    for (int i=0; i < eq_cons.sz(); i++) {
	mtor << "eq cons "<<i<<": x["<<eq_cons[i]<<"] == " << eq_consrhs[i]<<"\n";
    }
}


void
Ineq_constrained_qp::assert_solution(Vector sol) const
{
    svec<int> binding;
    for (int i=0; i < cons.sz(); i++) {
	Real R=cons[i] * sol- consrhs[i];
	assert(R> -EPS);
	if (R < EPS)
	    binding.add(i);
    }
    // KKT check...
    // todo
}
