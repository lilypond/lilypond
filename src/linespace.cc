#include <math.h>
#include "linespace.hh"
#include "debug.hh"
#include "qlp.hh"
#include "unionfind.hh"

const Real COLFUDGE=1e-3;
//#define COLFUDGE 1e-3
bool
Spacing_problem::contains(const PCol *w)
{
    for (int i=0; i< cols.sz(); i++)
	if (cols[i].pcol_ == w)
	    return true;
    return false;
}

int 
Spacing_problem::col_id(const PCol *w)const
{
    for (int i=0; i< cols.sz(); i++)
	if (cols[i].pcol_ == w)
	    return i;
    assert(false);
}

void
Spacing_problem::OK() const
{
#ifndef NDEBUG
    Union_find connected(cols.sz());
    svec<int> fixed;
    for (int i=0; i < ideals.sz(); i++) {
	assert(ideals[i]->hooke > 0);
	int l = col_id(ideals[i]->left);
	int r = col_id(ideals[i]->right);
	connected.connect(l,r);		
    }
    for (int i = 0; i < cols.sz(); i++)
	if (cols[i].fixed)
	    fixed.add(i);
    for (int i = 0; i < cols.sz(); i++) {
	bool c=false;
	for (int j =0; j<fixed.sz(); j++)
	    c |=  connected.equiv(j,i);
	assert(c);
    }
#endif    
}

bool
Spacing_problem::check_constraints(Vector v) const 
{
    int dim=v.dim();
    // mtor << "checking solution " << v << '\n';
    for (int i=0; i < dim; i++) {

	if (cols[i].fixed&& ABS(cols[i].fixpos - v(i)) > COLFUDGE) {
	    return false;
	}
	if (!i) 
	    continue;
	
	Real mindist=cols[i-1].minright()
	    +cols[i].minleft();

	// ugh... compares
	Real dif =v(i) - v(i-1)- mindist;
	bool b = (dif > - COLFUDGE);
	

#if 1
	if (!b)
	    return false;

#else
	mtor << "dif= "<<dif<<" fudge= " << COLFUDGE<< " dif >fudge= "<<
	    b << "\n";
	
	/* fucks up for unknown reasons */
	if (dif  < -COLFUDGE)
	    return false;
#endif

    }
    return true;
}

bool
Spacing_problem::check_feasible() const
{
    Vector sol(try_initial_solution());
    return check_constraints(sol);     
}

// generate a solution which obeys the min distances and fixed positions
Vector
Spacing_problem::try_initial_solution() const
{
    int dim=cols.sz();
    Vector initsol(dim);
    for (int i=0; i < dim; i++) {
	if (cols[i].fixed) {
	    initsol(i)=cols[i].fixpos;	    
	} else {
	    Real mindist=cols[i-1].minright()
		+cols[i].minleft();
	    assert(mindist >= 0.0);
	    initsol(i)=initsol(i-1)+mindist;

	    //nog niet 
	    //if (i>0)
	    //  assert(initsol(i) > initsol(i-1));
	}	
    }

    return initsol;
}
Vector
Spacing_problem::find_initial_solution() const
{
    Vector v(try_initial_solution());     
    assert(check_constraints(v));
    return v;
}
// generate the matrices
void
Spacing_problem::make_matrices(Matrix &quad, Vector &lin, Real &c) const
{
    quad.fill(0);
    lin.fill(0);
    for (int j=0; j < ideals.sz(); j++){
	Idealspacing const*i=ideals[j];
	int l = col_id(i->left);
	int r = col_id(i->right);

	quad(r,r) += i->hooke;
	quad(r,l) -= i->hooke;
	quad(l,r) -= i->hooke;
	quad(l,l) += i->hooke;

	lin(r) -= i->space*i->hooke;
	lin(l) += i->space*i->hooke;

	c += sqr(i->space);
    }
}

// put the constraints into the LP problem
void
Spacing_problem::make_constraints(Mixed_qp& lp) const
{    
    int dim=cols.sz();
    for (int j=0; j < dim; j++) {
	Colinfo *c=&(cols[j]);
	if (c->fixed) {
	    lp.add_fixed_var(j,c->fixpos);	    
	}
	if (j > 0){
	    Vector c1(dim);
	    
	       
	    c1(j)=1.0 ;
	    c1(j-1)=-1.0 ;
	    lp.add_inequality_cons(c1, cols[j-1].minright() +
				   cols[j].minleft());
	}
    }
}

svec<Real>
Spacing_problem::solve() const
{
    print();
    OK();
    assert(check_feasible());

    
    /* optimalisatiefunctie */        
    Mixed_qp lp(cols.sz());
    make_matrices(lp.quad,lp.lin, lp.const_term);
    make_constraints(lp);    
    Vector start=find_initial_solution();    
    Vector sol(lp.solve(start));
    if (!check_constraints(sol)) {
	WARN << "solution doesn't satisfy constraints.\n" ;
    }
	

    svec<Real> posns(sol);
    posns.add(lp.eval(sol));
    return posns;
}

/*
    add one column to the problem.
*/    
void
Spacing_problem::add_column(const PCol *col, bool fixed, Real fixpos)
{
    Colinfo c;
    c.fixed=fixed;
    c.fixpos=fixpos;
    assert(col);
    c.pcol_=col;
    cols.add(c);
}

void
Spacing_problem::add_ideal(const Idealspacing *i)
{
    const PCol *l =i->left;
    const PCol *r= i->right;
    
    if (!contains(l) || !contains(r)) {
	return;
    }
    ideals.add(i);
}

void
Spacing_problem::print_ideal(const Idealspacing*id)const
{
#ifndef NPRINT
    int l = col_id(id->left);
    int r = col_id(id->right);

    mtor << "between " << l <<","<<r<<":" ;
    id->print();
#endif
}

void
Spacing_problem::print() const
{
#ifndef NPRINT
    for (int i=0; i < cols.sz(); i++) {
	mtor << "col " << i<<' ';
	cols[i].print();
    }
    for (int i=0; i < ideals.sz(); i++) {
	print_ideal(ideals[i]);
    }
#endif
    
}

void
Colinfo::print() const
{
#ifndef NPRINT
    mtor << "column { ";
    if (fixed)
	mtor << "fixed at " << fixpos<<", ";
    assert(pcol_);
    mtor << "[" << minleft() << ", " << minright() << "]";
    mtor <<"}\n";
#endif
}

Colinfo::Colinfo()
{
    fixed=false;
    pcol_=0;
}
