#include <math.h>
#include "linespace.hh"
#include "p-col.hh"
#include "debug.hh"
#include "qlp.hh"
#include "unionfind.hh"
#include "idealspacing.hh"
#include "pointer.tcc"

const Real COLFUDGE=1e-3;
template class P<Real>;		// ugh.

bool
Spacing_problem::contains(PCol const *w)
{
    for (int i=0; i< cols.size(); i++)
	if (cols[i].pcol_l_ == w)
	    return true;
    return false;
}

int 
Spacing_problem::col_id(PCol const *w)const
{
    for (int i=0; i< cols.size(); i++)
	if (cols[i].pcol_l_ == w)
	    return i;
    assert(false);
    return -1;
}

void
Spacing_problem::OK() const
{
#ifndef NDEBUG
    for (int i = 1; i < cols.size(); i++)
	assert(cols[i].rank_i_ > cols[i-1].rank_i_);
    for (int i = 1; i < loose_col_arr_.size(); i++)
	assert(loose_col_arr_[i].rank_i_ > loose_col_arr_[i-1].rank_i_);
#endif 
}

/**
  Make sure no unconnected columns happen. 
 */
void
Spacing_problem::handle_loose_cols() 
{
    Union_find connected(cols.size());
    Array<int> fixed;
    for (int i=0; i < ideals.size(); i++) {
	assert(ideals[i]->hooke > 0);
	int l = col_id(ideals[i]->left);
	int r = col_id(ideals[i]->right);
	connected.connect(l,r);		
    }
    for (int i = 0; i < cols.size(); i++)
	if (cols[i].fixed())
	    fixed.push(i);
    for (int i=1; i < fixed.size(); i++)
	connected.connect(fixed[i-1], fixed[i]);

    for (int i = cols.size(); i--; ) {
	if (! connected.equiv(fixed[0], i)) {
	    warning("unconnected column: " + String(i));
	    loosen_column(i);
	}
    }
    OK();
}


/** Guess a stupid position for loose columns.  Put loose columns at
  regular distances from enclosing calced columns */
void
Spacing_problem::position_loose_cols(Vector &sol_vec)const
{
    if (!loose_col_arr_.size())
	return ; 
    assert(sol_vec.dim());
    Array<bool> fix_b_arr;
    fix_b_arr.set_size(cols.size() + loose_col_arr_.size());
    Real utter_right_f=-INFTY;
    Real utter_left_f =INFTY;
    for (int i=0; i < loose_col_arr_.size(); i++) {
	fix_b_arr[loose_col_arr_[i].rank_i_] = false;
    }
    for (int i=0; i < cols.size(); i++) {
	int r= cols[i].rank_i_;
	fix_b_arr[r] = true;
	utter_right_f = utter_right_f >? sol_vec(i);
	utter_left_f = utter_left_f <? sol_vec(i);
    }
    Vector v(fix_b_arr.size());
    int j =0;
    int k =0;
    for (int i=0; i < v.dim(); i++) {
	if (fix_b_arr[i]) {
	    assert(cols[j].rank_i_ == i);
	    v(i) = sol_vec(j++);
	} else {
	    Real left_pos_f = 
		(j>0) ?sol_vec(j-1) : utter_left_f;
	    Real right_pos_f = 
		(j < sol_vec.dim()) ? sol_vec(j) : utter_right_f;
	    int left_rank = (j>0) ? cols[j-1].rank_i_ : 0;
	    int right_rank = (j<sol_vec.dim()) ? cols[j].rank_i_ : sol_vec.dim();

	    int d_r = right_rank - left_rank;
	    Colinfo loose=loose_col_arr_[k++];
	    int r = loose.rank_i_ ;
	    assert(r > left_rank && r < right_rank);

	    v(i) =  (r - left_rank)*left_pos_f/ d_r + 
		(right_rank - r) *right_pos_f /d_r;
	}
    }
    sol_vec = v;
}
 
bool
Spacing_problem::check_constraints(Vector v) const 
{
    int dim=v.dim();
    assert(dim == cols.size());
    
    for (int i=0; i < dim; i++) {

	if (cols[i].fixed()&&
	    abs(cols[i].fixed_position() - v(i)) > COLFUDGE) 
	    return false;
	
	if (!i) 
	    continue;
	
	Real mindist=cols[i-1].minright()
	    +cols[i].minleft();

	// ugh... compares
	Real dif =v(i) - v(i-1)- mindist;
	bool b = (dif > - COLFUDGE);
	

	if (!b)
	    return false;

    }
    return true;
}

void
Spacing_problem::prepare()
{
    handle_loose_cols();
}

bool
Spacing_problem::check_feasible() const
{
    Vector sol(try_initial_solution());
    return check_constraints(sol);     
}

/// generate a solution which obeys the min distances and fixed positions
Vector
Spacing_problem::try_initial_solution() const
{
    int dim=cols.size();
    Vector initsol(dim);
    for (int i=0; i < dim; i++) {
	if (cols[i].fixed()) {
	    initsol(i)=cols[i].fixed_position();	

	    if (i > 0) {
		Real r =initsol(i-1)  + cols[i-1].minright();
		if (initsol(i) < r ) {
		    warning("overriding fixed position");
		    initsol(i) =r;
		} 
	    }
		
	} else {
	    Real mindist=cols[i-1].minright()
		+cols[i].minleft();
	    if (mindist < 0.0)
		warning("Excentric column");
	    initsol(i)=initsol(i-1)+mindist;
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
    c = 0;
    for (int j=0; j < ideals.size(); j++){
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
    int dim=cols.size();
    for (int j=0; j < dim; j++) {
	Colinfo c=cols[j];
	if (c.fixed()) {
	    lp.add_fixed_var(j,c.fixed_position());	    
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

Array<Real>
Spacing_problem::solve() const
{
    assert(check_feasible());
    print();

    Mixed_qp lp(cols.size());
    make_matrices(lp.quad,lp.lin, lp.const_term);
    make_constraints(lp);    
    Vector start=find_initial_solution();    
    Vector sol(lp.solve(start));
    if (!check_constraints(sol)) {
	WARN << "solution doesn't satisfy constraints.\n" ;
    }
    Real energy_f =lp.eval(sol);
    position_loose_cols(sol);

    Array<Real> posns(sol);

    posns.push(energy_f);
    return posns;
}

/**
    add one column to the problem.
*/    
void
Spacing_problem::add_column(PCol  *col, bool fixed, Real fixpos)
{
    Colinfo c(col,(fixed)? &fixpos :  0);
    if (cols.size())
	c.rank_i_ = cols.top().rank_i_+1;
    else
	c.rank_i_ = 0;
    cols.push(c);
}

Array<PCol*>
Spacing_problem::error_pcol_l_arr()const
{
    Array<PCol*> retval;
    for (int i=0; i< cols.size(); i++)
	if (cols[i].ugh_b_)
	    retval.push(cols[i].pcol_l_);
    for (int i=0;  i < loose_col_arr_.size(); i++) {
	retval.push(loose_col_arr_[i].pcol_l_);
    }
    return retval;
}

void
Spacing_problem::loosen_column(int i)
{
    Colinfo c=cols.get(i);
    for (int i=0; i < ideals.size(); ) {
	Idealspacing const *i_l =ideals[i];
	if (i_l->left == c.pcol_l_ || i_l->right == c.pcol_l_)
	    ideals.del(i);
	else
	    i++;
    }
    c.ugh_b_ = true;
    
    int i=0;
    for (; i < loose_col_arr_.size(); i++) {
	if (loose_col_arr_[i].rank_i_ > c.rank_i_)
	    break;
    }
    loose_col_arr_.insert(c,i);
}

void
Spacing_problem::add_ideal(Idealspacing const *i)
{
    PCol const *l =i->left;
    PCol const *r= i->right;
    
    if (!contains(l) || !contains(r)) {
	return;
    }
    ideals.push(i);
}

void
Spacing_problem::print_ideal(Idealspacing const *id)const
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
    for (int i=0; i < cols.size(); i++) {
	mtor << "col " << i<<' ';
	cols[i].print();
    }
    for (int i=0; i < ideals.size(); i++) {
	print_ideal(ideals[i]);
    }
#endif
    
}

/* **************** */

void
Colinfo::print() const
{
#ifndef NPRINT
    mtor << "column { ";
    if (fixed())
	mtor << "fixed at " << fixed_position()<<", ";
    assert(pcol_l_);
    mtor << "[" << minleft() << ", " << minright() << "]";
    mtor <<"}\n";
#endif
}

Colinfo::Colinfo(PCol *col_l, Real const *fixed_C)
{
    if (fixed_C)
	fixpos_p_.set_l(fixed_C);
    ugh_b_ = false;
    pcol_l_ = col_l;
    width = pcol_l_->width();
}


Colinfo::Colinfo()
{
    ugh_b_ = false;
    pcol_l_ =0;
}
