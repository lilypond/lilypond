/*
  spring-spacer.cc -- implement Spring_spacer

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include <math.h>
#include "spring-spacer.hh"
#include "p-col.hh"
#include "debug.hh"
#include "qlp.hh"
#include "unionfind.hh"
#include "idealspacing.hh"
#include "pointer.tcc"
#include "score-column.hh"
#include "paper-def.hh"
#include "dimen.hh"
#include "minterval.hh"

Vector
Spring_spacer::default_solution()const
{
    	return try_initial_solution() ; 
}

Score_column*
Spring_spacer::scol_l(int i) 
{
    return (Score_column*)cols[i].pcol_l_;
}

const Real COLFUDGE=1e-3;
template class P<Real>;		// ugh.

bool
Spring_spacer::contains(PCol const *w)
{
    for (int i=0; i< cols.size(); i++)
	if (cols[i].pcol_l_ == w)
	    return true;
    return false;
}


void
Spring_spacer::OK() const
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
Spring_spacer::handle_loose_cols() 
{
    Union_find connected(cols.size());
    Array<int> fixed;
    for (PCursor<Idealspacing*> i(ideal_p_list_.top()); i.ok(); i++){
	connected.connect(i->left_i_,i->right_i_);		
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


/**
  Guess a stupid position for loose columns.  Put loose columns at
  regular distances from enclosing calced columns 
  */
void
Spring_spacer::position_loose_cols(Vector &sol_vec)const
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
Spring_spacer::check_constraints(Vector v) const 
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

bool
Spring_spacer::check_feasible() const
{
    Vector sol(try_initial_solution());
    return check_constraints(sol);     
}

/// generate a solution which obeys the min distances and fixed positions
Vector
Spring_spacer::try_initial_solution() const
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
Spring_spacer::find_initial_solution() const
{
    Vector v(try_initial_solution());     
    assert(check_constraints(v));
    return v;
}

// generate the matrices
void
Spring_spacer::make_matrices(Matrix &quad, Vector &lin, Real &c) const
{
    quad.fill(0);
    lin.fill(0);
    c = 0;
    
    for (PCursor<Idealspacing*> i(ideal_p_list_.top()); i.ok(); i++) {
	int l = i->left_i_;
	int r = i->right_i_;

	quad(r,r) += i->hooke_f_;
	quad(r,l) -= i->hooke_f_;
	quad(l,r) -= i->hooke_f_;
	quad(l,l) += i->hooke_f_;

	lin(r) -= i->space_f_*i->hooke_f_;
	lin(l) += i->space_f_*i->hooke_f_;

	c += sqr(i->space_f_);
    }
}

// put the constraints into the LP problem
void
Spring_spacer::make_constraints(Mixed_qp& lp) const
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
Spring_spacer::solve() const
{
    assert(check_feasible());

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
Spring_spacer::add_column(PCol  *col, bool fixed, Real fixpos)
{
    Colinfo c(col,(fixed)? &fixpos :  0);
    if (cols.size())
	c.rank_i_ = cols.top().rank_i_+1;
    else
	c.rank_i_ = 0;
    cols.push(c);
}

Array<PCol*>
Spring_spacer::error_pcol_l_arr()const
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
Spring_spacer::loosen_column(int i)
{
    Colinfo c=cols.get(i);
    for (PCursor<Idealspacing*> j(ideal_p_list_.top()); j.ok(); j++){
	if (j->left_i_ == i|| j->right_i_ == i)
	    j.del();
	else
	    j++;
    }
    c.ugh_b_ = true;
    
    int j=0;
    for (; j < loose_col_arr_.size(); j++) {
	if (loose_col_arr_[j].rank_i_ > c.rank_i_)
	    break;
    }
    loose_col_arr_.insert(c,j);
}


void
Spring_spacer::print() const
{
#ifndef NPRINT
    for (int i=0; i < cols.size(); i++) {
	mtor << "col " << i<<' ';
	cols[i].print();
    }
    for (PCursor<Idealspacing*> i(ideal_p_list_.top()); i.ok(); i++){
	i->print();
    }
#endif
    
}


void
Spring_spacer::connect(int i, int j, Real d, Real h)
{
    Idealspacing * s = new Idealspacing;
    s->left_i_ = i;
    s->right_i_ = j;
    s->space_f_ = d;
    s->hooke_f_ = h;
    
    ideal_p_list_.bottom().add(s);
}

/**
  walk through all durations in all Score_columns
 */
struct Durations_iter
{
    Spring_spacer * sp_l_;
    int col_i_;
    int d_i_;
    
    Durations_iter(Spring_spacer*);

    Moment duration()const;
    Moment when()const;
    
    bool ok()const;
    void next();
};

Durations_iter::Durations_iter(Spring_spacer * s)
{
    col_i_ =0;
    d_i_ =0;		// ugh
    
    sp_l_ = s;
    if (! sp_l_->scol_l(col_i_)->durations.size() )
	next();
}

Moment
Durations_iter::duration() const 
{
    return sp_l_->scol_l(col_i_)->durations[d_i_];
}

bool
Durations_iter::ok()const{
    return col_i_ < sp_l_->cols.size();
}

Moment
Durations_iter::when()const{
    return sp_l_->scol_l(col_i_)->when();
}

void
Durations_iter::next()
{
    d_i_ ++;
    while ( col_i_ < sp_l_->cols.size() 
	    && d_i_ >= sp_l_->scol_l(col_i_)->durations.size()){
	col_i_ ++;
	d_i_ =0;
    }
}

	
/**
  generate springs between columns.

  UNDER DESTRUCTION
  
  TODO: This needs rethinking.  Spacing should take optical
  effects into account, and should be local (measure wide)

  The algorithm is taken from : 

  John S. Gourlay. ``Spacing a Line of Music,'' Technical Report
  OSU-CISRC-10/87-TR35, Department of Computer and Information
  Science, The Ohio State University, 1987.
  
  */
void
Spring_spacer::calc_idealspacing()
{
 
    for (int i=0; i < cols.size(); i++) 
	scol_l(i)->preprocess();
    
    /* get the shortest running note at a time. */
    Array<Moment> shortest_arr_;
    {
	Durations_iter d_iter(this);
	for (int i=0; i < cols.size(); i++) {
	    Moment now = scol_l(i)->when();
	    while (  d_iter.ok() && now >= d_iter.when() ) {
		if ( now < d_iter.when() + d_iter.duration())
		    break;
		d_iter.next();
	    }
	    if ( d_iter.ok() && now >= d_iter.when()) {
		Durations_iter d2 = d_iter;
		Moment shortest = INFTY;
		while (d2.ok() && d2.when() <= now) {
		    shortest = shortest <? d2.duration();
		    d2.next();
		}
		shortest_arr_.push( shortest );
	    } else
		shortest_arr_.push(0);
	}
	
    }
#ifndef NPRINT
    mtor << "shortest:[ ";
    for (int i=0; i < shortest_arr_.size(); i++)
	mtor << shortest_arr_[i] << " ";
    mtor << "]\n";
#endif
  
    Array<Real> ideal_arr_;
    Array<Real> hooke_arr_;    
    for (int i=0; i < cols.size(); i++){
	ideal_arr_.push(  -1.0);
	hooke_arr_.push(1.0);
    }
    
    for (int i=0; i < cols.size(); i++) {
	if ( !scol_l(i)->musical_b()) {
	    ideal_arr_[i] = cols[i].minright() + 2 PT;
	    hooke_arr_[i] = 2.0;
	    if (i+1 < cols.size()) {
		Moment delta_t =  scol_l(i+1)->when() - scol_l(i)->when() ;
		Real dist = delta_t ? paper_l()->duration_to_dist(delta_t) : 0;
		if (delta_t && dist > ideal_arr_[i])
		    ideal_arr_[i] = dist;
	    }
	}
    }
    for (int i=0; i < cols.size(); i++) {
	if (scol_l(i)->musical_b()) {
	    Moment shortest_len = shortest_arr_[i];
	    if ( ! shortest_len ) {
		warning( "Can't find a ruling note at " 
			 +String( scol_l(i)->when()));
		shortest_len = 1;
	    }
	    Moment delta_t = scol_l(i+1)->when() - scol_l(i)->when();
	    Real dist = paper_l()->duration_to_dist(shortest_len);
	    dist *= delta_t / shortest_len;
	    if (!scol_l(i+1)->musical_b() ) {

		if (ideal_arr_[i+1] + cols[i+1].minleft() < dist) {
		    ideal_arr_[i+1] = dist/2 + cols[i+1].minleft();
		    hooke_arr_[i+1] =1.0;
		} 
		ideal_arr_[i] = dist/2;
	    } else
		ideal_arr_[i] = dist;
	}
    }

    for (int i=0; i < ideal_arr_.size()-1; i++) {
	assert (ideal_arr_[i] >=0 && hooke_arr_[i] >=0);
	connect(i, i+1, ideal_arr_[i], hooke_arr_[i]);
    }
 
}



void
Spring_spacer::prepare()
{
    calc_idealspacing();
    handle_loose_cols();
    print();
}

Line_spacer*
Spring_spacer::constructor() 
{
    return new Spring_spacer;
}
   
#if 0
void obsolete()
{
    for (int i=0; i < cols.size(); i++) {
	if (!scol_l(i)->used_b())
	    continue;
	
	
	int j = i+1;

	if (scol_l(i)->musical_b()) {
	    assert ( j < cols.size());
	    
	    for (int n=0; n < scol_l(i)->durations.size(); n++) {
		Moment d = scol_l(i)->durations[n];
		Real dist = paper_l()->duration_to_dist(d);
		Real strength =  scol_l(i)->durations[0]/scol_l(i)->durations[n];
		assert(strength <= 1.0);
		
		while (j < cols.size()) {
		    if (scol_l(j)->used_b() 
			&& scol_l(j)->when() >= d + scol_l(i)->when() )
			break;
		    j++;
		}
		if ( j < cols.size() ){
		    Moment delta_desired = scol_l(j)->when() - (d+scol_l(i)->when());
		    dist += paper_l()->duration_to_dist(delta_desired);
		    if (scol_l(j)->musical_b()) {
			dist += cols[j].minleft() + 2 PT;
		    }
		    connect(i, j, dist, strength);
		}
	    }
	} else if (j < cols.size()) {
	    while  (!scol_l(j)->used_b())
		j++;
	    
	    /* attach i to the next column in use. This exists, since
	      the last col is breakable, and therefore in use
	      */
	    
	    Moment d = scol_l(j)->when() - scol_l(i)->when();
	    Real minimal_f = cols[i].minright()  +cols[j].minleft() + 2 PT;
	    Real durdist_f = (d) ? paper_l()->duration_to_dist(d) : 0; // todo
	    
	    connect(i, j, minimal_f <? durdist_f, (d) ? 1.0:1.0);
	}
	// !j.ok() might hold if we're at the last col.
    }
}
#endif
