/*
  spring-spacer.cc -- implement Spring_spacer

  source file of the GNU LilyPond music typesetter

  (c) 1996--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include <math.h>
#include <limits.h>
#include "killing-cons.tcc"
#include "spring-spacer.hh"
#include "p-col.hh"
#include "debug.hh"
#include "dimensions.hh"
#include "qlp.hh"
#include "unionfind.hh"
#include "idealspacing.hh"
#include "pointer.tcc"
#include "score-column.hh"
#include "paper-def.hh"
#include "colhpos.hh"
#include "main.hh"

Vector
Spring_spacer::default_solution() const
{
  return try_initial_solution();
}

Score_column*
Spring_spacer::scol_l (int i)
{
  return dynamic_cast<Score_column*>(cols_[i].pcol_l_);
}

const Real COLFUDGE=1e-3;
template class P<Real>;		// ugh.

bool
Spring_spacer::contains_b (Paper_column const *w)
{
  for (int i=0; i< cols_.size(); i++)
    if (cols_[i].pcol_l_ == w)
      return true;
  return false;
}


void
Spring_spacer::OK() const
{
#ifndef NDEBUG
  for (int i = 1; i < cols_.size(); i++)
    assert (cols_[i].rank_i_ > cols_[i-1].rank_i_);
  for (int i = 1; i < loose_col_arr_.size(); i++)
    assert (loose_col_arr_[i].rank_i_ > loose_col_arr_[i-1].rank_i_);
#endif
}

/**
  Make sure no unconnected columns happen.
 */
void
Spring_spacer::handle_loose_cols()
{
  Union_find connected (cols_.size());
  Array<int> fixed;
  
  for (Cons<Idealspacing> *i  = ideal_p_list_; i; i = i->next_)
    {
      connected.connect (i->car_->cols_drul_[LEFT],i->car_->cols_drul_[RIGHT]);
    }
  for (int i = 0; i < cols_.size(); i++)
    if (cols_[i].fixed_b())
      fixed.push (i);
  for (int i=1; i < fixed.size(); i++)
    connected.connect (fixed[i-1], fixed[i]);

  for (int i = cols_.size(); i--;)
    {
      if (! connected.equiv (fixed[0], i))
	{
	  warning (_f ("unconnected column: %d", i));
	  loosen_column (i);
	}
    }
  OK();
}


/**
  Guess a stupid position for loose columns.  Put loose columns at
  regular distances from enclosing calced columns
  */
void
Spring_spacer::position_loose_cols (Vector &sol_vec) const
{
  if (!loose_col_arr_.size())
    return ;
  assert (sol_vec.dim());
  Array<bool> fix_b_arr;
  fix_b_arr.set_size (cols_.size() + loose_col_arr_.size ());
  Real utter_right_f=-infinity_f;
  Real utter_left_f =infinity_f;
  for (int i=0; i < loose_col_arr_.size(); i++)
    {
      fix_b_arr[loose_col_arr_[i].rank_i_] = false;
    }
  for (int i=0; i < cols_.size(); i++)
    {
      int r= cols_[i].rank_i_;
      fix_b_arr[r] = true;
      utter_right_f = utter_right_f >? sol_vec (i);
      utter_left_f = utter_left_f <? sol_vec (i);
    }
  Vector v (fix_b_arr.size());
  int j =0;
  int k =0;
  for (int i=0; i < v.dim(); i++)
    {
      if (fix_b_arr[i])
	{
	  assert (cols_[j].rank_i_ == i);
	  v (i) = sol_vec (j++);
	}
      else
	{
	  Real left_pos_f =
	    (j>0) ?sol_vec (j-1) : utter_left_f;
	  Real right_pos_f =
	    (j < sol_vec.dim()) ? sol_vec (j) : utter_right_f;
	  int left_rank = (j>0) ? cols_[j-1].rank_i_ : 0;
	  int right_rank = (j<sol_vec.dim()) ? cols_[j].rank_i_ : sol_vec.dim ();

	  int d_r = right_rank - left_rank;
	  Column_info loose=loose_col_arr_[k++];
	  int r = loose.rank_i_ ;
	  assert (r > left_rank && r < right_rank);

	  v (i) =  (r - left_rank)*left_pos_f/ d_r +
	    (right_rank - r) *right_pos_f /d_r;
	}
    }
  sol_vec = v;
}

bool
Spring_spacer::check_constraints (Vector v) const
{
  int dim=v.dim();
  assert (dim == cols_.size());
  DOUT << "checking " << v;
  for (int i=0; i < dim; i++)
    {
      if (cols_[i].fixed_b() &&
	  abs (cols_[i].fixed_position() - v (i)) > COLFUDGE)
	{
	  DOUT << "Fixpos broken\n";
	  return false;
	}
      Array<Spacer_rod> const &rods (cols_[i].rods_[RIGHT]);
      for (int j =0; j < rods.size (); j++)
	{
	  int other =rods[j].other_idx_;
	  Real diff =v (other) - v (i) ;
	  if (COLFUDGE +diff <  rods[j].distance_f_)
	    {
	      DOUT << "i, other_i: " << i << "  " << other << '\n';
	      DOUT << "dist, minimal = " << diff << " "
		   << rods[j].distance_f_ << '\n';
	      return false;
	    }
	}

    }
  return true;
}

/** try to generate a solution which obeys the min
    distances and fixed positions
 */
Vector
Spring_spacer::try_initial_solution() const
{
  Vector v;
  if (!try_initial_solution_and_tell (v))
    {
      warning (_ ("I'm too fat; call Oprah"));
    }
  return v;

}

bool
Spring_spacer::try_initial_solution_and_tell (Vector &v) const
{
  int dim=cols_.size();
  bool succeeded = true;
  Vector initsol (dim);

  assert (cols_[0].fixed_b ());
  DOUT << "fixpos 0 " << cols_[0].fixed_position ();
  for (int i=0; i < dim; i++)
    {
      Real min_x = i ?  initsol (i-1) : cols_[0].fixed_position ();
      Array<Spacer_rod> const &sr_arr(cols_[i].rods_[LEFT]);
      for (int j=0; j < sr_arr.size (); j++)
	{
	  min_x = min_x >? (initsol (sr_arr[j].other_idx_) + sr_arr[j].distance_f_);
	}
      initsol (i) = min_x;
      
      if (cols_[i].fixed_b())
	{
	  initsol (i)=cols_[i].fixed_position();
	  if (initsol (i) < min_x )
	    {
	      DOUT << "failing: init, min : " << initsol (i) << " " << min_x << '\n';
	      initsol (i) = min_x;
	      succeeded = false;
	    }
	}
    }
  v = initsol;
  
  DOUT << "tried and told solution: " << v;
  if (!succeeded)
    DOUT << "(failed)\n";
  return succeeded;
}



// generate the matrices
void
Spring_spacer::make_matrices (Matrix &quad, Vector &lin, Real &c) const
{
  quad.fill (0);
  lin.fill (0);
  c = 0;

  for (Cons<Idealspacing> *p =ideal_p_list_; p; p = p->next_)
    {
      Idealspacing *i = p->car_;
      int l = i->cols_drul_[LEFT];
      int r = i->cols_drul_[RIGHT];

      quad (r,r) += i->hooke_f_;
      quad (r,l) -= i->hooke_f_;
      quad (l,r) -= i->hooke_f_;
      quad (l,l) += i->hooke_f_;

      lin (r) -= i->space_f_*i->hooke_f_;
      lin (l) += i->space_f_*i->hooke_f_;

      c += sqr (i->space_f_);
    }

  if (quad.dim() > 10)
    quad.set_band();
  

}

void
Spring_spacer::set_fixed_cols (Mixed_qp &qp) const
{
  for (int j=0; j < cols_.size(); j++)
    if (cols_[j].fixed_b())
      qp.add_fixed_var (j,cols_[j].fixed_position());
} 

// put the constraints into the LP problem
void
Spring_spacer::make_constraints (Mixed_qp& lp) const
{
  int dim=cols_.size();
  
  for (int j=0; j < dim -1; j++)
    {
      Array<Spacer_rod> const&rod_arr (cols_[j].rods_[RIGHT]);
      for (int i = 0; i < rod_arr.size (); i++)
	{
	  Vector c1(dim);
	  c1(rod_arr[i].other_idx_)=1.0 ;
	  c1(j)=-1.0 ;

	  lp.add_inequality_cons (c1, rod_arr[i].distance_f_);
	}
    }
}


Real
Spring_spacer::calculate_energy_f (Vector solution) const
{
  Real e = 0.0;
  for (Cons<Idealspacing>*p =ideal_p_list_; p; p = p->next_)
    {
      Idealspacing * i = p->car_;
      e += i->energy_f(solution(i->cols_drul_[RIGHT]) - solution(i->cols_drul_[LEFT]));
    }

  return e;
}
void
Spring_spacer::lower_bound_solution (Column_x_positions*positions) const
{
  Mixed_qp lp (cols_.size());
  make_matrices (lp.quad_,lp.lin_, lp.const_term_);
  set_fixed_cols (lp);

  Vector start (cols_.size());
  start.fill (0.0);
  Vector solution_vec (lp.solve (start));

  DOUT << "Lower bound sol: " << solution_vec;
  positions->energy_f_ = calculate_energy_f (solution_vec);
  positions->config = solution_vec;
  positions->satisfies_constraints_b_ = check_constraints (solution_vec);
}

Spring_spacer::Spring_spacer ()
{
  ideal_p_list_ =0;
  energy_normalisation_f_ = 1.0;
}

void
Spring_spacer::solve (Column_x_positions*positions) const
{
  DOUT << "Spring_spacer::solve ()...";

  Vector solution_try;
    
  bool constraint_satisfaction = try_initial_solution_and_tell (solution_try); 
  if  (constraint_satisfaction)
    {
      Mixed_qp lp (cols_.size());
      make_matrices (lp.quad_,lp.lin_, lp.const_term_);
      make_constraints (lp);
      set_fixed_cols (lp);
	
      Vector solution_vec (lp.solve (solution_try));
	
      positions->satisfies_constraints_b_ = check_constraints (solution_vec);
      if (!positions->satisfies_constraints_b_)
	{
	  WARN << _ ("solution doesn't satisfy constraints") << '\n' ;
	}
      position_loose_cols (solution_vec);
      positions->energy_f_ = calculate_energy_f (solution_vec);
      positions->config = solution_vec;
      positions->error_col_l_arr_ = error_pcol_l_arr();
    }
  else
    {
      positions->set_stupid_solution (solution_try);
    }

  DOUT << "Finished Spring_spacer::solve ()...";
}

/**
  add one column to the problem.
*/
void
Spring_spacer::add_column (Paper_column  *col, bool fixed, Real fixpos)
{
  Column_info c (col,(fixed)? &fixpos :  0);
  int this_rank =  cols_.size();
  c.rank_i_ = this_rank;
  
  for (int i=0; i < col->minimal_dists_arr_drul_[LEFT].size (); i++)
    {
      Column_rod &cr = col->minimal_dists_arr_drul_[LEFT][i];
      int left_idx = cr.other_l_->rank_i () - cols_[0].pcol_l_->rank_i ();
      if (left_idx < 0)
	continue;

      if (cols_[left_idx].pcol_l_ != cr.other_l_)
	continue;

      Spacer_rod l_rod;
      l_rod.distance_f_ = cr.distance_f_;
      l_rod.other_idx_ = left_idx;
      c.rods_[LEFT].push (l_rod);

      Spacer_rod r_rod;
      r_rod.distance_f_ = cr.distance_f_;
      r_rod.other_idx_ = this_rank;
      cols_[left_idx].rods_[RIGHT].push (r_rod);
    }

  for (int i=0; i < col->spring_arr_drul_[LEFT].size (); i++)
    {
      Column_spring &cr = col->spring_arr_drul_[LEFT][i];
      int idx = cr.other_l_->rank_i () - cols_[0].pcol_l_->rank_i ();
      if (idx < 0)
	continue;
      
      if (cols_[idx].pcol_l_ != cr.other_l_)
	    continue;
      
      Real d  = cr.distance_f_;
      if (fabs (d) < EPS)
	{
	  connect (idx, this_rank, 0.0, 10); // large strength. 
	  programming_error ("requesting zero distance.");
	}
      else
	connect (idx, this_rank, cr.distance_f_,
		 cr.strength_f_ / cr.distance_f_);
    }
      
  cols_.push (c);
}

Line_of_cols
Spring_spacer::error_pcol_l_arr() const
{
  Link_array<Paper_column> retval;
  for (int i=0; i< cols_.size(); i++)
    if (cols_[i].ugh_b_)
      retval.push (cols_[i].pcol_l_);
  for (int i=0;  i < loose_col_arr_.size(); i++)
    {
      retval.push (loose_col_arr_[i].pcol_l_);
    }
  return retval;
}

/*
  Ugh. Should junk this.
 */
void
Spring_spacer::loosen_column (int idx)
{
  Column_info c=cols_.get (idx);

  Cons<Idealspacing> **pp = &ideal_p_list_;

  while (*pp)
    {
      Idealspacing *j = (*pp)->car_;
      if (j->cols_drul_[LEFT] == idx|| j->cols_drul_[RIGHT] == idx)
	{
	  delete remove_cons (pp);
	}
      else
	{
	  pp = &(*pp)->next_;
	}
    }
  c.ugh_b_ = true;

  int j=0;
  for (; j < loose_col_arr_.size(); j++)
    {
      if (loose_col_arr_[j].rank_i_ > c.rank_i_)
	break;
    }
  loose_col_arr_.insert (c,j);
}


void
Spring_spacer::print() const
{
#ifndef NPRINT
  for (int i=0; i < cols_.size(); i++)
    {
      DOUT << "col " << i << " ";
      cols_[i].print();
    }

  for (Cons<Idealspacing> *p =ideal_p_list_; p; p = p->next_)
    {
      p->car_->print();
    }
#endif
}


void
Spring_spacer::connect (int i, int j, Real d, Real h)
{
  assert(d >= 0 && d <= 100 CM);
  assert(h >=0);

  Idealspacing * s = new Idealspacing;

  s->cols_drul_[LEFT] = i ;
  s->cols_drul_[RIGHT] = j;
  s->space_f_ = d;
  s->hooke_f_ = h;

  ideal_p_list_ = new Killing_cons<Idealspacing> (s, ideal_p_list_);
}

void
Spring_spacer::prepare()
{
  handle_loose_cols();
  print();
}

Line_spacer*
Spring_spacer::constructor()
{
  return new Spring_spacer;
}




Spring_spacer::~Spring_spacer()
{
  delete ideal_p_list_;
}
