/*
  spring-spacer.cc -- implement Spring_spacer

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include <math.h>
#include <limits.h>
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
#include "colhpos.hh"
#include "main.hh"		// experimental_fietsers

Vector
Spring_spacer::default_solution() const
{
  return try_initial_solution() ;
}

Score_column*
Spring_spacer::scol_l (int i)
{
  return (Score_column*)cols[i].pcol_l_;
}

const Real COLFUDGE=1e-3;
template class P<Real>;		// ugh.

bool
Spring_spacer::contains (Paper_column const *w)
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
    assert (cols[i].rank_i_ > cols[i-1].rank_i_);
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
  Union_find connected (cols.size());
  Array<int> fixed;
  for (PCursor<Idealspacing*> i (ideal_p_list_.top()); i.ok (); i++)
    {
      connected.connect (i->left_i_,i->right_i_);
    }
  for (int i = 0; i < cols.size(); i++)
    if (cols[i].fixed())
      fixed.push (i);
  for (int i=1; i < fixed.size(); i++)
    connected.connect (fixed[i-1], fixed[i]);

  for (int i = cols.size(); i--;)
    {
      if (! connected.equiv (fixed[0], i))
	{
	  warning (_("unconnected column: ") + String (i));
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
  fix_b_arr.set_size (cols.size() + loose_col_arr_.size ());
  Real utter_right_f=-infinity_f;
  Real utter_left_f =infinity_f;
  for (int i=0; i < loose_col_arr_.size(); i++)
    {
      fix_b_arr[loose_col_arr_[i].rank_i_] = false;
    }
  for (int i=0; i < cols.size(); i++)
    {
      int r= cols[i].rank_i_;
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
	  assert (cols[j].rank_i_ == i);
	  v (i) = sol_vec (j++);
	}
      else
	{
	  Real left_pos_f =
	    (j>0) ?sol_vec (j-1) : utter_left_f;
	  Real right_pos_f =
	    (j < sol_vec.dim()) ? sol_vec (j) : utter_right_f;
	  int left_rank = (j>0) ? cols[j-1].rank_i_ : 0;
	  int right_rank = (j<sol_vec.dim()) ? cols[j].rank_i_ : sol_vec.dim ();

	  int d_r = right_rank - left_rank;
	  Colinfo loose=loose_col_arr_[k++];
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
  assert (dim == cols.size());

  for (int i=0; i < dim; i++)
    {

      if (cols[i].fixed()&&
	  abs (cols[i].fixed_position() - v (i)) > COLFUDGE)
	return false;

      if (!i)
	continue;

      Real mindist=cols[i-1].width_[RIGHT]
	-cols[i].width_[LEFT];

      // ugh... compares
      Real dif =v (i) - v (i-1)- mindist;
      bool b = (dif > - COLFUDGE);


      if (!b)
	return false;

    }
  return true;
}

/// try to generate a solution which obeys the min distances and fixed
/// positions
Vector
Spring_spacer::try_initial_solution() const
{
  int dim=cols.size();
  Vector initsol (dim);
  for (int i=0; i < dim; i++)
    {
      if (cols[i].fixed())
	{
	  initsol (i)=cols[i].fixed_position();

	  if (i > 0)
	    {
	      Real r =initsol (i-1)  + cols[i-1].width_[RIGHT];
	      if (initsol (i) < r)
		  initsol (i) =r;
	    }

	}
      else
	{
	  Real mindist=cols[i-1].width_[RIGHT]
	    - cols[i].width_[LEFT];
	  if (mindist < 0.0)
	    warning (_("Excentric column"));
	  initsol (i)=initsol (i-1)+mindist;
	}
    }

  return initsol;
}



// generate the matrices
void
Spring_spacer::make_matrices (Matrix &quad, Vector &lin, Real &c) const
{
  quad.fill (0);
  lin.fill (0);
  c = 0;

  for (PCursor<Idealspacing*> i (ideal_p_list_.top()); i.ok (); i++)
    {
      int l = i->left_i_;
      int r = i->right_i_;

      quad (r,r) += i->hooke_f_;
      quad (r,l) -= i->hooke_f_;
      quad (l,r) -= i->hooke_f_;
      quad (l,l) += i->hooke_f_;

      lin (r) -= i->space_f_*i->hooke_f_;
      lin (l) += i->space_f_*i->hooke_f_;

      c += sqr (i->space_f_);
    }
}

void
Spring_spacer::set_fixed_cols (Mixed_qp &qp) const
{
  for (int j=0; j < cols.size(); j++)
    if (cols[j].fixed())
      qp.add_fixed_var (j,cols[j].fixed_position());
} 

// put the constraints into the LP problem
void
Spring_spacer::make_constraints (Mixed_qp& lp) const
{
  int dim=cols.size();
  for (int j=0; j < dim; j++)
    {
      Colinfo c=cols[j];
      if (j > 0)
	{
	  Vector c1(dim);

	  c1(j)=1.0 ;
	  c1(j-1)=-1.0 ;

	  lp.add_inequality_cons (c1, cols[j-1].width_[RIGHT] 
				  - cols[j].width_[LEFT]);
	}
    }
}


Real
Spring_spacer::calculate_energy_f (Vector solution) const
{
  Real e = 0.0;
  for (PCursor<Idealspacing*> i (ideal_p_list_.top()); i.ok(); i++)
    {
      e += i->energy_f(solution(i->right_i_) - solution(i->left_i_));
    }

  return e;
}
void
Spring_spacer::lower_bound_solution (Col_hpositions*positions) const
{
  Mixed_qp lp (cols.size());
  make_matrices (lp.quad,lp.lin, lp.const_term);
  set_fixed_cols (lp);

  Vector start (cols.size());
  start.fill (0.0);
  Vector solution_vec (lp.solve (start));

  DOUT << "Lower bound sol: " << solution_vec;
  positions->energy_f_ = calculate_energy_f (solution_vec);
  positions->config = solution_vec;
  positions->satisfies_constraints_b_ = check_constraints (solution_vec);
}

void
Spring_spacer::solve (Col_hpositions*positions) const
{
  Vector solution_try (try_initial_solution());
  
  if  (check_constraints (solution_try))
    {
      Mixed_qp lp (cols.size());
      make_matrices (lp.quad,lp.lin, lp.const_term);
      make_constraints (lp);
      set_fixed_cols (lp);

      Vector solution_vec (lp.solve (solution_try));


      positions->satisfies_constraints_b_ = check_constraints (solution_vec);
      if (!positions->satisfies_constraints_b_)
	{
	  WARN << _("solution doesn't satisfy constraints.\n") ;
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
}

/**
  add one column to the problem.
*/
void
Spring_spacer::add_column (Paper_column  *col, bool fixed, Real fixpos)
{
  Colinfo c (col,(fixed)? &fixpos :  0);
  if (cols.size())
    c.rank_i_ = cols.top().rank_i_+1;
  else
    c.rank_i_ = 0;
  cols.push (c);
}

Line_of_cols
Spring_spacer::error_pcol_l_arr() const
{
  Array<Paper_column*> retval;
  for (int i=0; i< cols.size(); i++)
    if (cols[i].ugh_b_)
      retval.push (cols[i].pcol_l_);
  for (int i=0;  i < loose_col_arr_.size(); i++)
    {
      retval.push (loose_col_arr_[i].pcol_l_);
    }
  return retval;
}

void
Spring_spacer::loosen_column (int i)
{
  Colinfo c=cols.get (i);
  for (PCursor<Idealspacing*> j (ideal_p_list_.top()); j.ok (); j++)
    {
      if (j->left_i_ == i|| j->right_i_ == i)
	j.del();
      else
	j++;
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
  for (int i=0; i < cols.size(); i++)
    {
      DOUT << "col " << i<<' ';
      cols[i].print();
    }
  for (PCursor<Idealspacing*> i (ideal_p_list_.top()); i.ok (); i++)
    {
      i->print();
    }
#endif
}


void
Spring_spacer::connect (int i, int j, Real d, Real h)
{
  assert(d >= 0 && d <= 100 CM);
  assert(h >=0);

  Idealspacing * s = new Idealspacing;

  s->left_i_ = i ;
  s->right_i_ = j;
  s->space_f_ = d;
  s->hooke_f_ = h;

  ideal_p_list_.bottom().add (s);
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



/**
  get the shortest_playing running note at a time. */
void
Spring_spacer::get_ruling_durations(Array<Moment> &shortest_playing_arr,
				    Array<Moment> &context_shortest_arr)
{
  for (int i=0; i < cols.size(); i++)
    {
      scol_l (i)->preprocess();
      scol_l (i)->print ();
    }
  int start_context_i=0;
  Moment context_shortest = infinity_mom;
  context_shortest_arr.set_size(cols.size());

  for (int i=0; i < cols.size(); i++)
    {
      Moment now = scol_l (i)->when();
      Moment shortest_playing = infinity_mom;

      if (scol_l (i)->breakable_b_)
	{
	  for (int ji=i; ji >= start_context_i; ji--)
	    context_shortest_arr[ji] = context_shortest;
	  start_context_i = i;
	  context_shortest = infinity_mom;
	}
      if (scol_l (i)->durations.size())
	{
	  context_shortest = context_shortest <? scol_l(i)->durations[0];
	}
      
      // ji was j, but triggered ICE
      for (int ji=i+1; ji --;)
	{
	  if (scol_l(ji)->durations.size() &&
	      now - scol_l(ji)->when() >= shortest_playing)
	    break;

	  for (int k =  scol_l (ji)->durations.size();
	       k-- && scol_l(ji)->durations[k] + scol_l(ji)->when() > now;
	       )
	    {
	      shortest_playing = shortest_playing <? scol_l(ji)->durations[k];
	    }
	}
      shortest_playing_arr.push(shortest_playing);
    }

#ifndef NPRINT
  DOUT << "shortest_playing/:[ ";
  for (int i=0; i < shortest_playing_arr.size(); i++)
    {
      DOUT << shortest_playing_arr[i] << " ";
      DOUT << context_shortest_arr[i] << ", ";
    }
  DOUT << "]\n";
#endif
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
  Array<Moment> shortest_playing_arr;
  Array<Moment> context_shortest_arr;
  get_ruling_durations(shortest_playing_arr, context_shortest_arr);

  Real interline_f = paper_l ()->interline_f ();
  Real nw_f = paper_l ()->note_width ();

  Array<Real> ideal_arr_;
  Array<Real> hooke_arr_;
  for (int i=0; i < cols.size() - 1; i++){
    ideal_arr_.push (-1.0);
    hooke_arr_.push (1.0);
  }

  /* 
     First do all non-musical columns
   */
  for (int i=0; i < cols.size(); i++)
    {
      if (!scol_l (i)->musical_b() && i+1 < cols.size())
	{
	  Real symbol_distance =cols[i].width_[RIGHT] + 2 PT;
	  Real durational_distance = 0;

	  
	      Moment delta_t =  scol_l (i+1)->when() - scol_l (i)->when () ;

	      Real k=  paper_l()->arithmetic_constant(context_shortest_arr[i]);
	      /*
		ugh should use shortest_playing distance
		*/
	      if (delta_t)
		durational_distance =  paper_l()->duration_to_dist (delta_t,k);
	      symbol_distance += -cols[i+1].width_[LEFT];
 

	  ideal_arr_[i] = symbol_distance >? durational_distance;
	  hooke_arr_[i] = 1; //2.0;
	}
    }

  /* 
     Then musicals
     */
  for (int i=0; i < cols.size(); i++)
    {
      if (scol_l (i)->musical_b())
	{
	  Moment shortest_playing_len = shortest_playing_arr[i];
	  Moment context_shortest = context_shortest_arr[i];
	  if (! shortest_playing_len)
	    {
	      warning (_("Can't find a ruling note at ")
		       +String (scol_l (i)->when()));
	      shortest_playing_len = 1;
	    }
	  if (! context_shortest)
	    {
	      warning(_("No minimum in measure at ")
		      + String (scol_l (i)->when()));
	      context_shortest = 1;
	    }
	  Moment delta_t = scol_l (i+1)->when() - scol_l (i)->when ();
	  Real k=  paper_l()->arithmetic_constant(context_shortest);
	  Real dist = paper_l()->duration_to_dist (shortest_playing_len, k);
	  dist *= delta_t / shortest_playing_len;

	  /*
	     According to [Ross] and [Wanske], and from what i've seen:
	     
	     * whitespace at the begin of the bar should be fixed at 
	     (about) one interline.
	     [Ross]:
	     when spacing gets real tight, a smaller fixed value may be 
	     used, so that there are two discrete amounts of whitespace 
	     possible at the begin of a bar; but this is not implemented 
	     right now.
	     
	     * whitespace at the end of the bar is the normal amount of 
	     "hinterfleish" that would have been used, had there been
	     yet another note in the bar.  
	     [Ross]:
	     some editors argue that the bar line should not take any 
	     space, not to hinder the flow of music spaced around a bar 
	     line.  
	     [Ross] and [Wanske] do not suggest this, however.  Further, 
	     it introduces some spacing problems and think that it is ugly 
	     too.
	     [jcn]
	   */

	  /* 
	     first musical column of bar
	   */
	  if (i && scol_l (i - 1)->breakable_b_)
	    {
	      // fixed: probably should set minimum (rod/spring)?
	      cols[i-1].width_[RIGHT] += interline_f;
	      // should adjust dist too?
	      ideal_arr_[i-1] = ideal_arr_[i-1] >? interline_f;
	    }

	  /* 
	     last musical column of bar
	   */
	  if (i + 1 < cols.size () && scol_l(i+1)->breakable_b_)
	    {
	      // hmm, how bout?
	      dist = dist >? interline_f;

	      /*
	        uhuh, this code looks fine, already?
		someone was junking this last "hinterfleisch" whitespace?!

		but this seems to be fixed now :-)
	       */
	       // set minimum rod 
	      cols[i].width_[RIGHT] += interline_f;
	    }

	  // ugh, do we need this?
	  if (i < cols.size () - 1 && !scol_l (i + 1)->musical_b ())
	    {
	      Real minimum = -cols[i + 1].width_[LEFT] + cols[i].width_[RIGHT]
		+ interline_f / 2;
	      dist = dist >? minimum;
	    }

          // ugh: never let columns touch... try to set over here...
	  // ugh: use j iso i triggers ice in gcc-2.7.2.3 
          cols[i].width_[LEFT] -= nw_f / 4;
	  ideal_arr_[i] = dist;
	}
    }

  for (int i=0; i < ideal_arr_.size(); i++)
    {
      assert (ideal_arr_[i] >=0 && hooke_arr_[i] >=0);
      connect (i, i+1, ideal_arr_[i], hooke_arr_[i]);
    }
}
