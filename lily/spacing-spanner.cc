/*   
  spacing-spanner.cc --  implement Spacing_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "spacing-spanner.hh"
#include "score-column.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "warn.hh"
#include "p-score.hh"
#include "line-of-score.hh"

Spacing_spanner::Spacing_spanner ()
{
  set_elt_property (break_helper_only_scm_sym, SCM_BOOL_T);
  set_elt_property (transparent_scm_sym, SCM_BOOL_T);
}

int
Spacing_spanner::col_count () const
{
  return pscore_l_->line_l_->cols_.size ();
}

Score_column *
Spacing_spanner::scol (int i)const
{
  return dynamic_cast<Score_column*> (pscore_l_->line_l_->cols_[i]);
}

/*
  cut 'n paste from spring-spacer.cc

  generate springs between columns.


  TODO
  
  * Spacing should take optical effects into account
  
  The algorithm is partly taken from :

  John S. Gourlay. ``Spacing a Line of Music,'' Technical Report
  OSU-CISRC-10/87-TR35, Department of Computer and Information
  Science, The Ohio State University, 1987.
  
 */
Array<Spring>
Spacing_spanner::do_measure (int col1, int col2) const
{
  for (int i =col1; i < col2; i++)
    {
      scol (i)->preprocess ();
      scol (i)->print ();
    }

  Moment shortest;
  shortest.set_infinite (1);
  for (int i =col1; i < col2; i++)
    {
      if (scol(i)->musical_b ())
	{
	  shortest = shortest <? scol(i)->shortest_starter_mom_;
	}
    }

  Array<Spring> meas_springs;

  /*
    UGR GUR URG.  duplicate code for spacing generation.
   */
  for (int i= col1; i < col2; i++)
    {
      SCM hint = scol (i)->get_elt_property (extra_space_scm_sym);
      if (hint != SCM_BOOL_F)
	{
	  hint = SCM_CDR (hint);

	  Spring s;
	  s.item_l_drul_[LEFT] = scol (i);
	  s.item_l_drul_[RIGHT] = scol (i+1);
	  Real unbroken_dist =  gh_scm2double (SCM_CDR(hint));

	  s.distance_f_ = unbroken_dist;
	  s.strength_f_ = 2.0;
	  

	  meas_springs.push (s);

	  
	  Item * l = scol(i)->find_prebroken_piece (RIGHT);
	  Item * r = scol(i+1)->find_prebroken_piece (LEFT);
	  if (l)
	    {
	        Spring s;
		s.item_l_drul_[LEFT] = l;
		s.item_l_drul_[RIGHT] = scol (i+1);
		hint = l->get_elt_property (extra_space_scm_sym);

		if (hint == SCM_BOOL_F)
		  {
		    programming_error ("No postbreak breakable spacing hint set.");
		    s.distance_f_= unbroken_dist;
		  }
		else
		  s.distance_f_ =  gh_scm2double (SCM_CDDR(hint));

		/*
		  space around barlines should not stretch very much.
		 */
		s.strength_f_ = 2.0;
		meas_springs.push (s);
	    }

	  if (r)
	    {
	      Spring s;
	      s.item_l_drul_[LEFT] = scol (i);
	      s.item_l_drul_[RIGHT] = r;
	      s.distance_f_ =  unbroken_dist;
	      
	      /*
		space around barlines should not stretch very much.
		 */
	      s.strength_f_ = 2.0;
	      meas_springs.push (s);
	    }

	  if (l&&r)
	    {
	      Spring s;
	      s.item_l_drul_[LEFT] = l;
	      s.item_l_drul_[RIGHT] = r;
	      
	      hint = l->get_elt_property (extra_space_scm_sym);
	      if (hint == SCM_BOOL_F)
		{
		  programming_error ("No postbreak breakable spacing hint set.");
		  s.distance_f_= unbroken_dist;
		}
	      else
		s.distance_f_ =  gh_scm2double (SCM_CDDR(hint));
	      
	      /*
		space around barlines should not stretch very much.
	      */
	      s.strength_f_ = 2.0;
	      meas_springs.push (s);
	    }
	}
      else if (!scol (i)->musical_b() && i+1 < col_count())
	{
	  Real symbol_distance = scol (i)->extent (X_AXIS)[RIGHT] ;
	  Real durational_distance = 0;
	  Moment delta_t =  scol (i+1)->when_mom () - scol (i)->when_mom () ;
	  /*
	    ugh should use shortest_playing distance
	  */
	  if (delta_t)
	    {
  	      Real k=  paper_l()->arithmetic_constant (shortest);
	      durational_distance =  paper_l()->length_mom_to_dist (delta_t,k);
	    }
	  symbol_distance += -scol (i+1)->extent(X_AXIS)[LEFT];

	  Spring s ;
	  s.item_l_drul_[LEFT] = scol (i);
	  s.item_l_drul_[RIGHT] = scol (i+1);
	  s.distance_f_ =  symbol_distance >? durational_distance;
	  meas_springs.push (s);

	  Item *l = s.item_l_drul_[LEFT]->find_prebroken_piece (RIGHT);
	  Item *r = s.item_l_drul_[RIGHT]->find_prebroken_piece (LEFT);
	  Spring sp_orig (s);
	  
	  if (l)
	    {
	      s = sp_orig;
	      s.item_l_drul_[LEFT] =l ;
	      meas_springs.push (s);
	    }

	  if (l && r)
	    {
	      s = sp_orig;
	      s.item_l_drul_[RIGHT] = r;
	      s.item_l_drul_[LEFT] = l;
	      meas_springs.push (s);
	    }
	  
	}
    }

  for (int i=col1; i < col2; i++)
    {
      if (scol (i)->musical_b())
	{
	  Moment shortest_playing_len = scol(i)->shortest_playing_mom_;
	  if (! shortest_playing_len)
	    {
	      warning (_f ("can't find a ruling note at %s", 
	        scol (i)->when_mom ().str ()));
	      shortest_playing_len = 1;
	    }
	  if (! shortest)
	    {
	      warning (_f ("no minimum in measure at %s", 
		      scol (i)->when_mom ().str ()));
	      shortest = 1;
	    }
	  Moment delta_t = scol (i+1)->when_mom () - scol (i)->when_mom ();
	  Real k=  paper_l()->arithmetic_constant(shortest);
	  Real dist = paper_l()->length_mom_to_dist (shortest_playing_len, k);
	  dist *= (double)(delta_t / shortest_playing_len);


	  Spring sp;
	  sp.distance_f_ =  dist;
	  sp.item_l_drul_[LEFT] = scol (i);
	  sp.item_l_drul_[RIGHT] = scol (i+1);

	  meas_springs.push (sp);

	  /*
	    UGH. TODO: more
	    advanced spacing here.
	   */
	  Spring sp_orig (sp);

	  Item *r =  sp.item_l_drul_[RIGHT]->find_prebroken_piece (LEFT);
	  
	  if (r)
	    {
	      sp = sp_orig;
	      sp.item_l_drul_[RIGHT] =r ;
	      meas_springs.push (sp);
	    }
	}
    }
  return meas_springs;
}

Array<Spring>
Spacing_spanner::get_springs () const
{
  Array<Spring> springs;
  int last_break =0;
  for (int i=1; i < col_count (); i++)
    {
      if (scol (i)->breakable_b ())
	{
	  springs.concat (do_measure (last_break, i));
	  last_break  = i;
	}
    }
  return springs;
}


