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
#include "paper-score.hh"
#include "line-of-score.hh"

Spacing_spanner::Spacing_spanner ()
{
  set_elt_property ("transparent", SCM_BOOL_T);
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


  The algorithm is partly taken from :

  John S. Gourlay. ``Spacing a Line of Music,'' Technical Report
  OSU-CISRC-10/87-TR35, Department of Computer and Information
  Science, The Ohio State University, 1987.

  TOO HAIRY.
  
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

  Real non_musical_space_strength = paper_l ()->get_var ("breakable_column_space_strength");
  for (int i= col1; i < col2; i++)
    {
      Item * l = scol(i);
      Item * r = scol(i+1);
      Item * lb = l->find_broken_piece (RIGHT);
      Item * rb = r->find_broken_piece (LEFT);      

      Item* combinations[4][2]={{l,r}, {lb,r}, {l,rb},{lb,rb}};

      for (int j=0; j < 4; j++)
	{
	  Score_column * lc = dynamic_cast<Score_column*> (combinations[j][0]);
	  Score_column *rc = dynamic_cast<Score_column*> (combinations[j][1]);
	  if (!lc || !rc)
	    continue;

	  Spring s;
	  s.item_l_drul_[LEFT] = lc;
	  s.item_l_drul_[RIGHT] = rc;
	  
	  SCM hint = lc->get_elt_property ("extra-space");
	  SCM next_hint = rc->get_elt_property ("extra-space");
	  SCM stretch_hint = lc->get_elt_property ("stretch-distance");
	  SCM next_stretch_hint = rc->get_elt_property ("stretch-distance");	  

	  Real left_distance;
	  if (hint != SCM_UNDEFINED)
	    {
	      left_distance = gh_scm2double (gh_cdr (hint)); 
	    }
	  else if (!lc->musical_b() && i+1 < col_count())
	    {
	      left_distance= default_bar_spacing (lc,rc,shortest);
	    }
	  else if (lc->musical_b())
	    {
	      left_distance  = note_spacing (lc, rc, shortest);
	    }

	  s.distance_f_ = left_distance;

	  /*
	    Only do tight spaces *after* barlines (breakable columns),
	    not before.

	    We want the space before barline to be like the note
	    spacing in the measure.
	  */
	  if (lc->breakable_b () || lc->original_l_)
	    s.strength_f_ = non_musical_space_strength;
	  else if (!lc->musical_b ())
	    left_distance *= paper_l ()->get_var ("decrease_nonmus_spacing_factor");

	  
	  Real right_dist = 0.0;
	  if (next_hint != SCM_UNDEFINED)
	    {
	      right_dist += - gh_scm2double (gh_car (next_hint));
	    }
	  else
	    {
	      Interval ext (rc->extent (X_AXIS));
	      right_dist =  ext.empty_b() ? 0.0 : - ext [LEFT];
	    }

	  /*
	    don't want to create too much extra space for accidentals
	  */
	  if (lc->musical_b () && rc->musical_b ())
	    {
	      if (rc->get_elt_property ("contains-grace") == SCM_UNDEFINED)
		right_dist *= paper_l ()->get_var ("musical_to_musical_left_spacing_factor");
	    }

	  if (rc->musical_b () && rc->get_elt_property ("contains-grace") != SCM_UNDEFINED)
	    right_dist *= paper_l ()->get_var ("before_grace_spacing_factor");
 
 
	  s.distance_f_ = left_distance + right_dist;
	    
	  Real stretch_dist = 0.;
	  if (gh_number_p (stretch_hint))
	    stretch_dist += gh_scm2double (stretch_hint);
	  else
	    stretch_dist += left_distance;
	  
	  if (next_stretch_hint != SCM_UNDEFINED)
	    // see regtest spacing-tight
	    stretch_dist += - gh_scm2double (SCM_CAR (next_stretch_hint));
	  else
	    stretch_dist += right_dist;

	  if (s.distance_f_ <0)
	    programming_error("negative dist");
	  
	  if (stretch_dist == 0.0)
	    {
	      /*
		\bar "".  We give it 0 space, with high strength. 
	       */
	      s.strength_f_ = 20.0; 
	    }
	  else
	    s.strength_f_ /= stretch_dist;
	  
	  meas_springs.push (s);	
	}
    }

  return meas_springs;
}

/**
   Do something if breakable column has no spacing hints set.
 */
Real
Spacing_spanner::default_bar_spacing (Score_column *lc, Score_column *rc,
				      Moment shortest) const
{
  Real symbol_distance = lc->extent (X_AXIS)[RIGHT] ;
  Real durational_distance = 0;
  Moment delta_t =  rc->when_mom () - lc->when_mom () ;

  /*
		ugh should use shortest_playing distance
  */
  if (delta_t)
    {
      Real k=  paper_l()->arithmetic_constant (shortest);
      durational_distance =  paper_l()->length_mom_to_dist (delta_t,k);
    }

  return  symbol_distance >? durational_distance;
}


Real
Spacing_spanner::note_spacing (Score_column *lc, Score_column *rc, Moment shortest) const
{
  Moment shortest_playing_len = lc->shortest_playing_mom_;
  if (! shortest_playing_len)
    {
      programming_error ("Can't find a ruling note at " + lc->when_mom ().str ());
      shortest_playing_len = 1;
    }
  
  if (! shortest)
    {
      programming_error ("no minimum in measure at " + lc->when_mom ().str ());
      shortest = 1;
    }
  Moment delta_t = rc->when_mom () - lc->when_mom ();
  Real k=  paper_l()->arithmetic_constant(shortest);
  Real dist = paper_l()->length_mom_to_dist (shortest_playing_len, k);
  dist *= (double)(delta_t / shortest_playing_len);

  dist += stem_dir_correction (lc,rc);
  return dist;
}


/**
   Correct for optical illusions. See [Wanske] p. 138. The combination
   up-stem + down-stem should get extra space, the combination
   down-stem + up-stem less.

   This should be more advanced, since relative heights of the note
   heads also influence required correction.

   Also might not work correctly ico. multi voices or staff changing voices

   TODO: lookup correction distances?  More advanced correction?
   Possibly turn this off?

   This routine reads the DIR_LIST property of both its L and R arguments.
*/
Real
Spacing_spanner::stem_dir_correction (Score_column*l, Score_column*r) const
{
  SCM dl = l->get_elt_property ("dir-list");
  SCM dr = r->get_elt_property ("dir-list");
  if (dl == SCM_UNDEFINED || dr == SCM_UNDEFINED)
    return 0.0;


  if (scm_ilength (dl) != 1 && scm_ilength (dr) != 1)
    return 0.;

  dl = SCM_CAR(dl);
  dr = SCM_CAR(dr);

  assert (gh_number_p (dl) && gh_number_p(dr));
  int d1 = gh_scm2int (dl);
  int d2 = gh_scm2int (dr);

  if (d1 == d2)
    return 0.0;

  bool err = false;
  Real correction = 0.0;
  Real ssc = paper_l ()->get_var("stemSpacingCorrection");


  if (d1 && d2)
    {
      if (d1 == 1 && d2 == -1)
	correction = ssc;
      else if (d1 == -1 && d2 == 1)
	correction = -ssc;
      else
	err = true;
    }
  
  else
    err = true;

  if (err)
    programming_error ("Stem directions not set correctly for optical correction");
  return correction;
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





