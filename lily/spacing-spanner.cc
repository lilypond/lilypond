/*   
  spacing-spanner.cc --  implement Spacing_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "spacing-spanner.hh"
#include "paper-column.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "warn.hh"
#include "paper-score.hh"
#include "line-of-score.hh"
#include "misc.hh"

Spacing_spanner::Spacing_spanner ()
{
  set_extent_callback (0, X_AXIS);
  set_extent_callback (0, Y_AXIS);  
  set_elt_property ("transparent", SCM_BOOL_T);
}

/*

  The algorithm is partly taken from :

  John S. Gourlay. ``Spacing a Line of Music,'' Technical Report
  OSU-CISRC-10/87-TR35, Department of Computer and Information
  Science, The Ohio State University, 1987.

  TOO HAIRY.

  TODO: write comments 
  
 */
Array<Spring>
Spacing_spanner::do_measure (Link_array<Paper_column> cols) const
{
  Moment shortest;
  Moment mean_shortest;
  shortest.set_infinite (1);

  int n = 0;
  for (int i =0 ; i < cols.size (); i++)  
    {
      if (cols[i]->musical_b ())
	{
	  SCM  st = cols[i]->get_elt_property ("shortest-starter-duration");
	  Moment this_shortest = (*SMOB_TO_TYPE(Moment, st));
	  shortest = shortest <? this_shortest;
	  if (!mean_shortest.infty_b ())
	    {
	      n++;
	      mean_shortest += this_shortest;
	    }
	}
    }
  mean_shortest /= n;

  Array<Spring> meas_springs;

  Real non_musical_space_strength = paper_l ()->get_var ("breakable_column_space_strength");
  for (int i= 0; i < cols.size () - 1; i++)
    {
      Item * l = cols[i];
      Item * r = cols[i+1];
      Item * lb = l->find_prebroken_piece (RIGHT);
      Item * rb = r->find_prebroken_piece (LEFT);      

      Item* combinations[4][2]={{l,r}, {lb,r}, {l,rb},{lb,rb}};

      for (int j=0; j < 4; j++)
	{
	  Paper_column * lc = dynamic_cast<Paper_column*> (combinations[j][0]);
	  Paper_column *rc = dynamic_cast<Paper_column*> (combinations[j][1]);
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
	  if (gh_pair_p (hint))
	    {
	      left_distance = gh_scm2double (gh_cdr (hint)); 
	    }
	   // 2nd condition should be (i+1 < col_count()), ie. not the last column in score.  FIXME
	  else if (!lc->musical_b() && i+1 < cols.size ()) 
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
	  if (gh_pair_p (next_hint))
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
	      if (!to_boolean (rc->get_elt_property ("contains-grace")))
		right_dist *= paper_l ()->get_var ("musical_to_musical_left_spacing_factor");
	    }

	  if (rc->musical_b () && to_boolean (rc->get_elt_property ("contains-grace")))
	    right_dist *= paper_l ()->get_var ("before_grace_spacing_factor");
 
 	  s.distance_f_ = left_distance + right_dist;
	    
	  Real stretch_dist = 0.;
	  if (gh_number_p (stretch_hint))
	    stretch_dist += gh_scm2double (stretch_hint);
	  else
	    stretch_dist += left_distance;
	  
	  if (gh_pair_p (next_stretch_hint))
	    // see regtest spacing-tight
	    stretch_dist += - gh_scm2double (gh_car  (next_stretch_hint));
	  else
	    stretch_dist += right_dist;

	  if (s.distance_f_ <0)
	    {
	      programming_error("Negative dist, setting to 1.0 PT");
	      s.distance_f_ = 1.0;
	    }
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
Spacing_spanner::default_bar_spacing (Paper_column *lc, Paper_column *rc,
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
      durational_distance =  get_duration_space (delta_t, shortest);
    }

  return  symbol_distance >? durational_distance;
}


/**
  Get the measure wide constant for arithmetic spacing.

  @see
  John S. Gourlay. ``Spacing a Line of Music,'' Technical Report
  OSU-CISRC-10/87-TR35, Department of Computer and Information Science,
  The Ohio State University, 1987.

  */
Real
Spacing_spanner::get_duration_space (Moment d, Moment shortest) const
{
  Real log = log_2 (Moment (1,8) <? shortest);
  Real k=   paper_l ()->get_var ("arithmetic_basicspace")
    - log;
  
  return (log_2 (d) + k) * paper_l ()->get_var ("arithmetic_multiplier");
}


Real
Spacing_spanner::note_spacing (Paper_column *lc, Paper_column *rc, Moment shortest) const
{
  Moment shortest_playing_len = 0;
  SCM s = lc->get_elt_property ("shortest-playing-duration");
  //  SCM s = lc->get_elt_property ("mean-playing-duration");  
  if (SMOB_IS_TYPE_B(Moment, s))
    shortest_playing_len = *SMOB_TO_TYPE (Moment, s);

  
  if (! shortest_playing_len)
    {
      programming_error ("can't find a ruling note at " + lc->when_mom ().str ());
      shortest_playing_len = 1;
    }
  
  if (! shortest)
    {
      programming_error ("no minimum in measure at " + lc->when_mom ().str ());
      shortest = 1;
    }
  Moment delta_t = rc->when_mom () - lc->when_mom ();
  Real dist = get_duration_space (shortest_playing_len, shortest);
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

   Also might not work correctly in case of multi voices or staff
   changing voices

   TODO: lookup correction distances?  More advanced correction?
   Possibly turn this off?

   This routine reads the DIR-LIST property of both its L and R arguments.  */
Real
Spacing_spanner::stem_dir_correction (Paper_column*l, Paper_column*r) const
{
  SCM dl = l->get_elt_property ("dir-list");
  SCM dr = r->get_elt_property ("dir-list");
  if (dl == SCM_UNDEFINED || dr == SCM_UNDEFINED)
    return 0.0;


  if (scm_ilength (dl) != 1 && scm_ilength (dr) != 1)
    return 0.;

  dl = gh_car (dl);
  dr = gh_car (dr);

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

  Link_array<Paper_column> all (pscore_l_->line_l_->column_l_arr ()) ;

  int j = 0;

  for (int i = 1; i < all.size (); i++)
    {
      Paper_column* sc = dynamic_cast<Paper_column*> (all[i]);
      if (sc->breakable_b ())
        {
	  Link_array<Paper_column> measure (all.slice (j, i+1));	  
          springs.concat (do_measure (measure));
	  j = i;
        }
    }

  /*
    farewell, cruel world
   */
  ((Spacing_spanner*)this)->suicide ();
  
  return springs;
}





