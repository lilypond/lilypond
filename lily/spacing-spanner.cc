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

void
Spacing_spanner::set_interface (Score_element*me)
{
  me->set_extent_callback (0, X_AXIS);
  me->set_extent_callback (0, Y_AXIS);  
}

/*

  The algorithm is partly taken from :

  John S. Gourlay. ``Spacing a Line of Music,'' Technical Report
  OSU-CISRC-10/87-TR35, Department of Computer and Information
  Science, The Ohio State University, 1987.

  TOO HAIRY.

  TODO: write comments 
  
 */
void
Spacing_spanner::do_measure (Score_element*me, Link_array<Score_element> cols) 
{
  Moment shortest;
  Moment mean_shortest;

  /*
    space as if this duration  is present. 
   */
  Moment base_shortest_duration = *unsmob_moment (me->get_elt_property ("maximum-duration-for-spacing"));
  shortest.set_infinite (1);

  int n = 0;
  for (int i =0 ; i < cols.size (); i++)  
    {
      if (dynamic_cast<Paper_column*> (cols[i])->musical_b ())
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

  Real non_musical_space_strength = me->paper_l ()->get_var ("breakable_column_space_strength");
  for (int i= 0; i < cols.size () - 1; i++)
    {
      Item * l = dynamic_cast<Item*> (cols[i]);
      Item * r =  dynamic_cast<Item*> (cols[i+1]);
      Item * lb = dynamic_cast<Item*> ( l->find_prebroken_piece (RIGHT));
      Item * rb = dynamic_cast<Item*> ( r->find_prebroken_piece (LEFT));

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
	      left_distance= default_bar_spacing (me,lc,rc,shortest <? base_shortest_duration);
	    }
	  else if (lc->musical_b())
	    {
	      left_distance  = note_spacing (me,lc, rc, shortest <? base_shortest_duration);
	    }

	  s.distance_f_ = left_distance;

	  /*
	    Only do tight spaces *after* barlines (breakable columns),
	    not before.

	    We want the space before barline to be like the note
	    spacing in the measure.
	  */
	  if (Item::breakable_b (lc) || lc->original_l_)
	    s.strength_f_ = non_musical_space_strength;
	  else if (!lc->musical_b ())
	    left_distance *= me->paper_l ()->get_var ("decrease_nonmus_spacing_factor");

	  
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
		right_dist *= me->paper_l ()->get_var ("musical_to_musical_left_spacing_factor");
	    }

	  if (rc->musical_b () && to_boolean (rc->get_elt_property ("contains-grace")))
	    right_dist *= me->paper_l ()->get_var ("before_grace_spacing_factor");
 
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
	  
	  s.add_to_cols ();
	}
    }
  
}

/**
   Do something if breakable column has no spacing hints set.
 */
Real
Spacing_spanner::default_bar_spacing (Score_element*me, Score_element *lc, Score_element *rc,
				      Moment shortest) 
{
  Real symbol_distance = lc->extent (X_AXIS)[RIGHT] ;
  Real durational_distance = 0;
  Moment delta_t = Paper_column::when_mom (rc) - Paper_column::when_mom (lc);

  /*
		ugh should use shortest_playing distance
  */
  if (delta_t)
    {
      durational_distance =  get_duration_space (me, delta_t, shortest);
    }

  return  symbol_distance >? durational_distance;
}


/**
  Get the measure wide ant for arithmetic spacing.

  @see
  John S. Gourlay. ``Spacing a Line of Music,'' Technical Report
  OSU-CISRC-10/87-TR35, Department of Computer and Information Science,
  The Ohio State University, 1987.

  */
Real
Spacing_spanner::get_duration_space (Score_element*me, Moment d, Moment shortest) 
{
  Real log =  log_2 (shortest);
  Real k=   me->paper_l ()->get_var ("arithmetic_basicspace")
    - log;
  
  return (log_2 (d) + k) * me->paper_l ()->get_var ("arithmetic_multiplier");
}


Real
Spacing_spanner::note_spacing (Score_element*me, Score_element *lc, Score_element *rc,
			       Moment shortest) 
{
  Moment shortest_playing_len = 0;
  SCM s = lc->get_elt_property ("shortest-playing-duration");

  //  SCM s = lc->get_elt_property ("mean-playing-duration");  
  if (unsmob_moment (s))
    shortest_playing_len = *unsmob_moment(s);
  
  if (! shortest_playing_len)
    {
      programming_error ("can't find a ruling note at " + Paper_column::when_mom (lc).str ());
      shortest_playing_len = 1;
    }
  
  if (! shortest)
    {
      programming_error ("no minimum in measure at " + Paper_column::when_mom (lc).str ());
      shortest = 1;
    }
  Moment delta_t = Paper_column::when_mom (rc) - Paper_column::when_mom (lc);
  Real dist = get_duration_space (me, shortest_playing_len, shortest);
  dist *= (double)(delta_t / shortest_playing_len);

  /*
    UGH: KLUDGE!
  */
  
  if (delta_t > Moment (1,32))
    dist += stem_dir_correction (me, lc,rc);
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

   TODO: have to check wether the stems are in the same staff.

   This routine reads the DIR-LIST property of both its L and R arguments.  */
Real
Spacing_spanner::stem_dir_correction (Score_element*me, Score_element*l, Score_element*r) 
{
  SCM dl = l->get_elt_property ("dir-list");
  SCM dr = r->get_elt_property ("dir-list");
  
  if (scm_ilength (dl) != 1 || scm_ilength (dr) != 1)
    return 0.;

  dl = gh_car (dl);
  dr = gh_car (dr);

  assert (gh_number_p (dl) && gh_number_p(dr));
  int d1 = gh_scm2int (dl);
  int d2 = gh_scm2int (dr);

  if (d1 == d2)
    return 0.0;


  Real correction = 0.0;
  Real ssc = me->paper_l ()->get_var("stemSpacingCorrection");


  if (d1 && d2 && d1 * d2 == -1)
    {
      correction = d1 * ssc;
    }
  else
    programming_error ("Stem directions not set correctly for optical correction");
  return correction;
}
  

MAKE_SCHEME_CALLBACK(Spacing_spanner, set_springs);
SCM
Spacing_spanner::set_springs (SCM smob)
{
  Score_element *me = unsmob_element (smob);
  Link_array<Score_element> all (me->pscore_l_->line_l_->column_l_arr ()) ;

  int j = 0;

  for (int i = 1; i < all.size (); i++)
    {
      Score_element *sc = all[i];
      if (Item::breakable_b (sc))
        {
	  Link_array<Score_element> measure (all.slice (j, i+1));	  
          do_measure (me, measure);
	  j = i;
        }
    }

  /*
    farewell, cruel world
   */
  me->suicide ();
  return SCM_UNSPECIFIED;
}





