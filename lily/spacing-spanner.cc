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
Spacing_spanner::set_interface (Grob*me)
{
  me->set_extent_callback (SCM_EOL, X_AXIS);
  me->set_extent_callback (SCM_EOL, Y_AXIS) ; 
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
Spacing_spanner::do_measure (Grob*me, Link_array<Grob> cols) 
{
  Moment shortest;
  Moment mean_shortest;

  /*
    space as if this duration  is present. 
   */
  Moment base_shortest_duration = *unsmob_moment (me->get_grob_property ("maximum-duration-for-spacing"));
  shortest.set_infinite (1);

  int n = 0;
  for (int i =0 ; i < cols.size (); i++)  
    {
      if (dynamic_cast<Paper_column*> (cols[i])->musical_b ())
	{
	  SCM  st = cols[i]->get_grob_property ("shortest-starter-duration");
	  Moment this_shortest = *unsmob_moment(st);
	  shortest = shortest <? this_shortest;
	  if (!mean_shortest.infty_b ())
	    {
	      n++;
	      mean_shortest += this_shortest;
	    }
	}
    }
  mean_shortest /= n;


  for (int i= 0; i < cols.size () - 1; i++)
    {
      Item * l = dynamic_cast<Item*> (cols[i]);
      Item * r =  dynamic_cast<Item*> (cols[i+1]);
      Item * lb = dynamic_cast<Item*> ( l->find_prebroken_piece (RIGHT));
      Item * rb = dynamic_cast<Item*> ( r->find_prebroken_piece (LEFT));

      Item* combinations[4][2]={{l,r}, {lb,r}, {l,rb},{lb,rb}};


      /*
	left refers to the space that is associated with items of the left column, so you have

	  LC  <- left_space -><- right_space -> RC
              <-    total space              ->
	      

        typically, right_space is non-zero when there are
        accidentals in RC
	  
       */
      for (int j=0; j < 4; j++)
	{
	  Paper_column * lc = dynamic_cast<Paper_column*> (combinations[j][0]);
	  Paper_column *rc = dynamic_cast<Paper_column*> (combinations[j][1]);
	  if (!lc || !rc)
	    continue;

	  Spring s;
	  s.item_l_drul_[LEFT] = lc;
	  s.item_l_drul_[RIGHT] = rc;
	  
	  SCM hint = lc->get_grob_property ("extra-space");
	  SCM next_hint = rc->get_grob_property ("extra-space");
	  SCM stretch_hint = lc->get_grob_property ("stretch-distance");
	  SCM next_stretch_hint = rc->get_grob_property ("stretch-distance");	  

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
	  SCM sfac =lc->get_grob_property ("space-factor");
	  if (Item::breakable_b (lc) || lc->original_l_)
	    {
	      s.strength_f_ =
		gh_scm2double (lc->get_grob_property ("column-space-strength"));
	    }
	  else if (gh_number_p (sfac))
	    left_distance *= gh_scm2double (sfac);

	  
	  Real right_dist = 0.0;
	  if (gh_pair_p (next_hint))
	    {
	      right_dist += - gh_scm2double (gh_car (next_hint));
	    }
	  else
	    {
	      Interval ext (rc->extent (rc, X_AXIS));
	      right_dist =  ext.empty_b() ? 0.0 : - ext [LEFT];
	    }

	  /*
	    don't want to create too much extra space for accidentals
	  */
	  if (rc->musical_b ())
	   {
	      if (to_boolean (rc->get_grob_property ("contains-grace")))
		right_dist *= gh_scm2double (rc->get_grob_property ("before-grace-spacing-factor")); // fixme.
	      else
		right_dist *= gh_scm2double (lc->get_grob_property ("before-musical-spacing-factor"));
	   }

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
Spacing_spanner::default_bar_spacing (Grob*me, Grob *lc, Grob *rc,
				      Moment shortest) 
{
  Real symbol_distance = lc->extent (lc,X_AXIS)[RIGHT] ;
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
Spacing_spanner::get_duration_space (Grob*me, Moment d, Moment shortest) 
{
  Real log =  log_2 (shortest);
  Real k = gh_scm2double (me->get_grob_property  ("arithmetic-basicspace"))
    - log;
  
  return (log_2 (d) + k) * gh_scm2double (me->get_grob_property ("arithmetic-multiplier"));
}


Real
Spacing_spanner::note_spacing (Grob*me, Grob *lc, Grob *rc,
			       Moment shortest) 
{
  Moment shortest_playing_len = 0;
  SCM s = lc->get_grob_property ("shortest-playing-duration");

  //  SCM s = lc->get_grob_property ("mean-playing-duration");  
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
Spacing_spanner::stem_dir_correction (Grob*me, Grob*l, Grob*r) 
{
  SCM dl = l->get_grob_property ("dir-list");
  SCM dr = r->get_grob_property ("dir-list");
  
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
  Real ssc = gh_scm2double (me->get_grob_property("stem-spacing-correction"));

  if (d1 && d2 && d1 * d2 == -1)
    {
      correction = d1 * ssc;
    }
  else
    programming_error ("Stem directions not set correctly for optical correction");
  return correction;
}
  

MAKE_SCHEME_CALLBACK(Spacing_spanner, set_springs,1);
SCM
Spacing_spanner::set_springs (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Link_array<Grob> all (me->pscore_l_->line_l_->column_l_arr ()) ;

  int j = 0;

  for (int i = 1; i < all.size (); i++)
    {
      Grob *sc = all[i];
      if (Item::breakable_b (sc))
        {
	  Link_array<Grob> measure (all.slice (j, i+1));	  
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



/*
  maximum-duration-for-spacing
From: bf250@freenet.carleton.ca (John Sankey)
To: gnu-music-discuss@gnu.org
Subject: note spacing suggestion
Date: Mon, 10 Jul 2000 11:28:03 -0400 (EDT)

Currently, Lily spaces notes by starting with a basic distance,
arithmetic_multiplier, which it applies to the minimum duration note
of the bar. Then she adds a logarithmic increment, scaled from
arithmetic_basicspace, for longer notes. (Then, columns are aligned
and justified.) Fundamentally, this matches visual spacing to musical
weight and works well.

A lot of the time in music, I see a section basically in melodic
notes that occasionally has a rapid ornamental run (scale). So, there
will be a section in 1/4 notes, then a brief passage in 1/32nds, then
a return to long notes. Currently, Lily gives the same horizontal
space to the 1/32nd notes in their bar (even if set in small size as
is commonly done for cadenzii) as she gives to 1/4 notes in bars
where 1/4 note is the minimum duration. The resulting visual weight
does not match the musical weight over the page.

Looking at the music I am typesetting, I feel that Lily's spacing
could be significantly improved if, with no change in the basic
method used, arithmetic_multiplier could be applied referred to the
same duration throughout a piece. Of course, the current method
should be retained for those who have already set music in it, so I
suggest a property called something like arithmetic_base=16 to fix
1/16 duration as the reference for arithmetic_multiplier; the default
would be a dynamic base is it is now.

Does anyone else feel that this would be a useful improvement for
their music? (Of course, if arithmetic_multiplier became a regular
property, this could be used to achieve a similar result by
tweaking.)
  
 */
