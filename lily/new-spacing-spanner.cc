/*   
  spacing-spanner.cc --  implement Spacing_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "new-spacing-spanner.hh"
#include "paper-column.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "warn.hh"
#include "paper-score.hh"
#include "line-of-score.hh"
#include "misc.hh"
#include "separation-item.hh"


void
New_spacing_spanner::set_interface (Grob*me)
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
New_spacing_spanner::do_measure (Grob*me, Link_array<Grob> *cols) 
{
  Moment shortest_in_measure;

  /*
    space as if this duration  is present. 
   */
  Moment base_shortest_duration = *unsmob_moment (me->get_grob_property ("maximum-duration-for-spacing"));
  shortest_in_measure.set_infinite (1);

  for (int i = cols->size(); i--;)
    {
      if (gh_pair_p (cols->elem(i)->get_grob_property ("between-cols")))
	cols->del (i);
    }
  
  int n = 0;
  for (int i =0 ; i < cols->size (); i++)  
    {
      if (Paper_column::musical_b (cols->elem (i)))
	{
	  Moment *when = unsmob_moment (cols->elem (i)->get_grob_property  ("when"));

	  /*
	    ignore grace notes for shortest notes.
	   */
	  if (when && when->grace_part_)
	    continue;
	  
	  SCM  st = cols->elem (i)->get_grob_property ("shortest-starter-duration");
	  Moment this_shortest = *unsmob_moment (st);
	  shortest_in_measure = shortest_in_measure <? this_shortest;
	}
    }
  
  Array<Spring> springs;

  Item * first_col = 0;
  for (int i= 0; i < cols->size () - 1; i++)
    {
      Item * l = dynamic_cast<Item*> (cols->elem (i));

      if (!first_col && Paper_column::musical_b (l))
	first_col = l;

      Item * r =  dynamic_cast<Item*> (cols->elem (i+1));
      Paper_column * lc = dynamic_cast<Paper_column*> (l);
      Paper_column *rc = dynamic_cast<Paper_column*> (r);


#if 0
cout << "params for cols " << Paper_column::rank_i (l) << " " << Paper_column::rank_i (r) << endl;
      cout << " musical: " << Paper_column::musical_b (l) << " " << Paper_column::musical_b (r) << endl;
#endif
      
      if (!Paper_column::musical_b (l))
	{
	  breakable_column_spacing (l, r);

	  l = l->find_prebroken_piece (RIGHT);
	  if (l)
	    breakable_column_spacing (l,r);

	  continue ; 
	}
      
      Real note_space = note_spacing (me,lc, rc, shortest_in_measure <? base_shortest_duration);
      Real hinterfleisch = note_space;
      Real headwid = gh_scm2double (me->get_grob_property ("arithmetic-multiplier"));

      SCM seq  = lc->get_grob_property ("spacing-sequence");

      Moment dt = Paper_column::when_mom (r) - Paper_column::when_mom (l);
      
      /*
	hinterfleisch = hind-meat = amount of space following a note.

	
	We adjust the space following a note only if the next note
	happens after the current note (this is set in the grob
	property SPACING-SEQUENCE.  */

      Real stretch_distance = note_space;
      if (shortest_in_measure <= dt)
	{
	  /*
	    currently SPACING-SEQUENCE is set in
	    Separating_group_spanner::find_musical_sequences (), which
	    works neatly for one-voice-per staff, however,

	    it can't find out the actual duration of the notes on a
	    staff, so when putting tuplets and normal patterns it gets
	    confused, (ie. believes that < { c8 c8 } { d8 d8 d8 }*2/3
	    > contains 1/12 notes. ).

	    here we kludge, by checking if the distance we're spacing
	    for is less than the shortest note.

	    TODO:

	    Move SPACING-SEQUENCE detection into a voice
	    level-engraver --or-- make sure that every column has
	    access to the note head.

	  */
	  for (SCM s = seq; gh_pair_p (s); s = gh_cdr (s))
	    {
	      Grob *lm = unsmob_grob (gh_caar (s));
	      Grob *rm = unsmob_grob (gh_cdar (s));

	      // TODO; configgable.
	      hinterfleisch += -headwid + Separation_item::my_width (lm)[RIGHT] -
		0.5 * Separation_item::my_width (rm)[LEFT];


	      hinterfleisch += stem_dir_correction (me, l, r);
	    }

	  // ? why.
	  if (gh_pair_p (seq))
	    stretch_distance -= headwid;
	}      
      Spring s;
      s.distance_f_ = hinterfleisch;
      s.strength_f_ = 1 / stretch_distance;

      s.item_l_drul_[LEFT] = l;
      s.item_l_drul_[RIGHT] = r;

      s.add_to_cols();
      if (r->find_prebroken_piece (LEFT))
	{
	  s.item_l_drul_[RIGHT] = r->find_prebroken_piece(LEFT);
	  s.add_to_cols();
	}
    }

}

/*
  Read hints from L (todo: R) and generate springs.
 */
void
New_spacing_spanner::breakable_column_spacing (Item* l, Item *r)
{
  Spring s;

  Real break_dist = 0.0;
  SCM espace = l->get_grob_property ("extra-space");
  if (gh_pair_p (espace))
    break_dist += gh_scm2double (gh_cdr (espace));

  if (!break_dist)
    break_dist = 1.0;

  Real break_stretch = 0.0;

  // todo: naming of "distance"
  espace = l->get_grob_property ("stretch-distance");
  if (gh_pair_p (espace))
    break_stretch += gh_scm2double (gh_cdr (espace));

  if (!break_stretch)
    break_stretch = 1.0;
  
  s.distance_f_ = break_dist;
  s.strength_f_ = 1/break_stretch;
  s.item_l_drul_[LEFT] = l;
  s.item_l_drul_[RIGHT] = r;

  s.add_to_cols ();
}

/*
  Look at COLS, searching for columns that have 'regular-distance-to
  set. A sequence of columns that have this property set should have
  an equal distance (an equispaced run). Extract the projected
  distance from SPRINGS, and scale SPRINGS for the equispaced run, to the
  widest space necessary.


  TODO:
  
  -- inefficient code; maybe it is easier to twiddle with the springs
  after they've become grob properties (ie. have their
  minimum-distances set)

  -- does not adjust strength field of the springs very well: result
  awkward spacing at the start of a line. (?)

  -- will be confused when there are multiple equispaced runs in a measure.

  -- dealing with springs for line breaks is a little tricky; in any
  case, we will only space per measure.

  -- we scale to actual distances, not to optical effects. Eg. if the
  equispaced run contains optical corrections, then the scaling will
  cancel those.

  -- Regular_spacing_engraver doesn't mark the first column of the
  next bar, making the space before a barline too short, in this case


       x<- 16ths--> x(8th)
       x(8th)       x(8th)      <- equispaced run.      
  
*/

void
New_spacing_spanner::stretch_to_regularity (Grob *me,
					    Array<Spring> * springs,
					    Link_array<Grob> const & cols)
{
  /*
    Find the starting column of the run. REGULAR-DISTANCE-TO points
    back to a previous column, so we look ahead to find a column
    pointing back to the first one.
    
   */
  Grob    * first_regular_spaced_col = 0;
  for (int i = 0 ;  i <  cols.size () && !first_regular_spaced_col; i++)
    {
      SCM rdt = cols[i]->get_grob_property ("regular-distance-to");
      if (cols.find_l (dynamic_cast<Item*> (unsmob_grob (rdt))))
	first_regular_spaced_col = unsmob_grob (rdt);
    }
  for (int i = springs->size ();  i-- ;)
    springs->elem (i).set_to_cols ();
  
  int i;
  for (i = 0; i < springs->size ()
	 && springs->elem (i).item_l_drul_[RIGHT] != first_regular_spaced_col;
       i++)
    ;


  if (i==springs->size ())
    return ;
    
  Real maxdist = 0.0;
  Real dist  =0.0;
  Grob *last_col = first_regular_spaced_col;
  Grob *last_regular_spaced_col = first_regular_spaced_col;
  

  /*
    find the max distance for this run. 
   */
  for (int j = i;  j < springs->size (); j++)
    {
      Spring *s = &(springs->elem_ref (j));
      if (s->item_l_drul_[LEFT] != last_col)
	continue;
      
      dist += s->distance_f_;

      last_col = s->item_l_drul_[RIGHT];
      SCM rdt = last_col->get_grob_property ("regular-distance-to");
      if (unsmob_grob (rdt) == last_regular_spaced_col)
	{
	  maxdist = maxdist >? dist;
	  dist = 0.0;
	  last_regular_spaced_col = last_col;
	}

    }

  /*
    Scale the springs
   */
  dist =0.0;
  last_col =  first_regular_spaced_col;
  last_regular_spaced_col = first_regular_spaced_col;
  for (int j = i;   j < springs->size (); j++)
    {
      Spring *s = &springs->elem_ref (j);
      if (s->item_l_drul_[LEFT] != last_col)
	continue;
      dist += s->distance_f_;

      last_col = s->item_l_drul_[RIGHT];
      SCM rdt = last_col->get_grob_property ("regular-distance-to");
      if (unsmob_grob (rdt) == last_regular_spaced_col)
	{
	  do {
	    springs->elem_ref (i).distance_f_ *= maxdist / dist;
	    springs->elem_ref (i).strength_f_ *= dist / maxdist;	    
	  } while (i++ < j);
	  last_regular_spaced_col = last_col;
	  dist =0.0;
	}
    }
}

/**
   Do something if breakable column has no spacing hints set.
 */
Real
New_spacing_spanner::default_bar_spacing (Grob*me, Grob *lc, Grob *rc,
					  Moment shortest) 
{
  Real symbol_distance = lc->extent (lc,X_AXIS)[RIGHT] ;
  Real durational_distance = 0;
  Moment delta_t = Paper_column::when_mom (rc) - Paper_column::when_mom (lc);

  /*
		ugh should use shortest_playing distance
  */
  if (delta_t.to_bool ())
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
New_spacing_spanner::get_duration_space (Grob*me, Moment d, Moment shortest) 
{
  Real log =  log_2 (shortest.main_part_);
  Real k = gh_scm2double (me->get_grob_property ("arithmetic-basicspace"))
    - log;

  Rational compdur = d.main_part_ + d.grace_part_ /Rational (3);
  
  return (log_2 (compdur) + k) * gh_scm2double (me->get_grob_property ("arithmetic-multiplier"));
}


Real
New_spacing_spanner::note_spacing (Grob*me, Grob *lc, Grob *rc,
				   Moment shortest) 
{
  Moment shortest_playing_len = 0;
  SCM s = lc->get_grob_property ("shortest-playing-duration");


  if (unsmob_moment (s))
    shortest_playing_len = *unsmob_moment (s);
  
  if (! shortest_playing_len.to_bool ())
    {
      programming_error ("can't find a ruling note at " + Paper_column::when_mom (lc).str ());
      shortest_playing_len = 1;
    }
  
  if (! shortest.to_bool ())
    {
      programming_error ("no minimum in measure at " + Paper_column::when_mom (lc).str ());
      shortest = 1;
    }
  Moment delta_t = Paper_column::when_mom (rc) - Paper_column::when_mom (lc);
  Real dist = 0.0;

  if (delta_t.main_part_)
    {
      dist = get_duration_space (me, shortest_playing_len, shortest);
      dist *= (double) (delta_t.main_part_ / shortest_playing_len.main_part_);
    }
  else if (delta_t.grace_part_)
    {
      dist = get_duration_space (me, shortest, shortest);

      Real grace_fact = 1.0;
      SCM gf = me->get_grob_property ("grace-space-factor");
      if (gh_number_p (gf))
	grace_fact = gh_scm2double (gf);

      dist *= grace_fact; 
    }

#if 0
  /*
    TODO: figure out how to space grace notes.
   */

  dist *= 
    +  grace_fact * (double) (delta_t.grace_part_ / shortest_playing_len.main_part_);


  Moment *lm = unsmob_moment (lc->get_grob_property ("when"));
  Moment *rm = unsmob_moment (rc->get_grob_property ("when"));

  if (lm && rm)
    {
      if (lm->grace_part_ && rm->grace_part_)
	dist *= 0.5;
      else if (!rm->grace_part_ && lm->grace_part_)
	dist *= 0.7;
    }
#endif
  
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
New_spacing_spanner::stem_dir_correction (Grob*me, Grob*l, Grob*r) 
{
  SCM dl = l->get_grob_property ("dir-list");
  SCM dr = r->get_grob_property ("dir-list");
  
  if (scm_ilength (dl) != 1 || scm_ilength (dr) != 1)
    return 0.;

  dl = gh_car (dl);
  dr = gh_car (dr);

  assert (gh_number_p (dl) && gh_number_p (dr));
  int d1 = gh_scm2int (dl);
  int d2 = gh_scm2int (dr);

  if (d1 == d2)
    return 0.0;


  Real correction = 0.0;
  Real ssc = gh_scm2double (me->get_grob_property ("stem-spacing-correction"));

  if (d1 && d2 && d1 * d2 == -1)
    {
      correction = d1 * ssc;
    }
  else
    programming_error ("Stem directions not set correctly for optical correction");
  return correction;
}
  

MAKE_SCHEME_CALLBACK (New_spacing_spanner, set_springs,1);
SCM
New_spacing_spanner::set_springs (SCM smob)
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
          do_measure (me, &measure);
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
