/*   
  spacing-spanner.cc -- implement Spacing_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "line-of-score.hh"
#include "paper-score.hh"
#include "paper-column.hh"
#include "item.hh"
#include "moment.hh"
#include "note-spacing.hh"
#include "misc.hh"
#include "warn.hh"

/*
  paper-column:

    right-neighbors = List of spacing-wish grobs that are close to the
    current column.


    left-neighbors = idem

    (TODO: property-doc these!)

  Don't be confused by right-items: each spacing wish can also contain
  a number of items, with which a spacing constraint may be kept. It's
  a little baroque, but it might come in handy later on?
    
 */

class Third_spacing_spanner
{
public:
  static Real default_bar_spacing (Grob*,Grob*,Grob*,Moment)  ;
  static Real note_spacing (Grob*,Grob*,Grob*,Moment)  ;
  static Real get_duration_space (Grob*,Moment dur, Moment shortest) ;
  
  static   void breakable_column_spacing (Item* l, Item *r);
  static  void find_loose_columns () {}
  static  void prune_loose_colunms (Link_array<Grob> *cols);
  static void find_loose_columns (Link_array<Grob> cols);
  static void set_explicit_neighbor_columns (Link_array<Grob> cols);
  static void set_implicit_neighbor_columns (Link_array<Grob> cols);
  static void do_measure (Grob*me,Link_array<Grob> *cols);
  DECLARE_SCHEME_CALLBACK (set_springs, (SCM ));
};



/*
  Return whether COL is fixed to its neighbors by some kind of spacing
  constraint.
*/
static bool
loose_column (Grob *l, Grob *c, Grob *r) 
{
  SCM rns = c->get_grob_property ("right-neighbors");
  SCM lns = c->get_grob_property ("left-neighbors");

 /*
    If this column doesn't have a proper neighbor, we should really
    make it loose, but spacing it correctly is more than we can
    currently can handle.

    (this happens in the following situation:

       |
       |    clef G 
      *

       |               |      ||
       |               |      || 
      O               O       ||


    the column containing the clef is really loose, and should be
    attached right to the first column, but that is a lot of work for
    such a borderline case.

    )
    
  */  
  if (!gh_pair_p (lns) || !gh_pair_p (rns))
    return false;

  Item * l_neighbor = dynamic_cast<Item*>  (unsmob_grob (gh_car (lns)));
  Item * r_neighbor = dynamic_cast<Item*>  (unsmob_grob (gh_car (rns)));

  if (!l_neighbor || !r_neighbor)
    return false;

  l_neighbor = l_neighbor->column_l();
  r_neighbor = dynamic_cast<Item*> (Note_spacing::right_column  (r_neighbor));

  if (l == l_neighbor && r == r_neighbor)
    return false;

  if (!l_neighbor || !r_neighbor)
    return false;

  /*
    Only declare loose if the bounds make a little sense.  This means
    some cases (two isolated, consecutive clef changes) won't be
    nicely folded, but hey, then don't do that.
  */
  if( (Paper_column::musical_b (l_neighbor) || Item::breakable_b (l_neighbor))
      && (Paper_column::musical_b (r_neighbor) || Item::breakable_b (r_neighbor)))
    {
      return true;
    }


  /*
    If in doubt: we're not loose; the spacing engine should space for
    it, risking suboptimal spacing.

    (Otherwise, we might risk core dumps, and other weird stuff.)


  */
  return false;
}

/*
  Remove columns that are not tightly fitting from COLS. In the
  removed columns, set 'between-cols to the columns where it is in
  between.
*/
void
Third_spacing_spanner::prune_loose_colunms (Link_array<Grob> *cols)
{
  Link_array<Grob> newcols;
  
  for (int i=0; i < cols->size ();  i++)
    {
      if (Item::breakable_b (cols->elem(i)) || Paper_column::musical_b (cols->elem (i)))
	{
	  newcols.push (cols->elem(i));
	  continue;
	}

      Grob *c = cols->elem(i);
      if (loose_column (cols->elem (i-1), c, cols->elem (i+1)))
	{
	  SCM lns = c->get_grob_property ("left-neighbors");
	  lns = gh_pair_p (lns) ? gh_car (lns) : SCM_BOOL_F;

	  SCM rns = c->get_grob_property ("right-neighbors");
	  rns = gh_pair_p (rns) ? gh_car (rns) : SCM_BOOL_F;

	  /*
	    Either object can be non existent, if the score ends
	    prematurely.
	   */
	  rns = gh_car (unsmob_grob (rns)->get_grob_property ("right-items"));
	  c->set_grob_property ("between-cols", gh_cons (lns,
							 rns));


	  /*
	    TODO: we should have distance constraints for loose
	    columns anyway, and the placement could be improved. Clefs
	    easily run into their neigbhors when folded into too
	    little space.
	  */
	}
      else
	{
	  newcols.push (c);
	}
    }

  *cols = newcols;
}

/*
  Set neighboring columns determined by the spacing-wishes grob property.  
*/
void
Third_spacing_spanner::set_explicit_neighbor_columns (Link_array<Grob> cols)
{
  for (int i=0; i < cols.size(); i++)
    {
      SCM right_neighbors = SCM_EOL;
      int min_rank = 100000;	// inf.


      SCM wishes=  cols[i]->get_grob_property ("spacing-wishes");
      for (SCM s =wishes; gh_pair_p (s); s = gh_cdr (s))
	{
	  Item * wish = dynamic_cast<Item*> (unsmob_grob (gh_car (s)));

	  Item * lc = wish->column_l ();
	  Grob * right = Note_spacing::right_column (wish);

	  if (!right)
	    continue;

	  Item * rc = dynamic_cast<Item*> (right);

	  int right_rank = Paper_column::rank_i (rc);
	  int left_rank = Paper_column::rank_i (lc);	  

	  /*
	    update the left column.
	   */
	  if (right_rank <= min_rank)
	    {
	      if (right_rank < min_rank)
		right_neighbors  =SCM_EOL;
	      
	      min_rank = right_rank;
	      right_neighbors = gh_cons (wish->self_scm (), right_neighbors);
	    }

	  /*
	    update the right column of the wish.
	   */
	  int maxrank = 0;
	  SCM left_neighs = rc->get_grob_property ("left-neighbors");
	  if (gh_pair_p (left_neighs)
	      && unsmob_grob (gh_car (left_neighs)))
	    {
	      Item * it = dynamic_cast<Item*> (unsmob_grob (gh_car (left_neighs)));
	      maxrank = Paper_column::rank_i (it->column_l());
	    }

	  if (left_rank >= maxrank)
	    {
	      if (left_rank > maxrank)
		left_neighs = SCM_EOL;

	      left_neighs = gh_cons (wish->self_scm (), left_neighs);
	      rc->set_grob_property ("left-neighbors", right_neighbors);
	    }
	}

      if (gh_pair_p (right_neighbors))
	{
	  cols[i]->set_grob_property ("right-neighbors", right_neighbors);
	}
    }
}

/*
  Set neighboring columns that have no left/right-neighbor set
  yet. Only do breakable non-musical columns, and musical columns. 
*/
void
Third_spacing_spanner::set_implicit_neighbor_columns (Link_array<Grob> cols)
{
  for (int i = 0; i < cols.size (); i++)
    {
      Item * it = dynamic_cast<Item*>(cols[i]);
      if (!Item::breakable_b (it) && !Paper_column::musical_b (it))
	continue;

      // it->breakable || it->musical
      SCM ln = cols[i] ->get_grob_property ("left-neighbors");
      if (!gh_pair_p (ln) && i ) 
	{
	  cols[i]->set_grob_property ("left-neighbors", cols[i-1]->self_scm());
	}

      SCM rn = cols[i] ->get_grob_property ("right-neighbors");
      if (!gh_pair_p (rn) && i < cols.size () - 1) 
	{
	  cols[i]->set_grob_property ("right-neighbors", cols[i + 1]->self_scm());
	}
    }
}


MAKE_SCHEME_CALLBACK (Third_spacing_spanner, set_springs,1);
SCM
Third_spacing_spanner::set_springs (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Link_array<Grob> all (me->pscore_l_->line_l_->column_l_arr ()) ;

  set_explicit_neighbor_columns (all);
  prune_loose_colunms (&all);
  set_implicit_neighbor_columns (all);
  
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

  return SCM_UNSPECIFIED;
}


void
Third_spacing_spanner::do_measure (Grob*me, Link_array<Grob> *cols) 
{
  Moment shortest_in_measure;

  /*
    space as if this duration  is present. 
  */
  Moment base_shortest_duration = *unsmob_moment (me->get_grob_property ("maximum-duration-for-spacing"));
  shortest_in_measure.set_infinite (1);

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

  for (int i= 0; i < cols->size () - 1; i++)
    {
      Item * l = dynamic_cast<Item*> (cols->elem (i));
      Item * r =  dynamic_cast<Item*> (cols->elem (i+1));

      Paper_column * lc = dynamic_cast<Paper_column*> (l);
      Paper_column * rc = dynamic_cast<Paper_column*> (r);

      if (!Paper_column::musical_b (l))
	{
	  breakable_column_spacing (l, r);

	  /*
	    
	    The case that the right part is broken as well is rather
	    rare, but it is possible, eg. with a single empty measure,
	    or if one staff finishes a tad earlier than the rest.
	    
	   */
	  Item *lb = l->find_prebroken_piece (RIGHT);
	  Item *rb = r->find_prebroken_piece (LEFT);
	  
	  if (lb)
	    breakable_column_spacing (lb,r);

	  if (rb)
	    breakable_column_spacing (l, rb);
	  if (lb && rb)
	    breakable_column_spacing (lb, rb);
	  
	  continue ; 
	}
      
      Real note_space = note_spacing (me,lc, rc, shortest_in_measure <? base_shortest_duration);
      Real hinterfleisch = note_space;
      Real headwid = gh_scm2double (me->get_grob_property ("arithmetic-multiplier"));

      SCM seq  = lc->get_grob_property ("right-neighbors");

      /*
	hinterfleisch = hind-meat = amount of space following a note.

	
	We adjust the space following a note only if the next note
	happens after the current note (this is set in the grob
	property SPACING-SEQUENCE.  */

      Real stretch_distance = note_space;

      hinterfleisch = -1.0;
      Real max_factor = 0.0;
      for (SCM s = seq; gh_pair_p (s); s = ly_cdr (s))
	{
	  Grob * wish = unsmob_grob (gh_car (s));

	  if (Note_spacing::left_column (wish) != lc
	      || Note_spacing::right_column (wish) != rc)
	    continue;

	  /*
	    This is probably a waste of time in the case of polyphonic
	    music.  */
	  if (Note_spacing::has_interface (wish))
	    {
	      hinterfleisch = hinterfleisch >?
		( - headwid +

		  (note_space + Note_spacing::get_spacing (wish))
		  *gh_scm2double (wish->get_grob_property ("space-factor"))

		  + Note_spacing::stem_dir_correction (wish));
	    }
	}

      if (hinterfleisch < 0)
	{
	  // maybe should issue a programming error.
	  hinterfleisch = note_space;
	}
      else
	stretch_distance -= headwid; // why?

      if (max_factor == 0.0)
	max_factor = 1.0; 
      
      Spring s;
      s.distance_f_ = max_factor *  hinterfleisch;
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
Third_spacing_spanner::breakable_column_spacing (Item* l, Item *r)
{
  Spring s;

  Real break_dist = 0.0;
  SCM espace = l->get_grob_property ("extra-space");
  if (gh_pair_p (espace))
    break_dist += gh_scm2double (ly_cdr (espace));

  if (!break_dist)
    break_dist = 1.0;

  Real break_stretch = 0.0;

  // todo: naming of "distance"
  espace = l->get_grob_property ("stretch-distance");
  if (gh_pair_p (espace))
    break_stretch += gh_scm2double (ly_cdr (espace));

  if (!break_stretch)
    break_stretch = 1.0;
  
  s.distance_f_ = break_dist;
  s.strength_f_ = 1/break_stretch;
  s.item_l_drul_[LEFT] = l;
  s.item_l_drul_[RIGHT] = r;

  s.add_to_cols ();
}


/**
  Get the measure wide ant for arithmetic spacing.

  @see
  John S. Gourlay. ``Spacing a Line of Music,'' Technical Report
  OSU-CISRC-10/87-TR35, Department of Computer and Information Science,
  The Ohio State University, 1987.

  */
Real
Third_spacing_spanner::get_duration_space (Grob*me, Moment d, Moment shortest) 
{
  Real log =  log_2 (shortest.main_part_);
  Real k = gh_scm2double (me->get_grob_property ("arithmetic-basicspace"))
    - log;

  Rational compdur = d.main_part_ + d.grace_part_ /Rational (3);
  
  return (log_2 (compdur) + k) * gh_scm2double (me->get_grob_property ("arithmetic-multiplier"));
}


Real
Third_spacing_spanner::note_spacing (Grob*me, Grob *lc, Grob *rc,
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

