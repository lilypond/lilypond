/*   
  spacing-spanner.cc -- implement Spacing_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include <math.h>
#include <stdio.h>

#include "line-of-score.hh"
#include "paper-score.hh"
#include "paper-column.hh"
#include "item.hh"
#include "moment.hh"
#include "note-spacing.hh"
#include "misc.hh"
#include "warn.hh"
#include "staff-spacing.hh"
#include "spring.hh"
#include "paper-column.hh"
#include "spaceable-grob.hh"

/*
  paper-column:

  Don't be confused by right-items: each spacing wish can also contain
  a number of items, with which a spacing constraint may be kept. It's
  a little baroque, but it might come in handy later on?
    
 */
class Spacing_spanner
{
public:
  static Real default_bar_spacing (Grob*,Grob*,Grob*,Moment);
  static Real note_spacing (Grob*,Grob*,Grob*,Moment, bool*);
  static Real get_duration_space (Grob*,Moment dur, Rational shortest, bool*);
  static Rational find_shortest (Link_array<Grob> const &);  
  static void breakable_column_spacing (Item* l, Item *r);
  static void find_loose_columns () {}
  static void prune_loose_colunms (Grob*,Link_array<Grob> *cols, Rational);
  static void find_loose_columns (Link_array<Grob> cols);
  static void set_explicit_neighbor_columns (Link_array<Grob> cols);
  static void set_implicit_neighbor_columns (Link_array<Grob> cols);
  static void do_measure (Rational, Grob*me,Link_array<Grob> *cols);
  static void musical_column_spacing (Grob*,Item*,Item*, Real, Rational); 
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
    such a borderline case.)
    
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
  if ((Paper_column::musical_b (l_neighbor) || Item::breakable_b (l_neighbor))
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
Spacing_spanner::prune_loose_colunms (Grob*me,Link_array<Grob> *cols, Rational shortest)
{
  Link_array<Grob> newcols;
  Real increment = gh_scm2double (me->get_grob_property ("spacing-increment"));
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
	    Set distance constraints for loose columns
	  */
	  Drul_array<Grob*> next_door;
	  next_door[LEFT] =cols->elem (i - 1);
	  next_door[RIGHT] =cols->elem (i + 1);	  
	  Direction d = LEFT;
	  Drul_array<Real> dists(0,0);

	  do
	    {
	      dists[d] = 0.0;
	      Item *lc = dynamic_cast<Item*> ((d == LEFT)  ? next_door[LEFT] : c);
	      Item *rc = dynamic_cast<Item*> (d == LEFT  ? c : next_door[RIGHT]);

	      for (SCM s = lc->get_grob_property ("spacing-wishes");
		   gh_pair_p (s); s = gh_cdr (s))
		{
		  Grob *sp = unsmob_grob (gh_car (s));
		  if (Note_spacing::left_column (sp) != lc
		      || Note_spacing::right_column (sp) != rc)
		    continue;

		  Real space, fixed;
		  fixed = 0.0;
		  bool dummy;

		  if (d == LEFT)
		    {
		      /*
			The note spacing should be taken from the musical
			columns.
		    
		      */
		      Real base = note_spacing (me, lc, rc, shortest, &dummy);
		      Note_spacing::get_spacing (sp, rc, base, increment, &space, &fixed);

		      space -= increment; 
		  
		      dists[d] = dists[d] >? space;
		    }
		  else
		    {
		      Real space, fixed_space;
		      Staff_spacing::get_spacing_params (sp,
							 &space, &fixed_space);

		      dists[d] = dists[d] >? fixed_space;
		    }
		  
		}
	    }
	  while (flip (&d) != LEFT);

	  Rod r;
	  r.distance_f_ = dists[LEFT] + dists[RIGHT];
	  r.item_l_drul_[LEFT] = dynamic_cast<Item*> (cols->elem(i-1));
	  r.item_l_drul_[RIGHT] = dynamic_cast<Item*> (cols->elem (i+1));

	  r.add_to_cols ();
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
Spacing_spanner::set_explicit_neighbor_columns (Link_array<Grob> cols)
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
Spacing_spanner::set_implicit_neighbor_columns (Link_array<Grob> cols)
{
  for (int i = 0; i < cols.size (); i++)
    {
      Item * it = dynamic_cast<Item*>(cols[i]);
      if (!Item::breakable_b (it) && !Paper_column::musical_b (it))
	continue;

      // it->breakable || it->musical

      /*
	sloppy with typnig left/right-neighbors should take list, but paper-column found instead.
       */
      SCM ln = cols[i] ->get_grob_property ("left-neighbors");
      if (!gh_pair_p (ln) && i ) 
	{
	  cols[i]->set_grob_property ("left-neighbors", gh_cons (cols[i-1]->self_scm(), SCM_EOL));
	}

      SCM rn = cols[i] ->get_grob_property ("right-neighbors");
      if (!gh_pair_p (rn) && i < cols.size () - 1) 
	{
	  cols[i]->set_grob_property ("right-neighbors", gh_cons (cols[i + 1]->self_scm(), SCM_EOL));
	}
    }
}


MAKE_SCHEME_CALLBACK (Spacing_spanner, set_springs,1);
SCM
Spacing_spanner::set_springs (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Link_array<Grob> all (me->pscore_l_->line_l_->column_l_arr ()) ;

  set_explicit_neighbor_columns (all);

  Rational global_shortest = find_shortest (all);
  prune_loose_colunms (me, &all, global_shortest);
  set_implicit_neighbor_columns (all);

  
  int j = 0;
  for (int i = 1; i < all.size (); i++)
    {
      Grob *sc = all[i];
      if (Item::breakable_b (sc))
        {
	  Link_array<Grob> measure (all.slice (j, i+1));	  
          do_measure (global_shortest, me, &measure);
	  j = i;
        }
    }

  return SCM_UNSPECIFIED;
}


/*
  We want the shortest note that is also "common" in the piece, so we
  find the shortest in each measure, and take the most frequently
  found duration.

  This probably gives weird effects with modern music, where every
  note has a different duration, but hey, don't write that kind of
  stuff, then.

*/
Rational
Spacing_spanner::find_shortest (Link_array<Grob> const &cols)
{
  /*
    ascending in duration
   */
  Array<Rational> durations; 
  Array<int> counts;
  
  Rational shortest_in_measure;
  shortest_in_measure.set_infinite (1);
  
  for (int i =0 ; i < cols.size (); i++)  
    {
      if (Paper_column::musical_b (cols[i]))
	{
	  Moment *when = unsmob_moment (cols[i]->get_grob_property  ("when"));

	  /*
	    ignore grace notes for shortest notes.
	  */
	  if (when && when->grace_part_)
	    continue;
	  
	  SCM  st = cols[i]->get_grob_property ("shortest-starter-duration");
	  Moment this_shortest = *unsmob_moment (st);
	  assert (this_shortest.to_bool());
	  shortest_in_measure = shortest_in_measure <? this_shortest.main_part_;
	}
      else if (!shortest_in_measure.infty_b()
	       && Item::breakable_b (cols[i]))
	{
	  int j = 0;
	  for (; j < durations.size(); j++)
	    {
	      if (durations[j] > shortest_in_measure)
		{
		  counts.insert (1, j);
		  durations.insert (shortest_in_measure, j);
		  break;
		}
	      else if (durations[j] == shortest_in_measure)
		{
		  counts[j]++;
		  break;
		}
	    }

	  if (durations.size() == j)
	    {
	      durations.push (shortest_in_measure);
	      counts.push (1);
	    }

	  shortest_in_measure.set_infinite(1); 
	}
    }

  int max_idx = -1;
  int max_count = 0;
  for (int i =durations.size(); i--;)
    {
      if (counts[i] >= max_count)
	{
	  max_idx = i;
	  max_count = counts[i];
	}

      //      printf ("Den %d/%d, c %d\n", durations[i].num (), durations[i].den (), counts[i]);
    }

  /*
    TODO: 1/8 should be adjustable?
   */
  Rational d = Rational (1,8);
  if (max_idx >= 0)
    d = d <? durations[max_idx] ;

  return d;
}

/*
  Generate spacing for a single measure. We used to have code that did
  per-measure spacing. Now we have piecewise spacing. We should fix
  this to support "spacing-regions": some regions have different notes
  (different time sigs) than others, and should be spaced differently.
 */
void
Spacing_spanner::do_measure (Rational shortest, Grob*me, Link_array<Grob> *cols) 
{

  Real headwid =       gh_scm2double (me->get_grob_property ("spacing-increment"));
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


      musical_column_spacing (me, lc, rc, headwid, shortest);
      if (Item *rb = r->find_prebroken_piece (LEFT))
	musical_column_spacing (me, lc, rb, headwid, shortest);
    }    
}


/*
  Generate the space between two musical columns LC and RC, given spacing parameters INCR and SHRTEST.
 */
void
Spacing_spanner::musical_column_spacing (Grob *me, Item * lc, Item *rc, Real increment, Rational shortest)
{
  bool expand_only = false;
  Real base_note_space = note_spacing (me, lc, rc, shortest, &expand_only);

  Real max_note_space = -infinity_f;
  Real max_fixed_note_space = -infinity_f;

  SCM seq  = lc->get_grob_property ("right-neighbors");

  /*
    We adjust the space following a note only if the next note
    happens after the current note (this is set in the grob
    property SPACING-SEQUENCE.
  */
  for (SCM s = seq; gh_pair_p (s); s = ly_cdr (s))
    {
      Grob * wish = unsmob_grob (gh_car (s));

      Item *wish_rcol = Note_spacing::right_column (wish);
      if (Note_spacing::left_column (wish) != lc
	  || (wish_rcol != rc && wish_rcol != rc->original_l_))
	continue;

      /*
	This is probably a waste of time in the case of polyphonic
	music.  */
      if (Note_spacing::has_interface (wish))
	{
	  Real space =0.0;
	  Real fixed =0.0;
	  
	  Note_spacing::get_spacing (wish, rc, base_note_space, increment, &space, &fixed);
	  max_note_space = max_note_space >? space;
	  max_fixed_note_space = max_fixed_note_space >? fixed;
	}

    }

  if (max_note_space < 0)
    {
      max_note_space = base_note_space;
      max_fixed_note_space = increment;
    }

  Spaceable_grob::add_spring (lc, rc, max_note_space,  1 / (max_note_space -max_fixed_note_space), expand_only);
}


/*
  Read hints from L and generate springs.
 */
void
Spacing_spanner::breakable_column_spacing (Item* l, Item *r)
{
  Real max_fixed = -infinity_f;
  Real max_space = -infinity_f;
  
  for (SCM s = l->get_grob_property ("spacing-wishes");
       gh_pair_p (s); s = gh_cdr (s))
    {
      Item * spacing_grob = dynamic_cast<Item*> (unsmob_grob (gh_car (s)));

      if (!spacing_grob || !Staff_spacing::has_interface (spacing_grob))
	continue;

      Real space;
      Real fixed_space;

      /*
	column for the left one settings should be ok due automatic
	pointer munging.

      */
      assert (spacing_grob-> column_l () == l);

      Staff_spacing::get_spacing_params (spacing_grob,
					 &space, &fixed_space);  
      if (space > max_space)
	{
	  max_space = space;
	  max_fixed = fixed_space;
	}
    }

  if (isinf (max_space))
    {
      programming_error ("No pref spacing found");
      max_space = 2.0;
      max_fixed = 1.0;
    }

  
  if (l->break_status_dir() == RIGHT
      && Paper_column::when_mom (l) == Paper_column::when_mom (r))
    {
      /* Start of line: this space is not stretchable */
      max_fixed = max_space;
    }

  /*
    Hmm.  we do 1/0 in the next thing. Perhaps we should check if this
    works on all architectures.
   */
  
  Spaceable_grob::add_spring (l, r, max_space,  1/(max_space - max_fixed), false);  
}


/**
  Get the measure wide ant for arithmetic spacing.
  */
Real
Spacing_spanner::get_duration_space (Grob*me, Moment d, Rational shortest, bool * expand_only) 
{
  Real k = gh_scm2double (me->get_grob_property ("shortest-duration-space"));
    
  if (d < shortest)
    {
      Rational ratio = d.main_part_ / shortest;
      
      *expand_only = true;
      return (0.5 + 0.5 * double (ratio)) * k ;
    }
  else
    {
      /*
	  @see
  John S. Gourlay. ``Spacing a Line of Music,'' Technical Report
  OSU-CISRC-10/87-TR35, Department of Computer and Information Science,
  The Ohio State University, 1987.
       */
      Real log =  log_2 (shortest);
      k -= log;
      Rational compdur = d.main_part_ + d.grace_part_ /Rational (3);
      *expand_only = false;      
   
      return (log_2 (compdur) + k) * gh_scm2double (me->get_grob_property ("spacing-increment"));
    }
}

Real
Spacing_spanner::note_spacing (Grob*me, Grob *lc, Grob *rc,
			       Moment shortest, bool * expand_only) 
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
  
  Moment delta_t = Paper_column::when_mom (rc) - Paper_column::when_mom (lc);
  Real dist = 0.0;

  if (delta_t.main_part_)
    {
      dist = get_duration_space (me, shortest_playing_len, shortest.main_part_, expand_only);
      dist *= (double) (delta_t.main_part_ / shortest_playing_len.main_part_);
    }
  else if (delta_t.grace_part_)
    {
      /*
	TODO: figure out how to space grace notes.
      */
      dist = get_duration_space (me, shortest, shortest.main_part_, expand_only);

      Real grace_fact = 1.0;
      SCM gf = me->get_grob_property ("grace-space-factor");
      if (gh_number_p (gf))
	grace_fact = gh_scm2double (gf);

      dist *= grace_fact;
    }

  
  return dist;
}



ADD_INTERFACE (Spacing_spanner,"spacing-spanner-interface",
  " SPACE = arithmetic_multiplier * ( C + log2 (TIME) ))
The space taken by a note is determined by the formula 



where TIME is the amount of time a note occupies.  The value of C is
chosen such that the smallest space within a measure is
arithmetic_basicspace:

C = arithmetic_basicspace - log2 (mininum (SHORTEST, 1/8)) 

The smallest space is the one following the shortest note in the
measure, or the space following a hypothetical 1/8 note.  Typically
arithmetic_basicspace is set to a value so that the shortest note
takes about two noteheads of space (ie, is followed by a notehead of
space):

@example
2*quartwidth = arithmetic_multiplier * ( C + log2 (SHORTEST) ))

@{ using: C = arithmetic_basicspace - log2 (mininum (SHORTEST, 1/8)) @}
@{ assuming: SHORTEST <= 1/8 @}

= arithmetic_multiplier *
( arithmetic_basicspace - log2 (SHORTEST) + log2 (SHORTEST) )

= arithmetic_multiplier * arithmetic_basicspace

@{ choose: arithmetic_multiplier = 1.0*quartwidth (why?) @}

= quartwidth * arithmetic_basicspace

=>	       

arithmetic_basicspace = 2/1 = 2


If you want to space your music wider, use something like:

arithmetic_basicspace = 4.;

@end example",
  "spacing-increment shortest-duration-space");

