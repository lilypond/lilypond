/*   
  spacing-spanner.cc -- implement Spacing_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include <cmath>
#include <cstdio>

#include "main.hh"
#include "system.hh"
#include "warn.hh"
#include "output-def.hh"
#include "paper-score.hh"
#include "paper-column.hh"
#include "moment.hh"
#include "note-spacing.hh"
#include "misc.hh"
#include "warn.hh"
#include "staff-spacing.hh"
#include "spring.hh"
#include "paper-column.hh"
#include "spaceable-grob.hh"
#include "break-align-interface.hh"
#include "spacing-interface.hh"

/*
  TODO: this file/class is too complex. Should figure out how to chop
  this up even more.
    
 */

class Spacing_spanner
{
public:
  static void standard_breakable_column_spacing (Grob * me, Item*l, Item*r,
						 Real * fixed, Real * space, Moment);
  

  static Real default_bar_spacing (Grob*,Grob*,Grob*,Moment);
  static Real note_spacing (Grob*,Grob*,Grob*,Moment, bool*);
  static Real get_duration_space (Grob*,Moment dur, Rational shortest, bool*);
  static Rational find_shortest (Grob *, Link_array<Grob> const &);  
  static void breakable_column_spacing (Grob*, Item* l, Item *r, Moment);
  static void find_loose_columns () {}
  static void prune_loose_columns (Grob*,Link_array<Grob> *cols, Rational);
  static void find_loose_columns (Link_array<Grob> cols);
  static void set_explicit_neighbor_columns (Link_array<Grob> cols);
  static void set_implicit_neighbor_columns (Link_array<Grob> cols);
  static void do_measure (Rational, Grob*me,Link_array<Grob> *cols);
  static void musical_column_spacing (Grob*,Item*,Item*, Real, Rational); 
  DECLARE_SCHEME_CALLBACK (set_springs, (SCM ));
  static bool has_interface (Grob*);
};

/*
  Return whether COL is fixed to its neighbors by some kind of spacing
  constraint.

  
  If in doubt, then we're not loose; the spacing engine should space
  for it, risking suboptimal spacing.
  
  (Otherwise, we might risk core dumps, and other weird stuff.)

*/
static bool
loose_column (Grob *l, Grob *c, Grob *r) 
{
  SCM rns = c->get_property ("right-neighbors");
  SCM lns = c->get_property ("left-neighbors");

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
  if (!scm_is_pair (lns) || !scm_is_pair (rns))
    return false;

  Item * l_neighbor = dynamic_cast<Item*>  (unsmob_grob (scm_car (lns)));
  Item * r_neighbor = dynamic_cast<Item*>  (unsmob_grob (scm_car (rns)));

  if (!l_neighbor || !r_neighbor)
    return false;

  l_neighbor = l_neighbor->get_column ();
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
  if (!  ((Paper_column::is_musical (l_neighbor) || Item::is_breakable (l_neighbor))
	 && (Paper_column::is_musical (r_neighbor) || Item::is_breakable (r_neighbor))) )
    {
      return false;
    }


  /*
    A rather hairy check, but we really only want to move around clefs. (anything else?)

    in any case, we don't want to move bar lines.
   */
  for (SCM e = c->get_property ("elements"); scm_is_pair (e); e = scm_cdr (e))
    {
      Grob * g = unsmob_grob (scm_car (e));
      if (g && Break_align_interface::has_interface (g))
	{
	  for (SCM s = g->get_property ("elements"); scm_is_pair (s);
	       s = scm_cdr (s))
	    {
	      Grob *h = unsmob_grob (scm_car (s));

	      /*
		ugh. -- fix staff-bar name? 
	      */
	      if (h  && h->get_property ("break-align-symbol") == ly_symbol2scm ("staff-bar"))
		return false;
	    }
	}
    }
  
  return true;
}

/*
  Remove columns that are not tightly fitting from COLS. In the
  removed columns, set 'between-cols to the columns where it is in
  between.
*/
void
Spacing_spanner::prune_loose_columns (Grob*me,Link_array<Grob> *cols, Rational shortest)
{
  Link_array<Grob> newcols;
  Real increment = robust_scm2double (me->get_property ("spacing-increment"), 1.2);
  for (int i=0; i < cols->size ();  i++)
    {
      if (Item::is_breakable (cols->elem (i)) || Paper_column::is_musical (cols->elem (i)))
	{
	  newcols.push (cols->elem (i));
	  continue;
	}

      Grob *c = cols->elem (i);
      if (loose_column (cols->elem (i-1), c, cols->elem (i+1)))
	{
	  SCM lns = c->get_property ("left-neighbors");
	  lns = scm_is_pair (lns) ? scm_car (lns) : SCM_BOOL_F;

	  SCM rns = c->get_property ("right-neighbors");
	  rns = scm_is_pair (rns) ? scm_car (rns) : SCM_BOOL_F;

	  /*
	    Either object can be non existent, if the score ends
	    prematurely.
	  */
	  rns = scm_car (unsmob_grob (rns)->get_property ("right-items"));
	  c->set_property ("between-cols", scm_cons (lns,
						     rns));

	  /*
	    Set distance constraints for loose columns
	  */
	  Drul_array<Grob*> next_door;
	  next_door[LEFT] =cols->elem (i - 1);
	  next_door[RIGHT] =cols->elem (i + 1);	  
	  Direction d = LEFT;
	  Drul_array<Real> dists (0,0);

	  do
	    {
	      dists[d] = 0.0;
	      Item *lc = dynamic_cast<Item*> ((d == LEFT)  ? next_door[LEFT] : c);
	      Item *rc = dynamic_cast<Item*> (d == LEFT  ? c : next_door[RIGHT]);

	      for (SCM s = lc->get_property ("spacing-wishes");
		   scm_is_pair (s); s = scm_cdr (s))
		{
		  Grob *sp = unsmob_grob (scm_car (s));
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
	  r.distance_ = dists[LEFT] + dists[RIGHT];
	  r.item_l_drul_[LEFT] = dynamic_cast<Item*> (cols->elem (i-1));
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
  for (int i=0; i < cols.size (); i++)
    {
      SCM right_neighbors = SCM_EOL;
      int min_rank = 100000;	// inf.


      SCM wishes=  cols[i]->get_property ("spacing-wishes");
      for (SCM s =wishes; scm_is_pair (s); s = scm_cdr (s))
	{
	  Item * wish = dynamic_cast<Item*> (unsmob_grob (scm_car (s)));

	  Item * lc = wish->get_column ();
	  Grob * right = Note_spacing::right_column (wish);

	  if (!right)
	    continue;

	  Item * rc = dynamic_cast<Item*> (right);

	  int right_rank = Paper_column::get_rank (rc);
	  int left_rank = Paper_column::get_rank (lc);	  

	  /*
	    update the left column.
	  */
	  if (right_rank <= min_rank)
	    {
	      if (right_rank < min_rank)
		right_neighbors  =SCM_EOL;
	      
	      min_rank = right_rank;
	      right_neighbors = scm_cons (wish->self_scm (), right_neighbors);
	    }

	  /*
	    update the right column of the wish.
	  */
	  int maxrank = 0;
	  SCM left_neighs = rc->get_property ("left-neighbors");
	  if (scm_is_pair (left_neighs)
	      && unsmob_grob (scm_car (left_neighs)))
	    {
	      Item * it = dynamic_cast<Item*> (unsmob_grob (scm_car (left_neighs)));
	      maxrank = Paper_column::get_rank (it->get_column ());
	    }

	  if (left_rank >= maxrank)
	    {
	      if (left_rank > maxrank)
		left_neighs = SCM_EOL;

	      left_neighs = scm_cons (wish->self_scm (), left_neighs);
	      rc->set_property ("left-neighbors", right_neighbors);
	    }
	}

      if (scm_is_pair (right_neighbors))
	{
	  cols[i]->set_property ("right-neighbors", right_neighbors);
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
      if (!Item::is_breakable (it) && !Paper_column::is_musical (it))
	continue;

      // it->breakable || it->musical

      /*
	sloppy with typnig left/right-neighbors should take list, but paper-column found instead.
      */
      SCM ln = cols[i] ->get_property ("left-neighbors");
      if (!scm_is_pair (ln) && i ) 
	{
	  cols[i]->set_property ("left-neighbors", scm_cons (cols[i-1]->self_scm (), SCM_EOL));
	}

      SCM rn = cols[i] ->get_property ("right-neighbors");
      if (!scm_is_pair (rn) && i < cols.size () - 1) 
	{
	  cols[i]->set_property ("right-neighbors", scm_cons (cols[i + 1]->self_scm (), SCM_EOL));
	}
    }
}


MAKE_SCHEME_CALLBACK (Spacing_spanner, set_springs,1);
SCM
Spacing_spanner::set_springs (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  /*
    can't use get_system() ? --hwn.
  */
  Link_array<Grob> all (me->pscore_->system_->columns ());

  set_explicit_neighbor_columns (all);

  SCM preset_shortest = me->get_property ("common-shortest-duration");
  Rational global_shortest;
  if (unsmob_moment (preset_shortest))
    {
      global_shortest = unsmob_moment (preset_shortest)->main_part_;
    }
  else
    {
      global_shortest = find_shortest (me, all);
      if (verbose_global_b)
	progress_indication (_f ("Global shortest duration is %s", global_shortest.to_string ()) + "\n");
    }
  prune_loose_columns (me, &all, global_shortest);
  set_implicit_neighbor_columns (all);

  
  int j = 0;
  for (int i = 1; i < all.size (); i++)
    {
      Grob *sc = all[i];
      if (Item::is_breakable (sc))
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
Spacing_spanner::find_shortest (Grob *me, Link_array<Grob> const &cols)
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
      if (Paper_column::is_musical (cols[i]))
	{
	  Moment *when = unsmob_moment (cols[i]->get_property  ("when"));

	  /*
	    ignore grace notes for shortest notes.
	  */
	  if (when && when->grace_part_)
	    continue;
	  
	  SCM  st = cols[i]->get_property ("shortest-starter-duration");
	  Moment this_shortest = *unsmob_moment (st);
	  assert (this_shortest.to_bool ());
	  shortest_in_measure = shortest_in_measure <? this_shortest.main_part_;
	}
      else if (!shortest_in_measure.is_infinity ()
	       && Item::is_breakable (cols[i]))
	{
	  int j = 0;
	  for (; j < durations.size (); j++)
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

	  if (durations.size () == j)
	    {
	      durations.push (shortest_in_measure);
	      counts.push (1);
	    }

	  shortest_in_measure.set_infinite (1); 
	}
    }

  int max_idx = -1;
  int max_count = 0;
  for (int i =durations.size (); i--;)
    {
      if (counts[i] >= max_count)
	{
	  max_idx = i;
	  max_count = counts[i];
	}

      //      printf ("duration %d/%d, count %d\n", durations[i].num (), durations[i].den (), counts[i]);
    }

  SCM  bsd = me->get_property ("base-shortest-duration");
  Rational d = Rational (1,8);
  if (Moment *m = unsmob_moment (bsd))
    d = m->main_part_;
  
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
Spacing_spanner::do_measure (Rational global_shortest, Grob*me,
			     Link_array<Grob> *cols) 
{

  Real headwid = robust_scm2double (me->get_property ("spacing-increment"), 1);
  for (int i= 0; i < cols->size () - 1; i++)
    {
      Item * l = dynamic_cast<Item*> (cols->elem (i));
      Item * r =  dynamic_cast<Item*> (cols->elem (i+1));

      Paper_column * lc = dynamic_cast<Paper_column*> (l);
      Paper_column * rc = dynamic_cast<Paper_column*> (r);

      if (!Paper_column::is_musical (l))
	{
	  breakable_column_spacing (me, l, r, global_shortest);

	  /*
	    
	    The case that the right part is broken as well is rather
	    rare, but it is possible, eg. with a single empty measure,
	    or if one staff finishes a tad earlier than the rest.
	    
	   */
	  Item *lb = l->find_prebroken_piece (RIGHT);
	  Item *rb = r->find_prebroken_piece (LEFT);
	  
	  if (lb)
	    breakable_column_spacing (me, lb,r, global_shortest);

	  if (rb)
	    breakable_column_spacing (me, l, rb, global_shortest);
	  if (lb && rb)
	    breakable_column_spacing (me, lb, rb, global_shortest);
	}
      else
	{
	  musical_column_spacing (me, lc, rc, headwid, global_shortest);
	  if (Item *rb = r->find_prebroken_piece (LEFT))
	    musical_column_spacing (me, lc, rb, headwid, global_shortest);
	}    
    }
}

/*
  Generate the space between two musical columns LC and RC, given
  spacing parameters INCR and SHORTEST.
 */
void
Spacing_spanner::musical_column_spacing (Grob *me, Item * lc, Item *rc, Real increment, Rational global_shortest)
{
  bool expand_only = false;
  Real base_note_space = note_spacing (me, lc, rc, global_shortest, &expand_only);

  Real compound_note_space = 0.0;
  Real compound_fixed_note_space = 0.0;
  int wish_count = 0;
  
  SCM seq  = lc->get_property ("right-neighbors");

  /*
    We adjust the space following a note only if the next note
    happens after the current note (this is set in the grob
    property SPACING-SEQUENCE.
  */
  for (SCM s = seq; scm_is_pair (s); s = scm_cdr (s))
    {
      Grob * wish = unsmob_grob (scm_car (s));

      Item *wish_rcol = Note_spacing::right_column (wish);
      if (Note_spacing::left_column (wish) != lc
	  || (wish_rcol != rc && wish_rcol != rc->original_))
	continue;

      /*
	This is probably a waste of time in the case of polyphonic
	music.  */
      if (Note_spacing::has_interface (wish))
	{
	  Real space =0.0;
	  Real fixed =0.0;
	  
	  Note_spacing::get_spacing (wish, rc, base_note_space, increment, &space, &fixed);

	  
	  compound_note_space = compound_note_space + space;
	  compound_fixed_note_space = compound_fixed_note_space + fixed;
	  wish_count ++;
	  
	}
    }

  if (Paper_column::when_mom (rc).grace_part_ &&
      !Paper_column::when_mom (lc).grace_part_)
    {
      /*
	Ugh. 0.8 is arbitrary.
       */
      compound_note_space *= 0.8; 
    }
  
  if (compound_note_space < 0 || wish_count == 0)
    {
      compound_note_space = base_note_space;
      compound_fixed_note_space =  increment;
    }
  else
    {
      compound_note_space /= wish_count;
      compound_fixed_note_space /= wish_count;
    }

  /*
    Whatever we do, the fixed space is smaller than the real
    space.

    TODO: this criterion is discontinuous in the derivative.
    Maybe it should be continuous?
  */
  compound_fixed_note_space = compound_fixed_note_space <? compound_note_space;

  bool packed = to_boolean (me->get_layout ()->c_variable ("packed"));
  Real strength, distance;

  /*
    TODO: make sure that the space doesn't exceed the right margin.
   */
  if (packed)
    {
      /*
	In packed mode, pack notes as tight as possible.  This makes
	sense mostly in combination with raggedright mode: the notes
	are then printed at minimum distance.  This is mostly useful
	for ancient notation, but may also be useful for some flavours
	of contemporary music.  If not in raggedright mode, lily will
	pack as much bars of music as possible into a line, but the
	line will then be stretched to fill the whole linewidth.
      */
      strength = 1.0;
      distance = compound_fixed_note_space;
    }
  else
    {
      strength = 1 / (compound_note_space - compound_fixed_note_space);
      distance = compound_note_space;
    }

  Spaceable_grob::add_spring (lc, rc, distance, strength);  
}


/*
  The one-size-fits all spacing. It doesn't take into account
  different spacing wishes from one to the next column.
 */
void
Spacing_spanner::standard_breakable_column_spacing (Grob * me, Item*l, Item*r,
				   Real * fixed, Real * space,
				   Moment shortest)
{
  *fixed = 0.0;
  Direction d = LEFT;
  Drul_array<Item*> cols (l,r);
  
  do
    {
      if (!Paper_column::is_musical (cols[d]))
	{
	  /*
	    Tied accidentals over barlines cause problems, so lets see
	    what happens if we do this for non musical columns only.
	   */
	  Interval lext = cols[d]->extent (cols [d], X_AXIS);
	  if (!lext.is_empty ())
	    *fixed += -d * lext[-d];
	}
    }
  while (flip (&d) != LEFT);
  

  if (l->is_breakable (l) && r->is_breakable (r))
    {
      Moment *dt = unsmob_moment (l->get_property ("measure-length"));
      Moment mlen (1);
      if (dt)
	mlen = *dt;
      
      Real incr = robust_scm2double (me->get_property ("spacing-increment"), 1);

      *space =  *fixed + incr * double (mlen.main_part_ / shortest.main_part_) * 0.8;
    }
  else
    {
      Moment dt = Paper_column::when_mom (r) - Paper_column::when_mom (l);

      if (dt == Moment (0,0))
	{
	  /*
	    In this case, Staff_spacing should handle the job,
	    using dt when it is 0 is silly.
	   */
	  *space = *fixed + 0.5; 
	}
      else
	{
	  bool dummy;
	  *space = *fixed + get_duration_space (me, dt, shortest.main_part_, &dummy);
	}
    }
}


/*
  Read hints from L and generate springs.
*/
void
Spacing_spanner::breakable_column_spacing (Grob*me, Item* l, Item *r,Moment shortest)
{
  Real compound_fixed = 0.0;
  Real compound_space = 0.0;
  int wish_count = 0;

  Moment dt = Paper_column::when_mom (r) - Paper_column::when_mom (l);

  if (dt == Moment (0,0))
    {
      for (SCM s = l->get_property ("spacing-wishes");
	   scm_is_pair (s); s = scm_cdr (s))
	{
	  Item * spacing_grob = dynamic_cast<Item*> (unsmob_grob (scm_car (s)));

	  if (!spacing_grob || !Staff_spacing::has_interface (spacing_grob))
	    continue;

	  Real space;
	  Real fixed_space;

	  /*
	    column for the left one settings should be ok due automatic
	    pointer munging.

	  */
	  assert (spacing_grob-> get_column () == l);

	  Staff_spacing::get_spacing_params (spacing_grob,
					     &space, &fixed_space);

	  if (Paper_column::when_mom (r).grace_part_)
	    {
	      /*
		Correct for grace notes.

		Ugh. The 0.8 is arbitrary.
	      */
	      space *= 0.8;
	    }


	  compound_space += space;
	  compound_fixed += fixed_space;
	  wish_count ++ ;
	}
    }

  if (compound_space <= 0.0 || !wish_count)
    {
      standard_breakable_column_spacing (me, l, r, &compound_fixed, &compound_space ,
					 shortest);
      wish_count = 1;
    }
  else
    {
      compound_space /= wish_count;
      compound_fixed /= wish_count;
    }

  assert (!isinf (compound_space));
  compound_space = compound_space >? compound_fixed;

  
  /*
    Hmm.  we do 1/0 in the next thing. Perhaps we should check if this
    works on all architectures.
   */

  /*
    There used to be code that changed spacing depending on
    raggedright setting.  Ugh.

    Do it more cleanly, or rename the property. 
    
   */
  Real strength = 1 / (compound_space - compound_fixed);
  Real distance = compound_space;
  Spaceable_grob::add_spring (l, r, distance, strength);
}


/**
  Get the measure wide ant for arithmetic spacing.
  */
Real
Spacing_spanner::get_duration_space (Grob*me, Moment d, Rational shortest, bool * expand_only) 
{
  Real k = robust_scm2double (me->get_property ("shortest-duration-space"), 1);
  Real incr = robust_scm2double (me->get_property ("spacing-increment"), 1);
  
  if (d < shortest)
    {
      /*
	We don't space really short notes using the log of the
	duration, since it would disproportionally stretches the long
	notes in a piece. In stead, we use geometric spacing with constant 0.5
	(i.e. linear.)

	This should probably be tunable, to use other base numbers.

	In Mozart hrn3 by EB., we have 8th note = 3.9 mm (total), 16th note =
	3.6 mm (total).  head-width = 2.4, so we 1.2mm for 16th, 1.5
	mm for 8th. (white space), suggesting that we use

	(1.2 / 1.5)^{-log2(duration ratio)}
	

       */
      Rational ratio = d.main_part_ / shortest;

      return ((k-1) + double (ratio)) * incr;
    }
  else
    {
      /*
	  John S. Gourlay. ``Spacing a Line of Music,'' Technical
	  Report OSU-CISRC-10/87-TR35, Department of Computer and
	  Information Science, The Ohio State University, 1987.
       */
      Real log =  log_2 (shortest);
      k -= log;
      Rational compdur = d.main_part_ + d.grace_part_ /Rational (3);
      *expand_only = false;      
   
      return (log_2 (compdur) + k) * incr;
    }
}

Real
Spacing_spanner::note_spacing (Grob*me, Grob *lc, Grob *rc,
			       Moment shortest, bool * expand_only) 
{
  Moment shortest_playing_len = 0;
  SCM s = lc->get_property ("shortest-playing-duration");

  if (unsmob_moment (s))
    shortest_playing_len = *unsmob_moment (s);
  
  if (! shortest_playing_len.to_bool ())
    {
      programming_error ("can't find a ruling note at " + Paper_column::when_mom (lc).to_string ());
      shortest_playing_len = 1;
    }

  Moment lwhen = Paper_column::when_mom (lc);
  Moment rwhen =  Paper_column::when_mom (rc);

  Moment delta_t = rwhen - lwhen;
  if (!Paper_column::is_musical (rc))
    {
      /*
	when toying with mmrests, it is possible to have musical
	column on the left and non-musical on the right, spanning
	several measures.
       */
      
      Moment *dt = unsmob_moment (rc->get_property ("measure-length"));
      if (dt)
	{
	  delta_t = delta_t <? *dt;

	  /*
	    The following is an extra safety measure, such that
	    the length of a mmrest event doesn't cause havoc.
	   */
	  shortest_playing_len = shortest_playing_len <? *dt;
	}
    }
  Real dist = 0.0;

  /*
    In normal situations, the next column is at most
    SHORTEST_PLAYING_LEN away. However chord-tremolos do funky faking stuff
    with durations, invalidating this assumption. Here we kludge
    around to get chord tremolos to behave properly.
    
   */
  shortest_playing_len = shortest_playing_len >? delta_t;
  if (delta_t.main_part_ && !lwhen.grace_part_)
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

      Real grace_fact
	= robust_scm2double (me->get_property ("grace-space-factor"), 1);

      dist *= grace_fact;
    }

  
  return dist;
}



ADD_INTERFACE (Spacing_spanner,"spacing-spanner-interface",
"The space taken by a note is dependent on its duration. Doubling a\n"
"duration adds spacing-increment to the space. The most common shortest\n"
"note gets @code{shortest-duration-space}. Notes that are even shorter are\n"
"spaced proportonial to their duration.\n"
"\n"
"Typically, the increment is the width of a black note head.  In a\n"
"piece with lots of 8th notes, and some 16th notes, the eighth note\n"
"gets 2 note heads width (i.e. the space following a note is 1 note\n"
"head width) A 16th note is followed by 0.5 note head width. The\n"
"quarter note is followed by  3 NHW, the half by 4 NHW, etc.\n",
  "grace-space-factor spacing-increment base-shortest-duration shortest-duration-space common-shortest-duration");



ADD_INTERFACE (Spacing_interface, "spacing-interface",
  "Something to do with line breaking and spacing. Kill this one after determining line breaks.",
  "");

