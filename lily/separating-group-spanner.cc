/*   
  separating-group-spanner.cc --  implement Separating_group_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "separating-group-spanner.hh"
#include "separation-item.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "dimensions.hh"
#include "group-interface.hh"

void
Separating_group_spanner::find_rods (Item * r, SCM next)
{
  Interval ri (Separation_item::my_width (r));
  if (ri.empty_b ())
    return;

  /*
    This is an inner loop, however, in most cases, the interesting L
    will just be the first entry of NEXT, making it linear in most of
    the cases.  */
  for(; gh_pair_p (next); next = ly_cdr (next))
    {
      Item *l = dynamic_cast<Item*> (unsmob_grob (ly_car( next)));
      Item *lb = l->find_prebroken_piece (RIGHT);

      if (lb)
	{
	  Interval li (Separation_item::my_width (lb));

	  if (!li.empty_b ())
	    {
	      Rod rod;

	      rod.item_l_drul_[LEFT] = lb;
	      rod.item_l_drul_[RIGHT] = r;

	      rod.distance_f_ = li[RIGHT] - ri[LEFT];
	
	      rod.columnize ();
	      rod.add_to_cols ();
	    }
	}

      Interval li (Separation_item::my_width (l));
      if (!li.empty_b ())
	{
	  Rod rod;

	  rod.item_l_drul_[LEFT] =l;
	  rod.item_l_drul_[RIGHT]=r;

	  rod.distance_f_ = li[RIGHT] - ri[LEFT];
	
	  rod.columnize ();
	  rod.add_to_cols ();

	  break;
	}
      else
	/*
	  this grob doesn't cause a constraint. We look further until we
	  find one that does.  */
	;
    }
}

MAKE_SCHEME_CALLBACK (Separating_group_spanner,set_spacing_rods,1);
SCM
Separating_group_spanner::set_spacing_rods (SCM smob)
{
  Grob*me = unsmob_grob (smob);
  
  for (SCM s = me->get_grob_property ("elements"); gh_pair_p (s) && gh_pair_p (ly_cdr (s)); s = ly_cdr (s))
    {
      /*
	Order of elements is reversed!
       */
      SCM elt = ly_car (s);
      Item *r = dynamic_cast<Item*> (unsmob_grob (elt));

      if (!r)
	continue;

      Item *rb
	= dynamic_cast<Item*> (r->find_prebroken_piece (LEFT));
      
      find_rods (r, ly_cdr (s));
      if (rb)
	find_rods (rb, ly_cdr (s));
    }
  find_musical_sequences (me);
#if 0
  /*
    TODO; restore this.
   */
  /*
    We've done our job, so we get lost. 
   */
  for (SCM s = me->get_grob_property ("elements"); gh_pair_p (s); s = ly_cdr (s))
    {
      Item * it =dynamic_cast<Item*> (unsmob_grob (ly_car (s)));
      if (it && it->broken_b ())
	{
	  it->find_prebroken_piece (LEFT) ->suicide ();
	  it->find_prebroken_piece (RIGHT)->suicide ();
	}
      it->suicide ();
    }
  me->suicide ();
#endif
  return SCM_UNSPECIFIED ;
}

void
Separating_group_spanner::add_spacing_unit (Grob* me ,Item*i)
{
  Pointer_group_interface::add_element (me, "elements",i);
  me->add_dependency (i);
}


void
Separating_group_spanner::find_musical_sequences (Grob *me)
{
  Item *last = 0;
  Item *llast = 0;
  for (SCM s = me->get_grob_property ("elements");
       gh_pair_p (s); s = ly_cdr (s))
    {
      Item *it = dynamic_cast<Item*> (unsmob_grob (ly_car (s)));
      if (last)
	{	
	  Item *lcol = last->column_l ();
	  Item *col = it->column_l ();

	  int lrank = Paper_column::rank_i (lcol);
	  int rank = Paper_column ::rank_i (col);

	  bool mus = Paper_column::musical_b (col);
	  bool lmus = Paper_column::musical_b (lcol);

	  if ((lrank - rank == 2) && lmus && mus)
	    {
	      SCM seq = col->get_grob_property ("spacing-sequence");
	      col->set_grob_property ("spacing-sequence",
				      gh_cons (gh_cons (it->self_scm (), last->self_scm ()), seq));
	    }

	  if (llast && !Paper_column::breakable_b (last))
	    {
	      Item *llcol = llast->column_l ();
	      int llrank = Paper_column::rank_i (llcol);
	      bool llmus= Paper_column::musical_b (llcol);
	      if (llrank - lrank == 1
		  && lrank - rank == 1
		  && llmus && !lmus && mus)
		{
		  SCM seq = col->get_grob_property ("spacing-sequence");
		  col->set_grob_property ("spacing-sequence",
					  gh_cons (gh_cons (it->self_scm (), last->self_scm ()), seq));
		}
	      else if (!lmus)
		{
		  SCM between = lcol->get_grob_property ("between-cols");

		  if (!gh_pair_p (between))
		    {
		      between = gh_cons (it->self_scm (), llast->self_scm ());
		      lcol ->set_grob_property ("between-cols", between);
		    }

		  Item * left
		    = dynamic_cast<Item*> (unsmob_grob (ly_car (between)));
		  if(Paper_column::rank_i (left->column_l ()) < rank)
		    gh_set_car_x (between, col->self_scm());
		  
		  Item * right
		    = dynamic_cast<Item*> (unsmob_grob (ly_cdr (between)));
		  if (Paper_column::rank_i (right->column_l ()) > llrank )
		    gh_set_cdr_x (between, llcol->self_scm ());
		}
	    }
	}

      llast = last;
      last = it;
    }
}

#if 0
void
Separating_group_spanner::set_loose_rods ()
{
  // loose columns should  also generate minimum distances.
  // TODO
}
#endif


void
Separating_group_spanner::set_interface (Grob*)
{
}

bool
Separating_group_spanner::has_interface (Grob*)
{//todo
  assert (false);
}
