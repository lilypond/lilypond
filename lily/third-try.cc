/*   
  spacing-spanner.cc -- implement Spacing_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "paper-column.hh"


class Third_spacing_spanner
{
public:
  void find_loose_columns () {}
  void prune_loose_colunms (Link_array<Grob> *cols);
  void find_loose_columns (Link_array<Grob> cols);
};



static bool
fixed_neighbor (Grob *col, SCM nm)
{
  SCM l = col->internal_get_grob_property (nm);

  if (!gh_pair_p (l))
    return false;

  Item * left = dynamic_cast<Item*>  (unsmob_grob (gh_car (l)));
  return abs (Paper_column::rank_i (left->column_l ()) - Paper_column::rank_i (col)) == 1 ;
}

void
Third_spacing_spanner::prune_loose_colunms (Link_array<Grob> *cols)
{
   for (int i=cols->size ();  i--; )
     {

       if (Item::breakable_b (cols->elem(i)))
	 continue;
       
       if (!fixed_neighbor (cols->elem(i), ly_symbol2scm ("left-neighbors"))
	   || !fixed_neighbor (cols->elem(i), ly_symbol2scm ("right-neighbors")))
	 cols->del (i);
     }
}

void
Third_spacing_spanner::find_loose_columns (Link_array<Grob> cols)
{
  for (int i=0; i< cols.size(); i++)
    {
      SCM right_neighbors = SCM_EOL;
      int min_rank = 100000;	// inf.
      
      for (SCM s = cols[i]-> get_grob_property ("spacing-wishes");
	   gh_pair_p (s); s = gh_cdr (s))
	{
	  Grob * wish = unsmob_grob (gh_car (s));

	  Grob * left = unsmob_grob (wish->get_grob_property ("left-item"));
	  Grob * right = unsmob_grob (wish->get_grob_property ("right-item"));

	  Item * li = dynamic_cast<Item*> (left);
	  Item * ri = dynamic_cast<Item*> (right);	  

	  assert (li->column_l () == cols[i]);

	  Item * rc = ri->column_l ();
	  Item * lc = li->column_l ();
	  int newrank = Paper_column::rank_i (lc);

	  SCM neighbors = rc->get_grob_property ("left-neighbors");
	  Item  * left_neighbor = gh_pair_p (neighbors)
	    ? dynamic_cast<Item*> (unsmob_grob (gh_car (neighbors))) : 0;

	  left_neighbor = left_neighbor->column_l ();
	  if (left_neighbor)
	    {
	      int oldrank = Paper_column::rank_i (left_neighbor->column_l ());

	      if (newrank > oldrank)
		{
		  neighbors= gh_cons (wish->self_scm (), SCM_EOL);
		}
	      else if (newrank == oldrank)
		{
		  neighbors = gh_cons (wish->self_scm (), neighbors); 
		}
	    }

	  if (newrank < min_rank)
	    {
	      right_neighbors = gh_cons (wish->self_scm(), SCM_EOL);
	      min_rank = newrank;
	    }
	  else if (newrank == min_rank)
	    {
	      right_neighbors = gh_cons (wish->self_scm (), right_neighbors); 
	    }
	}

      cols[i]->set_grob_property ("right-neighbors", right_neighbors);
    }
}
