/*
  system.cc -- implement System

  source file of the GNU LilyPond music typesetter

  (c) 1996--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "input-smob.hh"
#include "axis-group-interface.hh"
#include "warn.hh"
#include "system.hh"
#include "main.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "paper-outputter.hh"
#include "paper-score.hh"
#include "string.hh"
#include "warn.hh"
#include "dimensions.hh"
#include "molecule.hh"
#include "all-font-metrics.hh"
#include "spacing-interface.hh"

// todo: use map.
void
fixup_refpoints (SCM s)
{
  for (; gh_pair_p (s); s = ly_cdr (s))
    {
      Grob::fixup_refpoint (ly_car (s));
    }
}


System::System (SCM s)
  : Spanner (s)
{
  rank_i_ = 0;
}

int
System::element_count () const
{
  return scm_ilength (get_grob_property ("all-elements"));
}

void
System::typeset_grob (Grob * elem_p)
{
  elem_p->pscore_l_ = pscore_l_;
  Pointer_group_interface::add_grob (this, ly_symbol2scm ("all-elements"),elem_p);
  scm_gc_unprotect_object (elem_p->self_scm ());
}

void
System::output_lines ()
{
  for (SCM s = get_grob_property ("all-elements");
       gh_pair_p (s); s = ly_cdr (s))
    {
      Grob * g = unsmob_grob (ly_car (s));
      if (Spacing_interface::has_interface (g))
	{
	  /*
	    Kill no longer needed grobs. 
	   */
	  Item * it = dynamic_cast<Item*> (g);
	  if (it && Item::breakable_b(it))
	    {
	      it->find_prebroken_piece (LEFT)->suicide();
	      it->find_prebroken_piece (RIGHT)->suicide();
	    }
	  g->suicide ();
	}
      else if (g->live ())
	g->do_break_processing ();
    }

  /*
    fixups must be done in broken line_of_scores, because new elements
    are put over there.  */
  int count = 0;
  for (int i=0; i < broken_into_l_arr_.size (); i++)
    {
      Grob *se = broken_into_l_arr_[i];
      SCM all = se->get_grob_property ("all-elements");
      for (SCM s = all; gh_pair_p (s); s = ly_cdr (s))
	{
	  fixup_refpoint (ly_car (s));
	}
      count += scm_ilength (all);
    }
  
  /*
    needed for doing items.
   */
  fixup_refpoints (get_grob_property ("all-elements"));

  
  for (SCM s = get_grob_property ("all-elements");
       gh_pair_p (s); s = ly_cdr (s))
    {
      unsmob_grob (ly_car (s))->handle_broken_dependencies ();
    }
  handle_broken_dependencies ();

  if (verbose_global_b)
    progress_indication (_f ("Element count %d.",  count + element_count ()));

  
  for (int i=0; i < broken_into_l_arr_.size (); i++)
    {
      System *system = dynamic_cast<System*> (broken_into_l_arr_[i]);

      if (verbose_global_b)
	progress_indication ("[");
      system->post_processing (i+1 == broken_into_l_arr_.size ());

      if (verbose_global_b)
	{
	  progress_indication (to_str (i));
	  progress_indication ("]");
	}

      if (i < broken_into_l_arr_.size () - 1)
	{
	  SCM lastcol =  ly_car (system->get_grob_property ("columns"));
	  Grob*  e = unsmob_grob (lastcol);

	  SCM between = ly_symbol2scm ("between-system-string");
	  SCM inter = e->internal_get_grob_property (between);
	  if (gh_string_p (inter))
	    {
	      pscore_l_->outputter_l_
		->output_scheme (scm_list_n (between, 
					     inter, SCM_UNDEFINED));	      
	    }
	}
    }
}




/*
  Find the loose columns in POSNS, and drape them around the columns
  specified in BETWEEN-COLS.  */
void
set_loose_columns (System* which, Column_x_positions const *posns)
{
  for (int i = 0; i < posns->loose_cols_.size (); i++)
    {
      int divide_over = 1;
      Item *loose = dynamic_cast<Item*> (posns->loose_cols_[i]);
      Paper_column* col = dynamic_cast<Paper_column*> (loose);
      
      if (col->system_)
	continue;

      
      Item * left = 0;
      Item * right = 0;
      do
	{
	  SCM between = loose->get_grob_property ("between-cols");
	  if (!gh_pair_p (between))
	    break;


	  Item * l=dynamic_cast<Item*> (unsmob_grob (ly_car (between)));
	  Item * r=dynamic_cast<Item*> (unsmob_grob (ly_cdr (between)));

	  if (!(l && r))
	    break ;
	  
	  if (!left && l)
	    {
	      left = l->column_l ();
	    }

	  divide_over ++;

	  loose = right = r->column_l ();
	}
      while (1);
      
    
#if 0
      Real rx = right->relative_coordinate (right->get_parent (X_AXIS), X_AXIS);
      Real lx = left->relative_coordinate (left->get_parent (X_AXIS), X_AXIS);

      /*
	divide space equally over loose columns.
       */
      int j = 1;
      loose = col;
      while (1)
	{
	  SCM between = loose->get_grob_property ("between-cols");
	  if (!gh_pair_p (between))
	    break;

	  Paper_column *thiscol = dynamic_cast<Paper_column*> (loose);

	  thiscol->system_ = which;
	  thiscol->translate_axis (lx + j*(rx - lx)/divide_over, X_AXIS);

	  j ++;	
	  loose = dynamic_cast<Item*> (unsmob_grob (ly_cdr (between)));
	}
#else
      /*
	We divide the remaining space of the column over the left and
	right side. At the moment, we  
	
      */
      Grob * common = right->common_refpoint (left, X_AXIS);
      
      Real rx =	right->extent(common, X_AXIS)[LEFT];
      Real lx =  left->extent(common, X_AXIS)[RIGHT];
      Real total_dx = rx - lx;
      Interval cval =col->extent (col, X_AXIS);

      /*
	
	We put it in the middle. This is not an ideal solution -- the
	break alignment code inserts a fixed space before the clef
	(about 1 SS), while the space following the clef is
	flexible. In tight situations, the clef will almost be on top
	of the following note. 
	
      */
      Real dx = rx-lx - cval.length ();
      if (total_dx < 2* cval.length ())
	{
	  /*
	    todo: this is discontinuous. I'm too tired to
	    invent a sliding mechanism. Duh.

	    TODO.
	   */
	  dx *= 0.25;
	}
      else
	dx *= 0.5;

      col->system_ = which;
      col->translate_axis (lx + dx - cval[LEFT], X_AXIS); 
#endif
    }
}

// const?
void
System::break_into_pieces (Array<Column_x_positions> const &breaking)
{
  for (int i=0; i < breaking.size (); i++)
    {
      System *system = dynamic_cast <System*> (clone ());
      system->rank_i_ = i;
      //      system->set_immutable_grob_property ("rank", gh_int2scm (i));
      Link_array<Grob> c (breaking[i].cols_);
      pscore_l_->typeset_line (system);
      
      system->set_bound (LEFT,c[0]);
      system->set_bound (RIGHT,c.top ());
      for (int j=0; j < c.size (); j++)
	{
	  c[j]->translate_axis (breaking[i].config_[j],X_AXIS);
	  dynamic_cast<Paper_column*> (c[j])->system_ = system;
	}
      set_loose_columns (system, &breaking[i]);
      broken_into_l_arr_.push (system);
    }
}


void
System::output_molecule (SCM expr, Offset o)
{

  while (1)
    {
      if (!gh_pair_p (expr))
	return;
  
      SCM head =ly_car (expr);
      if (unsmob_input (head))
	{
	  Input * ip = unsmob_input (head);
      

	  pscore_l_->outputter_l_->output_scheme (scm_list_n (ly_symbol2scm ("define-origin"),
							   ly_str02scm (ip->file_str ().ch_C ()),
							   gh_int2scm (ip->line_number ()),
							   gh_int2scm (ip->column_number ()),
							   SCM_UNDEFINED));
	  expr = ly_cadr (expr);
	}
      else  if (head ==  ly_symbol2scm ("no-origin"))
	{
	  pscore_l_->outputter_l_->output_scheme (scm_list_n (head, SCM_UNDEFINED));
	  expr = ly_cadr (expr);
	}
      else if (head == ly_symbol2scm ("translate-molecule"))
	{
	  o += ly_scm2offset (ly_cadr (expr));
	  expr = ly_caddr (expr);
	}
      else if (head == ly_symbol2scm ("combine-molecule"))
	{
	  output_molecule (ly_cadr (expr), o);
	  expr = ly_caddr (expr);
	}
      else
	{
	  pscore_l_->outputter_l_->
	    output_scheme (scm_list_n (ly_symbol2scm ("placebox"),
				    gh_double2scm (o[X_AXIS]),
				    gh_double2scm (o[Y_AXIS]),
				    expr,
				    SCM_UNDEFINED));

	  return;
	}
    }
}

void
System::output_scheme (SCM s)
{
  pscore_l_->outputter_l_->output_scheme (s);
}

void
System::add_column (Paper_column*p)
{
  Grob *me = this;
  SCM cs = me->get_grob_property ("columns");
  Grob * prev =  gh_pair_p (cs) ? unsmob_grob (ly_car (cs)) : 0;

  p->rank_i_ = prev ? Paper_column::rank_i (prev) + 1 : 0; 

  me->set_grob_property ("columns",  gh_cons (p->self_scm (), cs));

  Axis_group_interface::add_element (me, p);
}



/*
  TODO: use scm_map iso. for loops.
 */
void
System::pre_processing ()
{
  for (SCM s = get_grob_property ("all-elements"); gh_pair_p (s); s = ly_cdr (s))
    unsmob_grob (ly_car (s))->discretionary_processing ();

  if (verbose_global_b)
    progress_indication (_f ("Grob count %d ",  element_count ()));

  
  for (SCM s = get_grob_property ("all-elements"); gh_pair_p (s); s = ly_cdr (s))
    unsmob_grob (ly_car (s))->handle_prebroken_dependencies ();
  
  fixup_refpoints (get_grob_property ("all-elements"));
  
  for (SCM s = get_grob_property ("all-elements"); gh_pair_p (s); s = ly_cdr (s))
    {
      Grob* sc = unsmob_grob (ly_car (s));
      sc->calculate_dependencies (PRECALCED, PRECALCING, ly_symbol2scm ("before-line-breaking-callback"));
    }
  
  progress_indication ("\n" + _ ("Calculating column positions...") + " ");
  for (SCM s = get_grob_property ("all-elements"); gh_pair_p (s); s = ly_cdr (s))
    {
      Grob * e = unsmob_grob (ly_car (s));
      SCM proc = e->get_grob_property ("spacing-procedure");
      if (gh_procedure_p (proc))
	gh_call1 (proc, e->self_scm ());
    }
}

void
System::post_processing (bool last_line)
{
  for (SCM s = get_grob_property ("all-elements");
       gh_pair_p (s); s = ly_cdr (s))
    {
      Grob* sc = unsmob_grob (ly_car (s));
      sc->calculate_dependencies (POSTCALCED, POSTCALCING,
				  ly_symbol2scm ("after-line-breaking-callback"));
    }

  Interval i (extent (this, Y_AXIS));
  if (i.empty_b ())
    programming_error ("Huh?  Empty System?");
  else
    translate_axis (- i[MAX], Y_AXIS);

  Real height = i.length ();
  if (height > 50 CM)
    {
      programming_error ("Improbable system height");
      height = 50 CM;
    }

  /*
    generate all molecules  to trigger all font loads.

    (ugh. This is not very memory efficient.)  */
  this->get_molecule();
  for (SCM s = get_grob_property ("all-elements"); gh_pair_p (s); s = ly_cdr (s))
    {
      unsmob_grob (ly_car (s))->get_molecule ();
    }
  /*
    font defs;
   */
  SCM font_names = ly_quote_scm (paper_l ()->font_descriptions ());  
  output_scheme (scm_list_n (ly_symbol2scm ("define-fonts"),
			     font_names,
			     SCM_UNDEFINED));

  /*
    line preamble.
   */
  output_scheme (scm_list_n (ly_symbol2scm ("start-system"),
			  gh_double2scm (height),
			  SCM_UNDEFINED));
  
  /* Output elements in three layers, 0, 1, 2.
     The default layer is 1. */

  {
    Molecule *m = this->get_molecule();
    if (m)
      output_molecule (m->get_expr (), Offset(0,0));
  }
  
  for (int i = 0; i < 3; i++)
    for (SCM s = get_grob_property ("all-elements"); gh_pair_p (s);
	 s = ly_cdr (s))
      {
	Grob *sc = unsmob_grob (ly_car (s));
	Molecule *m = sc->get_molecule ();
	if (!m)
	  continue;
	
	SCM s = sc->get_grob_property ("layer");
	int layer = gh_number_p (s) ? gh_scm2int (s) : 1;
	if (layer != i)
	  continue;
	
	Offset o (sc->relative_coordinate (this, X_AXIS),
		  sc->relative_coordinate (this, Y_AXIS));
	
	SCM e = sc->get_grob_property ("extra-offset");
	if (gh_pair_p (e))
	  {
	    o[X_AXIS] += gh_scm2double (ly_car (e));
	    o[Y_AXIS] += gh_scm2double (ly_cdr (e));      
	  }
	
	output_molecule (m->get_expr (), o);
      }

  
  
  if (last_line)
    {
      output_scheme (scm_list_n (ly_symbol2scm ("stop-last-system"), SCM_UNDEFINED));
    }
  else
    {
      output_scheme (scm_list_n (ly_symbol2scm ("stop-system"), SCM_UNDEFINED));
    }
}


Link_array<Item> 
System::broken_col_range (Item const*l, Item const*r) const
{
  Link_array<Item> ret;

  l = l->column_l ();
  r = r->column_l ();
  SCM s = get_grob_property ("columns");

  while (gh_pair_p (s) && ly_car (s) != r->self_scm ())
    s = ly_cdr (s);
    
  if (gh_pair_p (s))
    s = ly_cdr (s);
  
  while (gh_pair_p (s) && ly_car (s) != l->self_scm ())
    {
      Paper_column*c = dynamic_cast<Paper_column*> (unsmob_grob (ly_car (s)));
      if (Item::breakable_b (c) && !c->system_)
	ret.push (c);

      s = ly_cdr (s);
    }

  ret.reverse ();
  return ret;
}

/**
   Return all columns, but filter out any unused columns , since they might
   disrupt the spacing problem.
 */
Link_array<Grob>
System::column_l_arr ()const
{
  Link_array<Grob> acs
    = Pointer_group_interface__extract_grobs (this, (Grob*) 0, "columns");
  bool bfound = false;
  for (int i= acs.size (); i -- ;)
    {
      bool brb = Item::breakable_b (acs[i]);
      bfound = bfound || brb;

      /*
	the last column should be breakable. Weed out any columns that
	seem empty. We need to retain breakable columns, in case
	someone forced a breakpoint.
      */
      if (!bfound || !Paper_column::used_b (acs[i]))
	acs.del (i);
    }
  return acs;
}
  



ADD_INTERFACE (System,"system-interface",
  "Super grob, parent of all:

The columns of a score that form one line.  The toplevel grob.  Any
grob has a Line_of_score as both X and Y reference point. The
Paper_score contains one grob of this type. Control enters the
Grob dependency calculation from this single Line_of_score
object.",
  "between-system-string all-elements columns");
