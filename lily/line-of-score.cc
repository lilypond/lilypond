/*
  scoreline.cc -- implement Line_of_score

  source file of the GNU LilyPond music typesetter

  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "axis-group-interface.hh"
#include "debug.hh"
#include "line-of-score.hh"
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

Line_of_score::Line_of_score()
  : Spanner (SCM_EOL)
{
  set_elt_pointer ("columns", SCM_EOL);
  set_elt_pointer ("all-elements", SCM_EOL);

  Axis_group_interface (this).set_interface ();
  Axis_group_interface (this).set_axes (Y_AXIS,X_AXIS);
}

int
Line_of_score::element_count () const
{
  return scm_ilength ( get_elt_pointer ("all-elements"));
}

void
Line_of_score::typeset_element (Score_element * elem_p)
{
  elem_p->pscore_l_ = pscore_l_;
  Pointer_group_interface (this, "all-elements").add_element (elem_p);
  scm_unprotect_object (elem_p->self_scm_);
}

void
Line_of_score::output_lines ()
{
  for (SCM s = get_elt_pointer ("all-elements");
       gh_pair_p (s); s = gh_cdr (s))
    {
      unsmob_element (gh_car (s))->do_break_processing ();
    }
  /*
    fixups must be done in broken line_of_scores, because new elements
    are put over there.  */
  int count = 0;
  for (int i=0; i < broken_into_l_arr_.size (); i++)
    {
      Score_element *se = broken_into_l_arr_[i];
      SCM all = se->get_elt_pointer ("all-elements");
      for (SCM s = all; gh_pair_p (s); s = gh_cdr (s))
	{
	  unsmob_element (gh_car (s))->fixup_refpoint ();
	}
      count += scm_ilength (all);
    }

  
  /*
    needed for doing items.
   */
  for (SCM s = get_elt_pointer ("all-elements");
       gh_pair_p (s); s = gh_cdr (s))
    {
      unsmob_element (gh_car (s))->fixup_refpoint ();
    }
  
  for (SCM s = get_elt_pointer ("all-elements");
       gh_pair_p (s); s = gh_cdr (s))
    {
      unsmob_element (gh_car (s))->handle_broken_dependencies ();
    }
  handle_broken_dependencies ();
  progress_indication ( _f("Element count %d.",  count + element_count()));

  
  for (int i=0; i < broken_into_l_arr_.size (); i++)
    {
      Line_of_score *line_l = dynamic_cast<Line_of_score*> (broken_into_l_arr_[i]);

      progress_indication ("[");
      line_l->post_processing ();
      progress_indication (to_str (i));
      progress_indication ("]");
    }
}

// const?
void
Line_of_score::break_into_pieces (Array<Column_x_positions> const &breaking)
{
  for (int i=0; i < breaking.size (); i++)
    {
      Line_of_score *line_l = dynamic_cast <Line_of_score*> (clone());
      line_l->rank_i_ = i;
      Link_array<Paper_column> c (breaking[i].cols_);
      pscore_l_->typeset_line (line_l);
      
      line_l->set_bound(LEFT,c[0]);
      line_l->set_bound(RIGHT,c.top ());
      for (int j=0; j < c.size(); j++)
	{
	  c[j]->translate_axis (breaking[i].config_[j],X_AXIS);
	  c[j]->line_l_ = line_l;
	}
      
      broken_into_l_arr_.push (line_l);
    }
}


void
Line_of_score::output_molecule (SCM expr, Offset o)
{
  SCM offset_sym = ly_symbol2scm ("translate-molecule");
  SCM combine_sym = ly_symbol2scm ("combine-molecule");
enter:

  if (!gh_pair_p (expr))
    return;
  
  SCM head =gh_car (expr);
  if (head == offset_sym)
    {
      o += ly_scm2offset (gh_cadr (expr));
      expr = gh_caddr (expr);
      goto enter;
    }
  else if (head == combine_sym)
    {
      output_molecule (gh_cadr (expr), o);
      expr = gh_caddr (expr);
      goto enter;		// tail recursion
    }
  else
    {
      pscore_l_->outputter_l_->
	output_scheme (gh_list (ly_symbol2scm ("placebox"),
				gh_double2scm (o[X_AXIS]),
				gh_double2scm (o[Y_AXIS]),
				expr,
				SCM_UNDEFINED));
    }
}

void
Line_of_score::output_scheme (SCM s)
{
  pscore_l_->outputter_l_->output_scheme (s);
}

void
Line_of_score::add_column (Paper_column*p)
{
  SCM cs = get_elt_pointer ("columns");
  Score_element * prev =  gh_pair_p (cs) ? unsmob_element (gh_car (cs)) : 0;
  int rank = prev ? dynamic_cast<Paper_column*> (prev)->rank_i () + 1 : 0; 

  p->set_rank (rank);
  set_elt_pointer ("columns",  gh_cons (p->self_scm_, cs));

  Axis_group_interface (this).add_element (p);
  typeset_element (p);
}


void
fixup_refpoints (SCM s)
{
  for (; gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element * se = unsmob_element (gh_car (s));
      if (se)
	{
	  se->fixup_refpoint ();
	  if (!dynamic_cast<Line_of_score*> (se) && !se->parent_l (Y_AXIS))
	    {
	      programming_error ("No parent!");
	    }
	}
    }
}


void
Line_of_score::pre_processing ()
{
  for (SCM s = get_elt_pointer ("all-elements"); gh_pair_p (s); s = gh_cdr (s))
    unsmob_element (gh_car (s))->discretionary_processing ();

  progress_indication ( _f("Element count %d ",  element_count ()));

  
  for (SCM s = get_elt_pointer ("all-elements"); gh_pair_p (s); s = gh_cdr (s))
    unsmob_element (gh_car (s))->handle_prebroken_dependencies ();
  
  fixup_refpoints (get_elt_pointer ("all-elements"));
  
  for (SCM s = get_elt_pointer ("all-elements"); gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element* sc = unsmob_element (gh_car (s));
      sc->calculate_dependencies (PRECALCED, PRECALCING, &Score_element::before_line_breaking);
    }
  
  progress_indication ("\n" + _ ("Calculating column positions...") + " " );
  for (SCM s = get_elt_pointer ("all-elements"); gh_pair_p (s); s = gh_cdr (s))
    unsmob_element (gh_car (s))->do_space_processing ();
}

void
Line_of_score::post_processing ()
{
  for (SCM s = get_elt_pointer ("all-elements");
       gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element* sc = unsmob_element (gh_car (s));
      sc->calculate_dependencies (POSTCALCED, POSTCALCING, &Score_element::after_line_breaking);
    }

  Interval i(extent(Y_AXIS));
  if (i.empty_b())
    programming_error ("Huh?  Empty Line_of_score?");
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
  for (SCM s = get_elt_pointer ("all-elements"); gh_pair_p (s); s = gh_cdr (s))
    unsmob_element (gh_car (s))->get_molecule ();
  
  /*
    font defs;
   */
  SCM font_names = ly_quote_scm (all_fonts_global_p->font_descriptions ());  
  output_scheme (gh_list (ly_symbol2scm ("define-fonts"),
					font_names,
					SCM_UNDEFINED));

  /*
    line preamble.
   */
  output_scheme (gh_list (ly_symbol2scm ("start-line"),
			  gh_double2scm (height),
			  SCM_UNDEFINED));
  
  Real il = paper_l ()->get_var ("interline");

  /*
    all elements.
   */ 
  for (SCM s = get_elt_pointer ("all-elements"); gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element * sc =  unsmob_element (gh_car (s));
      Molecule m = sc->get_molecule ();
      
      Offset o (sc->relative_coordinate (this, X_AXIS),
		sc->relative_coordinate (this, Y_AXIS));

      SCM e = sc->get_elt_property ("extra-offset");
      if (gh_pair_p (e))
	{
	  o[X_AXIS] += il * gh_scm2double (gh_car (e));
	  o[Y_AXIS] += il * gh_scm2double (gh_cdr (e));      
	}

      output_molecule (m.get_expr (), o);
    }
  output_scheme (gh_list (ly_symbol2scm ("stop-line"), SCM_UNDEFINED));
}


Link_array<Item> 
Line_of_score::broken_col_range (Item const*l, Item const*r) const
{
  Link_array<Item> ret;

  l = l->column_l ();
  r = r->column_l ();
  SCM s = get_elt_pointer ("columns");

  while (gh_pair_p (s) && gh_car (s) != r->self_scm_)
    s = gh_cdr  (s);
    
  if (gh_pair_p (s))
    s = gh_cdr (s);
  
  while (gh_pair_p (s) && gh_car (s) != l->self_scm_)
    {
      Paper_column *c
	= dynamic_cast<Paper_column*> (unsmob_element (gh_car (s)));
      if (c->breakable_b () && !c->line_l_)
	ret.push (c);

      s = gh_cdr  (s);
    }

  ret.reverse ();
  return ret;
}

/**
   Return all columns, but filter out any unused columns , since they might
   disrupt the spacing problem.
 */
Link_array<Paper_column>
Line_of_score::column_l_arr ()const
{
  Link_array<Paper_column> acs
    = Pointer_group_interface__extract_elements (this, (Paper_column*) 0, "columns");
  bool bfound = false;
  for (int i= acs.size (); i -- ; )
    {
      bool brb = acs[i]->breakable_b();
      bfound = bfound || brb;

      /*
	the last column should be breakable. Weed out any columns that
	seem empty. We need to retain breakable columns, in case
	someone forced a breakpoint.
      */
      if (!bfound || !acs[i]->used_b ())
	acs.del (i);
    }
  return acs;
}
