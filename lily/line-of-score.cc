/*
  scoreline.cc -- implement Line_of_score

  source file of the GNU LilyPond music typesetter

  (c) 1996--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "input-smob.hh"
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

// todo: use map.
void
fixup_refpoints (SCM s)
{
  for (; gh_pair_p (s); s = ly_cdr (s))
    {
      Grob::fixup_refpoint (ly_car (s));
    }
}


Line_of_score::Line_of_score (SCM s)
  : Spanner (s)
{
  rank_i_ = 0;

  Axis_group_interface::set_interface (this);
  Axis_group_interface::set_axes (this, Y_AXIS,X_AXIS);
}

int
Line_of_score::element_count () const
{
  return scm_ilength (get_grob_property ("all-elements"));
}

void
Line_of_score::typeset_grob (Grob * elem_p)
{
  elem_p->pscore_l_ = pscore_l_;
  Pointer_group_interface::add_element (this, ly_symbol2scm ("all-elements"),elem_p);
  scm_gc_unprotect_object (elem_p->self_scm ());
}

void
Line_of_score::output_lines ()
{
  for (SCM s = get_grob_property ("all-elements");
       gh_pair_p (s); s = ly_cdr (s))
    {
      unsmob_grob (ly_car (s))->do_break_processing ();
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
      Line_of_score *line_l = dynamic_cast<Line_of_score*> (broken_into_l_arr_[i]);

      if (verbose_global_b)
	progress_indication ("[");
      line_l->post_processing (i+1 == broken_into_l_arr_.size ());

      if (verbose_global_b)
	{
	  progress_indication (to_str (i));
	  progress_indication ("]");
	}

      if (i < broken_into_l_arr_.size () - 1)
	{
	  SCM lastcol =  ly_car (line_l->get_grob_property ("columns"));
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
set_loose_columns (Line_of_score* which, Column_x_positions const *posns)
{
  for (int i = 0; i<posns->loose_cols_.size (); i++)
    {
      int divide_over = 1;
      Item *loose = dynamic_cast<Item*> (posns->loose_cols_[i]);
      Paper_column* col = dynamic_cast<Paper_column*> (loose);
      
      if (col->line_l_)
	continue;

      
      Item * left = 0;
      Item * right = 0;
      while (1)
	{
	  SCM between = loose->get_grob_property ("between-cols");
	  if (!gh_pair_p (between))
	    break;

	  if (!left)
	    {
	      left = dynamic_cast<Item*> (unsmob_grob (ly_car (between)));
	      left = left->column_l ();
	    }
	  divide_over ++;	
	  loose = dynamic_cast<Item*> (unsmob_grob (ly_cdr (between)));
	  loose = loose->column_l ();
	}

      right = loose;

      Real rx = right->relative_coordinate (right->get_parent (X_AXIS), X_AXIS);
      Real lx = left->relative_coordinate (left->get_parent (X_AXIS), X_AXIS);

      int j = 1;
      loose = col;
      while (1)
	{
	  SCM between = loose->get_grob_property ("between-cols");
	  if (!gh_pair_p (between))
	    break;

	  Paper_column *thiscol = dynamic_cast<Paper_column*> (loose);

	  thiscol->line_l_ = which;
	  thiscol->translate_axis (lx + j*(rx - lx)/divide_over, X_AXIS);

	  j ++;	
	  loose = dynamic_cast<Item*> (unsmob_grob (ly_cdr (between)));
	}
      
    }
}

// const?
void
Line_of_score::break_into_pieces (Array<Column_x_positions> const &breaking)
{
  for (int i=0; i < breaking.size (); i++)
    {
      Line_of_score *line_l = dynamic_cast <Line_of_score*> (clone ());
      line_l->rank_i_ = i;
      //      line_l->set_immutable_grob_property ("rank", gh_int2scm (i));
      Link_array<Grob> c (breaking[i].cols_);
      pscore_l_->typeset_line (line_l);
      
      line_l->set_bound (LEFT,c[0]);
      line_l->set_bound (RIGHT,c.top ());
      for (int j=0; j < c.size (); j++)
	{
	  c[j]->translate_axis (breaking[i].config_[j],X_AXIS);
	  dynamic_cast<Paper_column*> (c[j])->line_l_ = line_l;
	}
      set_loose_columns (line_l, &breaking[i]);
      broken_into_l_arr_.push (line_l);
    }
}


#define GLOBAL_SYMBOL(cname, name)  \
SCM cname ;					\
void \
cname ## _init_func ()				\
{						\
  cname = ly_symbol2scm (name);			\
  scm_permanent_object (cname);			\
}						\
ADD_SCM_INIT_FUNC (cname,cname ## _init_func);\


GLOBAL_SYMBOL (offset_sym , "translate-molecule");
GLOBAL_SYMBOL (placebox_sym , "placebox");
GLOBAL_SYMBOL (combine_sym , "combine-molecule");
GLOBAL_SYMBOL (no_origin_sym , "no-origin");
GLOBAL_SYMBOL (define_origin_sym , "define-origin");



void
Line_of_score::output_molecule (SCM expr, Offset o)
{

  while (1)
    {
      if (!gh_pair_p (expr))
	return;
  
      SCM head =ly_car (expr);
      if (unsmob_input (head))
	{
	  Input * ip = unsmob_input (head);
      

	  pscore_l_->outputter_l_->output_scheme (scm_list_n (define_origin_sym,
							   ly_str02scm (ip->file_str ().ch_C ()),
							   gh_int2scm (ip->line_number ()),
							   gh_int2scm (ip->column_number ()),
							   SCM_UNDEFINED));
	  expr = ly_cadr (expr);
	}
      else  if (head ==  no_origin_sym)
	{
	  pscore_l_->outputter_l_->output_scheme (scm_list_n (no_origin_sym, SCM_UNDEFINED));
	  expr = ly_cadr (expr);
	}
      else if (head == offset_sym)
	{
	  o += ly_scm2offset (ly_cadr (expr));
	  expr = ly_caddr (expr);
	}
      else if (head == combine_sym)
	{
	  output_molecule (ly_cadr (expr), o);
	  expr = ly_caddr (expr);
	}
      else
	{
	  pscore_l_->outputter_l_->
	    output_scheme (scm_list_n (placebox_sym,
				    gh_double2scm (o[X_AXIS]),
				    gh_double2scm (o[Y_AXIS]),
				    expr,
				    SCM_UNDEFINED));

	  return;
	}
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
Line_of_score::pre_processing ()
{
  for (SCM s = get_grob_property ("all-elements"); gh_pair_p (s); s = ly_cdr (s))
    unsmob_grob (ly_car (s))->discretionary_processing ();

  if (verbose_global_b)
    progress_indication (_f ("Element count %d ",  element_count ()));

  
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
Line_of_score::post_processing (bool last_line)
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
  output_scheme (scm_list_n (ly_symbol2scm ("start-line"),
			  gh_double2scm (height),
			  SCM_UNDEFINED));
  
  /* Output elements in three layers, 0, 1, 2.
     The default layer is 1. */
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
      output_scheme (scm_list_n (ly_symbol2scm ("stop-last-line"), SCM_UNDEFINED));
    }
  else
    {
      output_scheme (scm_list_n (ly_symbol2scm ("stop-line"), SCM_UNDEFINED));
    }
}


Link_array<Item> 
Line_of_score::broken_col_range (Item const*l, Item const*r) const
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
      if (Item::breakable_b (c) && !c->line_l_)
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
Line_of_score::column_l_arr ()const
{
  Link_array<Grob> acs
    = Pointer_group_interface__extract_elements (this, (Grob*) 0, "columns");
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
  
