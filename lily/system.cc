/*
  system.cc -- implement System

  source file of the GNU LilyPond music typesetter

  (c) 1996--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "system.hh"

#include <math.h>

#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "warn.hh"
#include "main.hh"
#include "paper-column.hh"
#include "output-def.hh"
#include "paper-score.hh"
#include "warn.hh"
#include "all-font-metrics.hh"
#include "spacing-interface.hh"
#include "staff-symbol-referencer.hh"
#include "paper-book.hh"
#include "paper-system.hh"
#include "tweak-registration.hh"


System::System (System const &src, int count)
  : Spanner (src, count)
{
  rank_ = 0;
}

System::System (SCM s, Object_key const*key)
  : Spanner (s, key)
{
  rank_ = 0;
}


Grob * 
System::clone (int count) const
{
  return new System (*this, count);
}


int
System::element_count () const
{
  return scm_ilength (get_property ("all-elements"));
}

int
System::spanner_count () const
{
  int k = 0;
  for (SCM s = get_property ("all-elements"); scm_is_pair (s); s = scm_cdr (s))
    if (dynamic_cast<Spanner*> (unsmob_grob (scm_car (s))))
      k++;
  return k;
}

void
System::typeset_grob (Grob * elem)
{
  if (elem->pscore_)
    programming_error ("Adding element twice.");
  else
    {
      elem->pscore_ = pscore_;
      Pointer_group_interface::add_grob (this, ly_symbol2scm ("all-elements"), elem);
      scm_gc_unprotect_object (elem->self_scm ());
    }
}

// todo: use map.
static void
fixup_refpoints (SCM s)
{
  for (; scm_is_pair (s); s = scm_cdr (s))
    {
      Grob::fixup_refpoint (scm_car (s));
    }
}

SCM
System::get_lines ()
{
  for (SCM s = get_property ("all-elements"); scm_is_pair (s); s = scm_cdr (s))
    {
      Grob *g = unsmob_grob (scm_car (s));
      if (g->internal_has_interface (ly_symbol2scm ("only-prebreak-interface")))
	{
	  /*
	    Kill no longer needed grobs. 
	   */
	  Item * it = dynamic_cast<Item*> (g);
	  if (it && Item::is_breakable (it))
	    {
	      it->find_prebroken_piece (LEFT)->suicide ();
	      it->find_prebroken_piece (RIGHT)->suicide ();
	    }
	  g->suicide ();
	}
      else if (g->is_live ())
	g->do_break_processing ();
    }

  /*
    fixups must be done in broken line_of_scores, because new elements
    are put over there.  */
  int count = 0;
  for (int i = 0; i < broken_intos_.size (); i++)
    {
      Grob *se = broken_intos_[i];
      SCM all = se->get_property ("all-elements");
      for (SCM s = all; scm_is_pair (s); s = scm_cdr (s))
	fixup_refpoint (scm_car (s));
      count += scm_ilength (all);
    }
  
  /*
    needed for doing items.
   */
  fixup_refpoints (get_property ("all-elements"));

  for (SCM s = get_property ("all-elements"); scm_is_pair (s); s = scm_cdr (s))
    unsmob_grob (scm_car (s))->handle_broken_dependencies ();
  handle_broken_dependencies ();

#if 0  /* don't do this: strange side effects.  */
  
  /* Because the this->get_property (all-elements) contains items in 3
     versions, handle_broken_dependencies () will leave duplicated
     items in all-elements.  Strictly speaking this is harmless, but
     it leads to duplicated symbols in the output.  ly_list_qsort_uniq_x ()
     makes sure that no duplicates are in the list.  */
  for (int i = 0; i < line_count; i++)
    {
      SCM all = broken_intos_[i]->get_property ("all-elements");
      all = ly_list_qsort_uniq_x(all); 
    }
#endif
  
  if (be_verbose_global)
    progress_indication (_f ("Element count %d.",  count + element_count ()));

  int line_count = broken_intos_.size ();
  SCM lines = scm_c_make_vector (line_count, SCM_EOL);
  
  for (int i = 0; i < line_count; i++)
    {
      if (be_verbose_global)
	progress_indication ("[");

      System *system = dynamic_cast<System*> (broken_intos_[i]);
      system->post_processing ();
      scm_vector_set_x (lines, scm_int2num (i), system->get_line ());

      if (be_verbose_global)
	progress_indication (to_string (i) + "]");
    }
  return lines;
}

/* Find the loose columns in POSNS, and drape them around the columns
   specified in BETWEEN-COLS.  */
static void
set_loose_columns (System* which, Column_x_positions const *posns)
{
  int loose_col_count = posns->loose_cols_.size ();
  for (int i = 0; i < loose_col_count; i++)
    {
      int divide_over = 1;
      Item *loose = dynamic_cast<Item*> (posns->loose_cols_[i]);
      Paper_column* col = dynamic_cast<Paper_column*> (loose);
      
      if (col->system_)
	continue;
      
      Item *left = 0;
      Item *right = 0;
      while (1)
	{
	  SCM between = loose->get_property ("between-cols");
	  if (!scm_is_pair (between))
	    break;

	  Item *le = dynamic_cast<Item*> (unsmob_grob (scm_car (between)));
	  Item *re = dynamic_cast<Item*> (unsmob_grob (scm_cdr (between)));

	  if (!(le && re))
	    break;
	  
	  if (!left && le)
	    {
	      left = le->get_column ();
	      if (!left->get_system ())
		left = left->find_prebroken_piece (RIGHT);
	    }

	  divide_over++;
	  loose = right = re->get_column ();
	}

      if (!right->get_system ())
	right = right->find_prebroken_piece (LEFT);
      
      /* Divide the remaining space of the column over the left and
	right side.  At the moment,  FIXME  */
      Grob *common = right->common_refpoint (left, X_AXIS);
      
      Real rx =	right->extent (common, X_AXIS)[LEFT];
      Real lx = left->extent (common, X_AXIS)[RIGHT];
      Real total_dx = rx - lx;
      Interval cval = col->extent (col, X_AXIS);

      /* Put it in the middle.  This is not an ideal solution -- the
	 break alignment code inserts a fixed space before the clef
	 (about 1 SS), while the space following the clef is flexible.
	 In tight situations, the clef will almost be on top of the
	 following note.  */
      Real dx = rx - lx - cval.length ();
      if (total_dx < 2* cval.length ())
	{
	  /* TODO: this is discontinuous. I'm too tired to
	    invent a sliding mechanism.  Duh. */
	  dx *= 0.25;
	}
      else
	dx *= 0.5;

      col->system_ = which;
      col->translate_axis (-col->relative_coordinate (common, X_AXIS), X_AXIS);
      col->translate_axis (lx + dx - cval[LEFT], X_AXIS); 
    }
}

// const?
void
System::break_into_pieces (Array<Column_x_positions> const &breaking)
{
  for (int i = 0; i < breaking.size (); i++)
    {
      System *system = dynamic_cast <System*> (clone (i));
      system->rank_ = i;

      Link_array<Grob> c (breaking[i].cols_);
      pscore_->typeset_line (system);
      
      system->set_bound (LEFT,c[0]);
      system->set_bound (RIGHT,c.top ());
      for (int j = 0; j < c.size (); j++)
	{
	  c[j]->translate_axis (breaking[i].config_[j], X_AXIS);
	  dynamic_cast<Paper_column*> (c[j])->system_ = system;
	}
      set_loose_columns (system, &breaking[i]);
      broken_intos_.push (system);
    }
}

void
System::add_column (Paper_column*p)
{
  Grob *me = this;
  SCM cs = me->get_property ("columns");
  Grob *prev =  scm_is_pair (cs) ? unsmob_grob (scm_car (cs)) : 0;

  p->rank_ = prev ? Paper_column::get_rank (prev) + 1 : 0; 

  me->set_property ("columns", scm_cons (p->self_scm (), cs));

  Axis_group_interface::add_element (me, p);
}

void
apply_tweaks (Grob *g, bool broken)
{
  if (bool (g->original_) == broken)
    {
      SCM tweaks = global_registry_->get_tweaks (g);
      for (SCM s = tweaks; scm_is_pair (s); s = scm_cdr (s))
	{
	  SCM proc = scm_caar (s);
	  SCM rest = scm_cdar (s);
	  scm_apply_1 (proc, g->self_scm(), rest);
	}
    }
}

void
System::pre_processing ()
{
  for (SCM s = get_property ("all-elements"); scm_is_pair (s); s = scm_cdr (s))
    unsmob_grob (scm_car (s))->discretionary_processing ();

  if (be_verbose_global)
    progress_indication (_f ("Grob count %d", element_count ()));
  
  for (SCM s = get_property ("all-elements"); scm_is_pair (s); s = scm_cdr (s))
    unsmob_grob (scm_car (s))->handle_prebroken_dependencies ();
  
  fixup_refpoints (get_property ("all-elements"));

  
  for (SCM s = get_property ("all-elements"); scm_is_pair (s); s = scm_cdr (s))
    apply_tweaks (unsmob_grob (scm_car (s)), false);

  for (SCM s = get_property ("all-elements"); scm_is_pair (s); s = scm_cdr (s))
    {
      Grob *sc = unsmob_grob (scm_car (s));
      sc->calculate_dependencies (PRECALCED, PRECALCING, ly_symbol2scm ("before-line-breaking-callback"));
    }
  
  progress_indication ("\n");
  progress_indication (_ ("Calculating line breaks..."));
  progress_indication (" ");
  for (SCM s = get_property ("all-elements"); scm_is_pair (s); s = scm_cdr (s))
    {
      Grob *e = unsmob_grob (scm_car (s));
      SCM proc = e->get_property ("spacing-procedure");
      if (ly_c_procedure_p (proc))
	scm_call_1 (proc, e->self_scm ());
    }
}

void
System::post_processing ()
{
  for (SCM s = get_property ("all-elements"); scm_is_pair (s); s = scm_cdr (s))
    {
      Grob *g = unsmob_grob (scm_car (s));
      
      apply_tweaks (g, true);

      g->calculate_dependencies (POSTCALCED, POSTCALCING,
          ly_symbol2scm ("after-line-breaking-callback"));
    }

  Interval iv (extent (this, Y_AXIS));
  if (iv.is_empty ())
    programming_error ("System with zero extent.");
  else
    translate_axis (-iv[MAX], Y_AXIS);
  
  /* Generate all stencils to trigger font loads.
     This might seem inefficient, but Stencils are cached per grob
     anyway. */
  SCM all = get_property ("all-elements");
  all = ly_list_qsort_uniq_x (all);

  this->get_stencil ();
  for (SCM s = all; scm_is_pair (s); s = scm_cdr (s))
    {
      Grob *g = unsmob_grob (scm_car (s));
      g->get_stencil ();
    }
}

SCM
System::get_line ()
{  
  static int const LAYER_COUNT = 3;

  SCM exprs = SCM_EOL;
  SCM *tail = &exprs;

  /* Output stencils in three layers: 0, 1, 2.  Default layer: 1.

     Start with layer 3, since scm_cons prepends to list.  */
  SCM all = get_property ("all-elements");
  
  for (int i = LAYER_COUNT; i--;)
    for (SCM s = all; scm_is_pair (s); s = scm_cdr (s))
      {
	Grob *g = unsmob_grob (scm_car (s));
	Stencil *stil = g->get_stencil ();

	/* Skip empty stencils and grobs that are not in this layer.  */
	if (!stil
	    || robust_scm2int (g->get_property ("layer"), 1) != i)
	  continue;

	Offset o (g->relative_coordinate (this, X_AXIS),
		  g->relative_coordinate (this, Y_AXIS));

	Offset extra = robust_scm2offset (g->get_property ("extra-offset"),
					  Offset (0, 0))
	  * Staff_symbol_referencer::staff_space (g);

	/* Must copy the stencil, for we cannot change the stencil
	   cached in G.  */

	Stencil st = *stil;
	st.translate (o + extra);
	*tail = scm_cons (st.expr (), SCM_EOL);
	tail = SCM_CDRLOC(*tail);
      }

  if (Stencil *me = get_stencil ())
    exprs = scm_cons (me->expr (), exprs);

  Interval x (extent (this, X_AXIS));
  Interval y (extent (this, Y_AXIS));
  Stencil sys_stencil (Box (x,y),
		       scm_cons (ly_symbol2scm ("combine-stencil"),
				 exprs));

  Interval staff_refpoints;
  staff_refpoints.set_empty();
  for (SCM s = get_property ("spaceable-staves");
       scm_is_pair (s); s = scm_cdr (s))
      {
	Grob *g = unsmob_grob (scm_car (s));
	staff_refpoints.add_point (g->relative_coordinate (this, Y_AXIS));
      }
  
 
  Paper_system *pl = new Paper_system (sys_stencil, false);
  pl->staff_refpoints_ = staff_refpoints;
  Item * break_point = this->get_bound(LEFT);
  pl->break_before_penalty_ =
    robust_scm2double (break_point->get_property ("page-penalty"), 0.0);
  
  return scm_gc_unprotect_object (pl->self_scm ());
}

Link_array<Item> 
System::broken_col_range (Item const *left, Item const *right) const
{
  Link_array<Item> ret;

  left = left->get_column ();
  right = right->get_column ();
  SCM s = get_property ("columns");

  while (scm_is_pair (s) && scm_car (s) != right->self_scm ())
    s = scm_cdr (s);

  if (scm_is_pair (s))
    s = scm_cdr (s);

  while (scm_is_pair (s) && scm_car (s) != left->self_scm ())
    {
      Paper_column*c = dynamic_cast<Paper_column*> (unsmob_grob (scm_car (s)));
      if (Item::is_breakable (c) && !c->system_)
	ret.push (c);

      s = scm_cdr (s);
    }

  ret.reverse ();
  return ret;
}

/** Return all columns, but filter out any unused columns , since they might
    disrupt the spacing problem. */
Link_array<Grob>
System::columns () const
{
  Link_array<Grob> acs
    = Pointer_group_interface__extract_grobs (this, (Grob*) 0, "columns");
  bool found = false;
  for (int i = acs.size (); i--;)
    {
      bool brb = Item::is_breakable (acs[i]);
      found = found || brb;

      /*
	the last column should be breakable. Weed out any columns that
	seem empty. We need to retain breakable columns, in case
	someone forced a breakpoint.
      */
      if (!found || !Paper_column::is_used (acs[i]))
	acs.del (i);
    }
  return acs;
}

ADD_INTERFACE (System,"system-interface",
	       "This is the toplevel object: each object in a score "
	       "ultimately has a System object as its X and Y parent. ",
	       "all-elements spaceable-staves columns")
