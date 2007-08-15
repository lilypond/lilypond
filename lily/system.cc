/*
  system.cc -- implement System

  source file of the GNU LilyPond music typesetter

  (c) 1996--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "system.hh"

#include "align-interface.hh"
#include "all-font-metrics.hh"
#include "axis-group-interface.hh"
#include "grob-array.hh"
#include "international.hh"
#include "main.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "paper-system.hh"
#include "pointer-group-interface.hh"
#include "spacing-interface.hh"
#include "staff-symbol-referencer.hh"
#include "tweak-registration.hh"
#include "warn.hh"

System::System (System const &src, int count)
  : Spanner (src, count)
{
  all_elements_ = 0;
  pscore_ = 0;
  rank_ = 0;
  init_elements ();
}

System::System (SCM s, Object_key const *key)
  : Spanner (s, key)
{
  all_elements_ = 0;
  rank_ = 0;
  init_elements ();
}

void
System::init_elements ()
{
  SCM scm_arr = Grob_array::make_array ();
  all_elements_ = unsmob_grob_array (scm_arr);
  all_elements_->set_ordered (false);
  set_object ("all-elements", scm_arr);
}

Grob *
System::clone (int index) const
{
  return new System (*this, index);
}

int
System::element_count () const
{
  return all_elements_->size ();
}

int
System::spanner_count () const
{
  int k = 0;
  for (vsize i = all_elements_->size (); i--;)
    if (dynamic_cast<Spanner *> (all_elements_->grob (i)))
      k++;
  return k;
}

void
System::typeset_grob (Grob *elem)
{
  if (elem->layout_)
    programming_error ("adding element twice");
  else
    {
      elem->layout_ = pscore_->layout ();
      all_elements_->add (elem);
      elem->unprotect ();
    }
}

void
System::derived_mark () const
{
  if (!all_elements_->empty ())
    {
      Grob **ptr = &all_elements_->array_reference ()[0];
      Grob **end = ptr + all_elements_->size ();
      while (ptr < end)
	{
	  scm_gc_mark ((*ptr)->self_scm ());
	  ptr++;
	}
    }

  if (pscore_)
    scm_gc_mark (pscore_->self_scm ());

  Spanner::derived_mark ();
}

static void
fixup_refpoints (vector<Grob*> const &grobs)
{
  for (vsize i = grobs.size (); i--;)
    grobs[i]->fixup_refpoint ();
}

SCM
System::get_paper_systems ()
{
  for (vsize i = 0; i < all_elements_->size (); i++)
    {
      Grob *g = all_elements_->grob (i);
      if (g->internal_has_interface (ly_symbol2scm ("only-prebreak-interface")))
	{
	  /*
	    Kill no longer needed grobs.
	  */
	  Item *it = dynamic_cast<Item *> (g);
	  if (it && Item::is_non_musical (it))
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
  for (vsize i = 0; i < broken_intos_.size (); i++)
    {
      Grob *se = broken_intos_[i];

      extract_grob_set (se, "all-elements", all_elts);
      for (vsize j = 0; j < all_elts.size (); j++)
	{
	  Grob *g = all_elts[j];
	  g->fixup_refpoint ();
	}

      count += all_elts.size ();
    }

  /*
    needed for doing items.
  */
  fixup_refpoints (all_elements_->array ());

  for (vsize i = 0; i < all_elements_->size (); i++)
    all_elements_->grob (i)->handle_broken_dependencies ();

  handle_broken_dependencies ();

  /* Because the this->get_property (all-elements) contains items in 3
     versions, handle_broken_dependencies () will leave duplicated
     items in all-elements.  Strictly speaking this is harmless, but
     it leads to duplicated symbols in the output.  uniq makes sure
     that no duplicates are in the list.  */
  for (vsize i = 0; i < broken_intos_.size (); i++)
    {
      System *child = dynamic_cast<System*> (broken_intos_[i]);
      child->all_elements_->remove_duplicates ();
    }

  if (be_verbose_global)
    message (_f ("Element count %d.", count + element_count ()));

  SCM lines = scm_c_make_vector (broken_intos_.size (), SCM_EOL);
  for (vsize i = 0; i < broken_intos_.size (); i++)
    {
      if (be_verbose_global)
	progress_indication ("[");

      System *system = dynamic_cast<System *> (broken_intos_[i]);

      system->post_processing ();
      scm_vector_set_x (lines, scm_from_int (i),
			system->get_paper_system ());

      if (be_verbose_global)
	progress_indication (to_string (i) + "]");
    }
  return lines;
}

void
System::break_into_pieces (vector<Column_x_positions> const &breaking)
{
  for (vsize i = 0; i < breaking.size (); i++)
    {
      System *system = dynamic_cast<System *> (clone (broken_intos_.size ()));
      system->rank_ = broken_intos_.size ();

      vector<Grob*> c (breaking[i].cols_);
      pscore_->typeset_system (system);

      int st = Paper_column::get_rank (c[0]);
      int end = Paper_column::get_rank (c.back ());
      Interval iv (pure_height (this, st, end));
      system->set_property ("pure-Y-extent", ly_interval2scm (iv));

      system->set_bound (LEFT, c[0]);
      system->set_bound (RIGHT, c.back ());
      for (vsize j = 0; j < c.size (); j++)
	{
	  c[j]->translate_axis (breaking[i].config_[j], X_AXIS);
	  dynamic_cast<Paper_column *> (c[j])->system_ = system;
	}
      
      set_loose_columns (system, &breaking[i]);
      broken_intos_.push_back (system);
    }
}

void
System::add_column (Paper_column *p)
{
  Grob *me = this;
  Grob_array *ga = unsmob_grob_array (me->get_object ("columns"));
  if (!ga)
    {
      SCM scm_ga = Grob_array::make_array ();
      me->set_object ("columns", scm_ga);
      ga = unsmob_grob_array (scm_ga);
    }

  p->rank_
    = ga->size ()
    ? Paper_column::get_rank (ga->array ().back ()) + 1
    : 0;

  ga->add (p);
  Axis_group_interface::add_element (this, p);
}

void
apply_tweaks (Grob *g, bool broken)
{
  if (bool (g->original ()) == broken)
    {
      SCM tweaks = global_registry_->get_tweaks (g);
      for (SCM s = tweaks; scm_is_pair (s); s = scm_cdr (s))
	{
	  SCM proc = scm_caar (s);
	  SCM rest = scm_cdar (s);
	  scm_apply_1 (proc, g->self_scm (), rest);
	}
    }
}

void
System::pre_processing ()
{
  for (vsize i = 0; i < all_elements_->size (); i++)
    all_elements_->grob (i)->discretionary_processing ();

  if (be_verbose_global)
    message (_f ("Grob count %d", element_count ()));

  /*
    order is significant: broken grobs are added to the end of the
    array, and should be processed before the original is potentially
    killed.
  */
  for (vsize i = all_elements_->size (); i--;)
    all_elements_->grob (i)->handle_prebroken_dependencies ();

  fixup_refpoints (all_elements_->array ());

  for (vsize i = 0; i < all_elements_->size (); i++)
    apply_tweaks (all_elements_->grob (i), false);

  for (vsize i = 0; i < all_elements_->size (); i++)
    {
      Grob *g = all_elements_->grob (i);
      (void) g->get_property ("before-line-breaking");
    }

  for (vsize i = 0; i < all_elements_->size (); i++)
    {
      Grob *e = all_elements_->grob (i);
      (void) e->get_property ("springs-and-rods");
    }
}

void
System::post_processing ()
{
  for (vsize i = 0; i < all_elements_->size (); i++)
    {
      Grob *g = all_elements_->grob (i);

      apply_tweaks (g, true);
      (void) g->get_property ("after-line-breaking");
    }

  Interval iv (extent (this, Y_AXIS));
  if (iv.is_empty ())
    programming_error ("system with empty extent");
  else
    translate_axis (-iv[MAX], Y_AXIS);

  /* Generate all stencils to trigger font loads.
     This might seem inefficient, but Stencils are cached per grob
     anyway. */

  vector<Grob*> all_elts_sorted (all_elements_->array ());
  vector_sort (all_elts_sorted, std::less<Grob*> ());
  uniq (all_elts_sorted);
  this->get_stencil ();
  for (vsize i = all_elts_sorted.size (); i--;)
    {
      Grob *g = all_elts_sorted[i];
      g->get_stencil ();
    }
}

struct Layer_entry
{
  Grob *grob_;
  int layer_;
};

bool
operator< (Layer_entry  const &a,
	   Layer_entry  const &b)
{
  return a.layer_ < b.layer_;
}


SCM
System::get_paper_system ()
{
  SCM exprs = SCM_EOL;
  SCM *tail = &exprs;

  vector<Layer_entry> entries;
  for (vsize j = 0; j < all_elements_->size (); j++)
    {
      Layer_entry e;
      e.grob_ = all_elements_->grob (j);
      e.layer_ = robust_scm2int (e.grob_->get_property ("layer"), 1);
      
      entries.push_back (e); 
    }

  vector_sort (entries, std::less<Layer_entry> ());
  for (vsize j = 0; j < entries.size (); j++)
    {
      Grob *g = entries[j].grob_;
      Stencil st = g->get_print_stencil ();

      if (st.expr() == SCM_EOL)
	continue;
      
      Offset o;
      for (int a = X_AXIS; a < NO_AXES; a++)
	o[Axis (a)] = g->relative_coordinate (this, Axis (a));

      Offset extra = robust_scm2offset (g->get_property ("extra-offset"),
					Offset (0, 0))
	* Staff_symbol_referencer::staff_space (g);

      /* Must copy the stencil, for we cannot change the stencil
	 cached in G.  */

      st.translate (o + extra);

      *tail = scm_cons (st.expr (), SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }

  if (Stencil *me = get_stencil ())
    exprs = scm_cons (me->expr (), exprs);

  Interval x (extent (this, X_AXIS));
  Interval y (extent (this, Y_AXIS));
  Stencil sys_stencil (Box (x, y),
		       scm_cons (ly_symbol2scm ("combine-stencil"),
				 exprs));

  Grob *left_bound = this->get_bound (LEFT);
  SCM prop_init = left_bound->get_property ("line-break-system-details");
  Prob *pl = make_paper_system (prop_init);
  paper_system_set_stencil (pl, sys_stencil);

  /* information that the page breaker might need */
  Grob *right_bound = this->get_bound (RIGHT);
  pl->set_property ("page-break-permission", right_bound->get_property ("page-break-permission"));
  pl->set_property ("page-turn-permission", right_bound->get_property ("page-turn-permission"));
  pl->set_property ("page-break-penalty", right_bound->get_property ("page-break-penalty"));
  pl->set_property ("page-turn-penalty", right_bound->get_property ("page-turn-penalty"));

  if (!scm_is_pair (pl->get_property ("refpoint-Y-extent")))
    {
      Interval staff_refpoints;
      staff_refpoints.set_empty ();
      extract_grob_set (this, "spaceable-staves", staves);
      for (vsize i = 0; i < staves.size (); i++)
	{
	  Grob *g = staves[i];
	  staff_refpoints.add_point (g->relative_coordinate (this, Y_AXIS));
	}
      pl->set_property ("refpoint-Y-extent", ly_interval2scm (staff_refpoints));
    }

  pl->set_property ("system-grob", this->self_scm ()); 

  return pl->unprotect ();
}

vector<Item*>
System::broken_col_range (Item const *left, Item const *right) const
{
  vector<Item*> ret;

  left = left->get_column ();
  right = right->get_column ();

  extract_grob_set (this, "columns", cols);
  vsize i = 0;
  while (i < cols.size ()
	 && cols[i] != left)
    i++;

  if (i < cols.size ())
    i++;

  while (i < cols.size ()
	 && cols[i] != right)
    {
      Paper_column *c = dynamic_cast<Paper_column *> (cols[i]);
      if (Paper_column::is_breakable (c) && !c->system_)
	ret.push_back (c);
      i++;
    }

  return ret;
}


/** Return all columns, but filter out any unused columns , since they might
    disrupt the spacing problem. */
vector<Grob*>
System::columns () const
{
  extract_grob_set (this, "columns", ro_columns);

  int last_breakable = ro_columns.size ();

  while (last_breakable--)
    {
      if (Paper_column::is_breakable (ro_columns [last_breakable]))
	break;
    }

  vector<Grob*> columns;
  for (int i = 0; i <= last_breakable; i++)
    {
      if (Paper_column::is_used (ro_columns[i]))
	columns.push_back (ro_columns[i]);
    }

  return columns;
}

Grob*
System::column (int which) const
{
  extract_grob_set (this, "columns", columns);
  for (vsize i = 0; i < columns.size (); i++)
    if (dynamic_cast<Paper_column*> (columns[i])->get_rank () == which)
      return columns[i];
  programming_error ("couldn't find the column of the requested index");
  return columns[0];
}

Paper_score*
System::paper_score () const
{
  return pscore_;
}

int
System::get_rank () const
{
  return rank_;
}

System *
get_root_system (Grob *me) 
{
  Grob *system_grob = me;
  
  while (system_grob->get_parent (Y_AXIS))
    system_grob = system_grob->get_parent (Y_AXIS);

  return dynamic_cast<System*> (system_grob); 
}



ADD_INTERFACE (System, "system-interface",
	       "This is the toplevel object: each object in a score "
	       "ultimately has a System object as its X and Y parent. ",

	       /* properties */
	       "all-elements "
	       "columns "
	       "pure-Y-extent "
	       "spaceable-staves "
	       )
