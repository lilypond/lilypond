/*
  score-elem.cc -- implement Score_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include <string.h>

#include "group-interface.hh"
#include "misc.hh"
#include "paper-score.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "score-element.hh"
#include "debug.hh"
#include "spanner.hh"
#include "line-of-score.hh"
#include "item.hh"
#include "paper-column.hh"
#include "molecule.hh"
#include "misc.hh"
#include "paper-outputter.hh"
#include "dimension-cache.hh"
#include "side-position-interface.hh"
#include "item.hh"

Score_element::Score_element()
{
  output_p_ =0;
  dim_cache_[X_AXIS] = new Dimension_cache;
  dim_cache_[Y_AXIS] = new Dimension_cache;
  dim_cache_[X_AXIS]->elt_l_ = dim_cache_[Y_AXIS]->elt_l_ = this;
  
  used_b_ = false;

  dim_cache_[X_AXIS]->set_callback (molecule_extent);
  dim_cache_[Y_AXIS]->set_callback (molecule_extent); 
  used_b_ = false;
  pscore_l_=0;
  lookup_l_ =0;
  status_i_ = 0;
  self_scm_ = SCM_EOL;
  original_l_ = 0;
  element_property_alist_ = SCM_EOL;

  smobify_self ();


  set_elt_property ("dependencies", SCM_EOL);
}

SCM ly_deep_copy (SCM);

SCM
ly_deep_copy (SCM l)
{
  if (gh_pair_p (l))
    {
      return gh_cons (ly_deep_copy (gh_car (l)), ly_deep_copy (gh_cdr (l)));
    }
  else
    return l;
}


Score_element::Score_element (Score_element const&s)
{
  dim_cache_[X_AXIS] = new Dimension_cache (*s.dim_cache_[X_AXIS]);
  dim_cache_[Y_AXIS] = new Dimension_cache (*s.dim_cache_[Y_AXIS]);
  dim_cache_[X_AXIS]->elt_l_ = dim_cache_[Y_AXIS]->elt_l_ = this;
  
  self_scm_ = SCM_EOL;
  used_b_ = true;
  original_l_ =(Score_element*) &s;

  /*
    should protect because smobify_self () might trigger GC.
   */
  element_property_alist_ = scm_protect_object (ly_deep_copy (s.element_property_alist_));

  output_p_ =0;
  status_i_ = s.status_i_;
  lookup_l_ = s.lookup_l_;
  pscore_l_ = s.pscore_l_;

  smobify_self ();
}

Score_element::~Score_element()
{
  assert (!output_p_);
  assert (status_i_ >=0);
  status_i_  = -1;

  delete dim_cache_[X_AXIS];
  delete dim_cache_[Y_AXIS];  
}


Real
Score_element::get_real (String s) const
{
  return gh_scm2double (get_elt_property (s));
}

void
Score_element::set_real (String s, Real r)
{
  set_elt_property (s, gh_double2scm (r));
}

// should also have one that takes SCM arg. 
SCM
Score_element::get_elt_property (String nm) const
{
  SCM sym =  ly_symbol2scm (nm.ch_C());
  SCM s = scm_assq(sym, element_property_alist_);

  if (s != SCM_BOOL_F)
    return gh_cdr (s); 
  
  if (pscore_l_)
    {
      SCM sym2 = ly_symbol2scm ((name () + ("::" + nm)).ch_C());
      SCM val;
      
      // should probably check for Type::sym as well.
      Paper_def * p= pscore_l_->paper_l_;
      if (p->default_properties_.try_retrieve (sym2, &val))
	return val;
      else if (p->default_properties_.try_retrieve (sym, &val))
	return val;
    }
  
  return SCM_UNDEFINED;
}

SCM
Score_element::remove_elt_property (String key)
{
  SCM s = get_elt_property (key); 
  SCM sym = ly_symbol2scm (key.ch_C());
  element_property_alist_ =  scm_assq_remove_x (element_property_alist_, sym);
  return s;
}

/*
  UGH. assoc vs. assq
 */
void
Score_element::set_elt_property (String k, SCM v)
{
  SCM s = ly_symbol2scm (k.ch_C( ));
  element_property_alist_ = scm_assoc_set_x (element_property_alist_, s, v);
}

Interval
Score_element::molecule_extent(Dimension_cache const *c)
{
  Score_element *s = dynamic_cast<Score_element*>(c->element_l());
  Molecule*m = s->do_brew_molecule_p();
  return  m->extent()[c->axis ()];
}


void
Score_element::print() const
{
#ifndef NPRINT
  DEBUG_OUT << classname(this) << "{\n";
  
  
  if (flower_dstream && !flower_dstream->silent_b ("Score_element"))
    ly_display_scm (element_property_alist_);

  if (original_l_)
    DEBUG_OUT << "Copy ";
  do_print();
  
  DEBUG_OUT <<  "}\n";
#endif
}

Paper_def*
Score_element::paper_l ()  const
{
 return pscore_l_ ? pscore_l_->paper_l_ : 0;
}

Lookup const *
Score_element::lookup_l () const
{
  if (!lookup_l_)
    {
      Score_element * urg = (Score_element*)this;
      SCM sz = urg->remove_elt_property ("fontsize");
      int i = (sz != SCM_UNDEFINED)
	? gh_scm2int  (sz)
	: 0;

      urg->lookup_l_ =  (Lookup*)pscore_l_->paper_l_->lookup_l (i);
    }
  return lookup_l_;
}

void
Score_element::add_processing()
{
  assert (status_i_ >=0);
  if (status_i_)
    return;
  status_i_ ++;

#if 0
    /*
    UGH. UGH. UGH.
   */
  if (get_elt_property ("self-alignment-X") != SCM_UNDEFINED
      && !dim_cache_[X_AXIS]->off_callback_l_)
    {
      dim_cache_[X_AXIS]->off_callbacks_.push (Side_position_interface::self_alignment);
    }
  
  if (get_elt_property ("self-alignment-Y") != SCM_UNDEFINED
      && !dim_cache_[X_AXIS]->off_callback_l_)
      
    {
      dim_cache_[Y_AXIS]->set_offset_callback (Side_position_interface::self_alignment);
    }
#endif
  
  do_add_processing();
}

void
Score_element::calculate_dependencies (int final, int busy,
				       Score_element_method_pointer funcptr)
{
  assert (status_i_ >=0);

  if (status_i_ >= final)
    return;

  assert (status_i_!= busy);
  status_i_= busy;

  Link_array<Score_element> dependency_arr =
    Group_interface__extract_elements (this, (Score_element*)0, "dependencies");
  
  for (int i=0; i < dependency_arr.size(); i++)
    dependency_arr[i]->calculate_dependencies (final, busy, funcptr);

  Link_array<Score_element> extra (get_extra_dependencies());
  for (int i=0; i < extra.size(); i++)
    extra[i]->calculate_dependencies (final, busy, funcptr);
  
  (this->*funcptr)();
  status_i_= final;
}

void
Score_element::output_processing () 
{
  if (get_elt_property ("transparent") != SCM_UNDEFINED)
    return;

  // we're being silly here. 
  if (output_p_)
    delete output_p_;
  
  output_p_ = do_brew_molecule_p ();
  Offset o (relative_coordinate (0, X_AXIS), relative_coordinate (0, Y_AXIS));

  SCM s = get_elt_property ("extra-offset");
  if (gh_pair_p (s))
    {
      Real il = paper_l ()->get_var ("interline");
      o[X_AXIS] += il * gh_scm2double (gh_car (s));
      o[Y_AXIS] += il * gh_scm2double (gh_cdr (s));      
    }
  
  pscore_l_->outputter_l_->output_molecule (output_p_,
					    o,
					    classname(this));

  delete output_p_;
  output_p_ =0;
}

/*
  
  VIRTUAL STUBS

 */
void
Score_element::do_break_processing()
{
}

void
Score_element::do_post_processing()
{
}

void
Score_element::do_breakable_col_processing()
{
  handle_prebroken_dependencies();
}

void
Score_element::do_pre_processing()
{
}

void
Score_element::do_space_processing ()
{
}

void
Score_element::do_add_processing()
{
}



Molecule*
Score_element::do_brew_molecule_p() const
{
  Interval emp;
  emp.set_empty ();
  Molecule a (lookup_l ()->fill (Box (emp,emp)));
  return new Molecule (a);
}


Line_of_score *
Score_element::line_l() const
{
  return 0;
}

void
Score_element::add_dependency (Score_element*e)
{
  if (e)
    {
      Group_interface gi (this, "dependencies");
      gi.add_element (e);
    }
  else
    programming_error ("Null dependency added");
}




/**
      Do break substitution in S, using CRITERION. Return new value.
         CRITERION is either a SMOB pointer to the desired line, or a number
	 representing the break direction.  */
SCM
Score_element::handle_broken_smobs (SCM s, SCM criterion)
{
  Score_element *sc = unsmob_element ( s);
  if (sc)
    {
      if (criterion == SCM_UNDEFINED)
	return SCM_UNDEFINED;
      else if (gh_number_p (criterion))
	{
	  Item * i = dynamic_cast<Item*> (sc);
	  Direction d = to_dir (criterion);
	  if (i && i->break_status_dir () != d)
	    {
	      Item *br = i->find_broken_piece (d);
	      return  (br) ? br->self_scm_ : SCM_UNDEFINED;
	    }
	}
      else
	{
	  Score_element * ln = unsmob_element ( criterion);
	  Line_of_score * line = dynamic_cast<Line_of_score*> (ln);
	  Score_element * br =0;
	  Line_of_score * dep_line = sc->line_l ();
	  if (dep_line != line)
	    {
	      br = sc->find_broken_piece (line);
	      return  (br) ?  br->self_scm_ : SCM_UNDEFINED;
	    }
	  if (!dep_line)
	    return SCM_UNDEFINED;
	}
    }
  else if (gh_pair_p (s))
    {
      /*
	UGH! breaks on circular lists.
      */
      gh_set_car_x (s, handle_broken_smobs (gh_car (s), criterion));
      gh_set_cdr_x (s, handle_broken_smobs (gh_cdr (s), criterion));

      SCM c = gh_cdr(s);

      // gh_list_p () is linear, this is O(1)  
      bool list = gh_pair_p (c) || c == SCM_EOL;
      
      if (gh_car (s) == SCM_UNDEFINED && list)
	return c;
    }
  return s;
}

void
Score_element::handle_broken_dependencies()
{
  Line_of_score *line  = line_l();
  element_property_alist_ = handle_broken_smobs (element_property_alist_,
						 line ? line->self_scm_ : SCM_UNDEFINED);

  if (!line)
    return;
}


/*
  TODO: cleanify.
 */
void
Score_element::handle_prebroken_dependencies()
{
  if (Item*i =dynamic_cast<Item*> (this))
    {
      element_property_alist_
	= handle_broken_smobs (element_property_alist_,
			       gh_int2scm (i->break_status_dir ()));
    }
}





Link_array<Score_element>
Score_element::get_extra_dependencies() const
{
  Link_array<Score_element> empty;
  return empty;
}

bool
Score_element::linked_b() const
{
  return used_b_;
}

void
Score_element::do_print () const
{
}

Score_element*
Score_element::find_broken_piece (Line_of_score*) const
{
  return 0;
}

SCM
Score_element::mark_smob (SCM ses)
{
  void * mp = (void*) gh_cdr(ses);
  Score_element * s = (Score_element*) mp;

  if (s->self_scm_ != ses)
    {
      programming_error ("SMOB marking gone awry");
      return SCM_EOL;
    }
  return s->element_property_alist_;
}


int
Score_element::print_smob (SCM s, SCM port, scm_print_state *)
{
  Score_element *sc = (Score_element *) gh_cdr (s);
     
  scm_puts ("#<Score_element ", port);
  scm_puts ((char *)sc->name (), port);

  // scm_puts (" properties = ", port);
  // scm_display (sc->element_property_alist_, port);
  scm_puts (" >", port);
  return 1;
}

void
Score_element::do_smobify_self ()
{
  scm_unprotect_object (element_property_alist_); // ugh
}
#include "ly-smobs.icc"
IMPLEMENT_SMOBS(Score_element);

SCM
Score_element::equal_p (SCM a, SCM b)
{
  return gh_cdr(a) == gh_cdr(b) ? SCM_BOOL_T : SCM_BOOL_F;
}

void
Score_element::translate_axis (Real y, Axis a)
{
  dim_cache_[a]->translate (y);
}  

Real
Score_element::relative_coordinate (Score_element const*e, Axis a) const
{
  return dim_cache_[a]->relative_coordinate (e ? e->dim_cache_[a] : 0);
}

Score_element * 
Score_element::common_refpoint (Score_element const* s, Axis a) const
{
  Dimension_cache *dim = dim_cache_[a]->common_refpoint (s->dim_cache_[a]);
  if (!dim)
    programming_error ("No  common reference point");
  return  dim ? dim->element_l () : 0;
}

void
Score_element::set_empty (Axis a)
{
  dim_cache_[a]->callback_l_ =0;
}

bool
Score_element::empty_b (Axis a)const
{
  return !dim_cache_[a]->callback_l_;
}

Interval
Score_element::extent (Axis a) const
{
  Dimension_cache const * d = dim_cache_[a];

  return d->get_dim ();
}

Score_element*
unsmob_element (SCM s)
{
  if (SMOB_IS_TYPE_B (Score_element, s))
    return SMOB_TO_TYPE(Score_element,s);
  else
    return 0;
}


Score_element*
Score_element::parent_l (Axis a) const
{
  Dimension_cache*d= dim_cache_[a]->parent_l_;
  return d ? d->elt_l_ : 0;
}

Score_element *
Score_element::common_refpoint (Link_array<Score_element> gs, Axis a) const
{
  Dimension_cache * common = dim_cache_[a];
  for (int i=0; i < gs.size (); i++)
    {
      common = common->common_refpoint (gs[i]->dim_cache_[a]);
    }

  return common->element_l ();
}

char const *
Score_element::name () const
{
  return classname (this);
}


void
Score_element::set_parent (Score_element *g, Axis a)
{
  dim_cache_[a]->parent_l_ = g ? g->dim_cache_[a]: 0;
}

void
Score_element::fixup_refpoint ()
{
  for (int a = X_AXIS; a < NO_AXES; a ++)
    {
      Axis ax = (Axis)a;
      Score_element * par = parent_l (ax);

      if (!par)
	continue;
      
      if (par->line_l () != line_l ())
	{
	  Score_element * newpar = par->find_broken_piece (line_l ());
	  set_parent (newpar, ax);
	}

      if (Item * i  = dynamic_cast<Item*> (this))
	{
	  Item *pari = dynamic_cast<Item*> (par);

	  if (pari && i)
	    {
	      Direction  my_dir = i->break_status_dir () ;
	      if (my_dir!= pari->break_status_dir())
		{
		  Item *newpar =  pari->find_broken_piece (my_dir);
		  set_parent (newpar, ax);
		}
	    }
	}
    }
}
