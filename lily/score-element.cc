/*
  score-elem.cc -- implement Score_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include <string.h>
#include <math.h>

#include "input-smob.hh"
#include "libc-extension.hh"
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

#include "ly-smobs.icc"

/*
TODO:

remove dynamic_cast<Spanner,Item> and put this code into respective
  subclass.
*/


#define INFINITY_MSG "Infinity or NaN encountered"

Score_element::Score_element(SCM basicprops)
{
  /*
    fixme: default should be no callback.
   */

  pscore_l_=0;
  lookup_l_ =0;
  status_i_ = 0;
  original_l_ = 0;
  immutable_property_alist_ =  basicprops;
  mutable_property_alist_ = SCM_EOL;

  smobify_self ();

  char const*onames[] = {"X-offset-callbacks", "Y-offset-callbacks"};
  char const*enames[] = {"X-extent-callback", "Y-extent-callback"};
  
  for (int a = X_AXIS; a <= Y_AXIS; a++){
    SCM l = get_elt_property (onames[a]);

    if (scm_ilength (l) >=0)
      {
	dim_cache_[a].offset_callbacks_ = l;
	dim_cache_[a].offsets_left_ = scm_ilength (l);
      }
    else
      {
	programming_error ("[XY]-offset-callbacks must be a list");
      }

    SCM cb = get_elt_property (enames[a]);

    /*
      Should change default to be empty? 
     */
    if (!gh_procedure_p (cb) && !gh_pair_p (cb))
      cb = molecule_extent_proc;
    
    dim_cache_[a].dimension_ = cb;
  }

  SCM meta = get_elt_property ("meta");
  SCM ifs = scm_assoc (ly_symbol2scm ("interfaces"), meta);
  
  set_elt_property ("interfaces",gh_cdr (ifs));
}


Score_element::Score_element (Score_element const&s)
   : dim_cache_ (s.dim_cache_)
{
  original_l_ =(Score_element*) &s;
  immutable_property_alist_ = s.immutable_property_alist_;
  mutable_property_alist_ = SCM_EOL;
  
  status_i_ = s.status_i_;
  lookup_l_ = s.lookup_l_;
  pscore_l_ = s.pscore_l_;

  smobify_self ();
}

Score_element::~Score_element()
{
  /*
    do nothing scm-ish and no unprotecting here.
   */
}


SCM
Score_element::get_elt_property (const char *nm) const
{
  SCM sym = ly_symbol2scm (nm);
  return get_elt_property (sym);
}

SCM
Score_element::get_elt_property (SCM sym) const
{
  SCM s = scm_sloppy_assq(sym, mutable_property_alist_);
  if (s != SCM_BOOL_F)
    return gh_cdr (s);

  s = scm_sloppy_assq (sym, immutable_property_alist_);
  return (s == SCM_BOOL_F) ? SCM_EOL : gh_cdr (s); 
}

/*
  Remove the value associated with KEY, and return it. The result is
  that a next call will yield SCM_UNDEFINED (and not the underlying
  `basic' property.
*/
SCM
Score_element::remove_elt_property (const char* key)
{
  SCM val = get_elt_property (key);
  if (val != SCM_EOL)
    set_elt_property (key, SCM_EOL);
  return val;
}

void
Score_element::set_elt_property (const char* k, SCM v)
{
  SCM s = ly_symbol2scm (k);
  set_elt_property (s, v);
}

/*
  Puts the k, v in the immutable_property_alist_, which is convenient for
  storing variables that are needed during the breaking process. (eg.
  Line_of_score::rank : int )
 */
void
Score_element::set_immutable_elt_property (const char*k, SCM v)
{
  SCM s = ly_symbol2scm (k);
  set_immutable_elt_property (s, v);
}

void
Score_element::set_immutable_elt_property (SCM s, SCM v)
{
  immutable_property_alist_ = gh_cons (gh_cons (s,v), mutable_property_alist_);
  mutable_property_alist_ = scm_assq_remove_x (mutable_property_alist_, s);
}
void
Score_element::set_elt_property (SCM s, SCM v)
{
  mutable_property_alist_ = scm_assq_set_x (mutable_property_alist_, s, v);
}


MAKE_SCHEME_CALLBACK(Score_element,molecule_extent,2);
SCM
Score_element::molecule_extent (SCM element_smob, SCM scm_axis)
{
  Score_element *s = unsmob_element (element_smob);
  Axis a = (Axis) gh_scm2int (scm_axis);

  Molecule m = s->get_molecule ();
  return ly_interval2scm ( m.extent(a));
}

MAKE_SCHEME_CALLBACK(Score_element,preset_extent,2);

SCM
Score_element::preset_extent (SCM element_smob, SCM scm_axis)
{
  Score_element *s = unsmob_element (element_smob);
  Axis a = (Axis) gh_scm2int (scm_axis);

  SCM ext = s->get_elt_property ((a == X_AXIS)
				 ? "extent-X"
				 : "extent-Y");
  
  if (gh_pair_p (ext))
    {
      Real l = gh_scm2double (gh_car (ext));
      Real r = gh_scm2double (gh_cdr (ext));
      l *= s->paper_l ()->get_var ("staffspace");
      r *= s->paper_l ()->get_var ("staffspace");
      return ly_interval2scm (Interval (l, r));
    }
  
  return ly_interval2scm ( Interval ());
}



Paper_def*
Score_element::paper_l ()  const
{
 return pscore_l_ ? pscore_l_->paper_l_ : 0;
}

Lookup const *
Score_element::lookup_l () const
{
  /*
    URG junkthis, caching is clumsy.
   */
  if (!lookup_l_)
    {
      Score_element * urg = (Score_element*)this;
      SCM sz = urg->remove_elt_property ("font-size");
      int i = (gh_number_p (sz))
	? gh_scm2int  (sz)
	: 0;

      urg->lookup_l_ =  (Lookup*)pscore_l_->paper_l_->lookup_l (i);
    }
  return lookup_l_;
}

void
Score_element::calculate_dependencies (int final, int busy, SCM funcname)
{
  assert (status_i_ >=0);

  if (status_i_ >= final)
    return;

  if (status_i_== busy)
    {
      programming_error ("Element is busy, come back later");
      return;
    }
  
  status_i_= busy;

  for (SCM d=  get_elt_property ("dependencies"); gh_pair_p (d); d = gh_cdr (d))
    {
      unsmob_element (gh_car (d))
	->calculate_dependencies (final, busy, funcname);
    }

  // ughugh.
  String s = ly_symbol2string (funcname);
  SCM proc = get_elt_property (s.ch_C());
  if (gh_procedure_p (proc))
    gh_call1 (proc, this->self_scm ());
  
  status_i_= final;

}

Molecule
Score_element::get_molecule ()  const
{
  SCM proc = get_elt_property ("molecule-callback");

  SCM mol = SCM_EOL;
  if (gh_procedure_p (proc)) 
    mol = gh_apply (proc, gh_list (this->self_scm (), SCM_UNDEFINED));

    
  SCM origin =get_elt_property ("origin");
  if (!unsmob_input (origin))
    origin =ly_symbol2scm ("no-origin");
  
  if (gh_pair_p (mol))
    {
      // ugr.
	mol = gh_cons (gh_list (origin, gh_car (mol), SCM_UNDEFINED), gh_cdr (mol));
    }


  Molecule m (create_molecule (mol));

  /*
    This is almost the same as setting molecule-callback to #f, but
    this retains the dimensions of this element, which means that you
    can erase elements individually.  */
  if (to_boolean (get_elt_property ("transparent")))
    m = Molecule (m.extent_box (), SCM_EOL);

  return m;
}


/*
  
  VIRTUAL STUBS

 */
void
Score_element::do_break_processing()
{
}





MAKE_SCHEME_CALLBACK(Score_element,brew_molecule,1)

/*
  ugh.
 */  
SCM
Score_element::brew_molecule (SCM smob) 
{
  Score_element * sc = unsmob_element (smob);
  SCM glyph = sc->get_elt_property ("glyph");
  if (gh_string_p (glyph))
    {
      return sc->lookup_l ()->afm_find (String (ly_scm2string (glyph))).create_scheme ();
    }
  else
    {
      return SCM_EOL;
    }
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
      Pointer_group_interface ::add_element (this, "dependencies",e);

    }
  else
    programming_error ("Null dependency added");
}




/**
      Do break substitution in S, using CRITERION. Return new value.
      CRITERION is either a SMOB pointer to the desired line, or a number
      representing the break direction. Do not modify SRC.
*/
SCM
Score_element::handle_broken_smobs (SCM src, SCM criterion)
{
 again:
  Score_element *sc = unsmob_element (src);
  if (sc)
    {
      if (gh_number_p (criterion))
	{
	  Item * i = dynamic_cast<Item*> (sc);
	  Direction d = to_dir (criterion);
	  if (i && i->break_status_dir () != d)
	    {
	      Item *br = i->find_prebroken_piece (d);
	      return  (br) ? br->self_scm () : SCM_UNDEFINED;
	    }
	}
      else
	{
	  Line_of_score * line
	    = dynamic_cast<Line_of_score*> (unsmob_element (criterion));
	  if (sc->line_l () != line)
	    {
	      sc = sc->find_broken_piece (line);

	    }

	  /* now: !sc || (sc && sc->line_l () == line) */
	  if (!sc)
	    return SCM_UNDEFINED;

	  /* now: sc && sc->line_l () == line */
	  if (!line
	      || (sc->common_refpoint (line, X_AXIS)
		  && sc->common_refpoint (line, Y_AXIS)))
	    {
	      return sc->self_scm ();
	    }
	  return SCM_UNDEFINED;
	}
    }
  else if (gh_pair_p (src))
    {
      SCM oldcar =gh_car (src);
      /*
	UGH! breaks on circular lists.
      */
      SCM newcar = handle_broken_smobs (oldcar, criterion);
      SCM oldcdr = gh_cdr (src);
      
      if (newcar == SCM_UNDEFINED
	  && (gh_pair_p (oldcdr) || oldcdr == SCM_EOL))
	{
	  /*
	    This is tail-recursion, ie. 
	    
	    return handle_broken_smobs (cdr, criterion);

	    We don't want to rely on the compiler to do this.  Without
	    tail-recursion, this easily crashes with a stack overflow.  */
	  src =  oldcdr;
	  goto again;
	}

      SCM newcdr = handle_broken_smobs (oldcdr, criterion);
      return gh_cons (newcar, newcdr);
    }
  else
    return src;

  return src;
}

void
Score_element::handle_broken_dependencies()
{
  Spanner * s= dynamic_cast<Spanner*> (this);
  if (original_l_ && s)
    return;

  if (s)
    {
      for (int i = 0;  i< s->broken_into_l_arr_ .size (); i++)
	{
	  Score_element * sc = s->broken_into_l_arr_[i];
	  Line_of_score * l = sc->line_l ();
	  sc->mutable_property_alist_ =
	    handle_broken_smobs (mutable_property_alist_,
				 l ? l->self_scm () : SCM_UNDEFINED);
	}
    }


  Line_of_score *line = line_l();

  if (line && common_refpoint (line, X_AXIS) && common_refpoint (line, Y_AXIS))
    {
      mutable_property_alist_
	= handle_broken_smobs (mutable_property_alist_,
			       line ? line->self_scm () : SCM_UNDEFINED);
    }
  else if (dynamic_cast <Line_of_score*> (this))
    {
      mutable_property_alist_ = handle_broken_smobs (mutable_property_alist_,
					    SCM_UNDEFINED);
    }
  else
    {
      /*
	This element is `invalid'; it has been removed from all
	dependencies, so let's junk the element itself.

	do not do this for Line_of_score, since that would remove
	references to the originals of score-elts, which get then GC'd
	(a bad thing.)
      */
      suicide();
    }
}

/*
 Note that we still want references to this element to be
 rearranged, and not silently thrown away, so we keep pointers
 like {broken_into_{drul,array}, original}
*/
void
Score_element::suicide ()
{
  mutable_property_alist_ = SCM_EOL;
  immutable_property_alist_ = SCM_EOL;

  set_extent_callback (SCM_EOL, Y_AXIS);
  set_extent_callback (SCM_EOL, X_AXIS);

  for (int a= X_AXIS; a <= Y_AXIS; a++)
    {
      dim_cache_[a].offset_callbacks_ = SCM_EOL;
      dim_cache_[a].offsets_left_ = 0;
    }
}

void
Score_element::handle_prebroken_dependencies()
{
}

Score_element*
Score_element::find_broken_piece (Line_of_score*) const
{
  return 0;
}

void
Score_element::translate_axis (Real y, Axis a)
{
  if (isinf (y) || isnan (y))
    programming_error (_(INFINITY_MSG));
  else
    {
      dim_cache_[a].offset_ += y;
    }
}  

Real
Score_element::relative_coordinate (Score_element const*refp, Axis a) const
{
  if (refp == this)
    return 0.0;

  /*
    We catch PARENT_L_ == nil case with this, but we crash if we did
    not ask for the absolute coordinate (ie. REFP == nil.)
    
   */
  if (refp == dim_cache_[a].parent_l_)
    return get_offset (a);
  else
    return get_offset (a) + dim_cache_[a].parent_l_->relative_coordinate (refp, a);
}

Real
Score_element::get_offset (Axis a) const
{
  Score_element *me = (Score_element*) this;
  while (dim_cache_[a].offsets_left_)
    {
      int l = --me->dim_cache_[a].offsets_left_;
      SCM cb = scm_list_ref (dim_cache_[a].offset_callbacks_,  gh_int2scm (l));
      SCM retval = gh_call2 (cb, self_scm (), gh_int2scm (a));

      Real r =  gh_scm2double (retval);
      if (isinf (r) || isnan (r))
	{
	  programming_error (INFINITY_MSG);
	  r = 0.0;
	}
      me->dim_cache_[a].offset_ +=r;
    }
  return dim_cache_[a].offset_;
}


MAKE_SCHEME_CALLBACK(Score_element,point_dimension_callback,2);
SCM
Score_element::point_dimension_callback (SCM , SCM )
{
  return ly_interval2scm ( Interval (0,0));
}

bool
Score_element::empty_b (Axis a)const
{
  return ! (gh_pair_p (dim_cache_[a].dimension_ ) ||
	    gh_procedure_p (dim_cache_[a].dimension_ ));
}

/*
  TODO: add

    Score_element *refpoint

  to arguments?
 */
Interval
Score_element::extent (Score_element * refp, Axis a) const
{
  Real x = relative_coordinate (refp, a);

  
  Dimension_cache * d = (Dimension_cache *)&dim_cache_[a];
  Interval ext ;   
  if (gh_pair_p (d->dimension_))
    ;
  else if (gh_procedure_p (d->dimension_))
    {
      /*
	FIXME: add doco on types, and should typecheck maybe? 
       */
      d->dimension_= gh_call2 (d->dimension_, self_scm(), gh_int2scm (a));
    }
  else
    return ext;

  if (!gh_pair_p (d->dimension_))
    return ext;
  
  ext = ly_scm2interval (d->dimension_);

  SCM extra = get_elt_property (a == X_AXIS
				? "extra-extent-X"
				: "extra-extent-Y");

  /*
    signs ?
   */
  Real s = paper_l ()->get_var ("staffspace");
  if (gh_pair_p (extra))
    {
      ext[BIGGER] +=  s * gh_scm2double (gh_cdr (extra));
      ext[SMALLER] +=  s * gh_scm2double (gh_car (extra));
    }
  
  extra = get_elt_property (a == X_AXIS
				? "minimum-extent-X"
				: "minimum-extent-Y");
  if (gh_pair_p (extra))
    {
      ext.unite (Interval (s * gh_scm2double (gh_car (extra)),
			   s * gh_scm2double (gh_cdr (extra))));
    }

  ext.translate (x);
  
  return ext;
}


Score_element*
Score_element::parent_l (Axis a) const
{
  return  dim_cache_[a].parent_l_;
}

Score_element * 
Score_element::common_refpoint (Score_element const* s, Axis a) const
{
  /*
    I don't like the quadratic aspect of this code, but I see no other
    way. The largest chain of parents might be 10 high or so, so
    it shouldn't be a real issue. */
  for (Score_element const *c = this; c; c = c->dim_cache_[a].parent_l_)
    for (Score_element const * d = s; d; d = d->dim_cache_[a].parent_l_)
      if (d == c)
	return (Score_element*)d;

  return 0;
}


Score_element *
Score_element::common_refpoint (SCM elist, Axis a) const
{
  Score_element * common = (Score_element*) this;
  for (; gh_pair_p (elist); elist = gh_cdr (elist))
    {
      Score_element * s = unsmob_element (gh_car (elist));
      if (s)
	common = common->common_refpoint (s, a);
    }

  return common;
}

String
Score_element::name () const
{
  SCM meta = get_elt_property ("meta");
  SCM nm = scm_assoc (ly_symbol2scm ("name"), meta);
  nm =  (gh_pair_p (nm)) ? gh_cdr (nm) : SCM_EOL;
  return  gh_string_p (nm) ?ly_scm2string (nm) :  classname (this);  
}

void
Score_element::add_offset_callback (SCM cb, Axis a)
{
  if (!has_offset_callback_b (cb, a))
  {
    dim_cache_[a].offset_callbacks_ = gh_cons (cb, dim_cache_[a].offset_callbacks_ );
    dim_cache_[a].offsets_left_ ++;
  }
}

bool
Score_element::has_extent_callback_b (SCM cb, Axis a)const
{
  return scm_equal_p (cb, dim_cache_[a].dimension_);
}


bool
Score_element::has_extent_callback_b (Axis a) const
{
  return gh_procedure_p (dim_cache_[a].dimension_);
}

bool
Score_element::has_offset_callback_b (SCM cb, Axis a)const
{
  return scm_memq (cb, dim_cache_[a].offset_callbacks_) != SCM_BOOL_F;
}

void
Score_element::set_extent_callback (SCM dc, Axis a)
{
  dim_cache_[a].dimension_ =dc;
}

void
Score_element::set_parent (Score_element *g, Axis a)
{
  dim_cache_[a].parent_l_ = g;
}

MAKE_SCHEME_CALLBACK(Score_element,fixup_refpoint,1);
SCM
Score_element::fixup_refpoint (SCM smob)
{
  Score_element *me = unsmob_element (smob);
  for (int a = X_AXIS; a < NO_AXES; a ++)
    {
      Axis ax = (Axis)a;
      Score_element * parent = me->parent_l (ax);

      if (!parent)
	continue;
      
      if (parent->line_l () != me->line_l () && me->line_l ())
	{
	  Score_element * newparent = parent->find_broken_piece (me->line_l ());
	  me->set_parent (newparent, ax);
	}

      if (Item * i  = dynamic_cast<Item*> (me))
	{
	  Item *parenti = dynamic_cast<Item*> (parent);

	  if (parenti && i)
	    {
	      Direction  my_dir = i->break_status_dir () ;
	      if (my_dir!= parenti->break_status_dir())
		{
		  Item *newparent =  parenti->find_prebroken_piece (my_dir);
		  me->set_parent (newparent, ax);
		}
	    }
	}
    }
  return smob;
}



/****************************************************
  SMOB funcs
 ****************************************************/


IMPLEMENT_UNSMOB(Score_element, element);
IMPLEMENT_SMOBS(Score_element);
IMPLEMENT_DEFAULT_EQUAL_P(Score_element);

SCM
Score_element::mark_smob (SCM ses)
{
  Score_element * s = (Score_element*) SCM_CELL_WORD_1(ses);
  scm_gc_mark (s->immutable_property_alist_);
  scm_gc_mark (s->mutable_property_alist_);

  for (int a =0 ; a < 2; a++)
    {
      scm_gc_mark (s->dim_cache_[a].offset_callbacks_);
      scm_gc_mark (s->dim_cache_[a].dimension_);
    }
  
  if (s->parent_l (Y_AXIS))
    scm_gc_mark (s->parent_l (Y_AXIS)->self_scm ());
  if (s->parent_l (X_AXIS))
    scm_gc_mark (s->parent_l (X_AXIS)->self_scm ());

  if (s->original_l_)
    scm_gc_mark (s->original_l_->self_scm ());
  return s->do_derived_mark ();
}

int
Score_element::print_smob (SCM s, SCM port, scm_print_state *)
{
  Score_element *sc = (Score_element *) gh_cdr (s);
     
  scm_puts ("#<Score_element ", port);
  scm_puts ((char *)sc->name ().ch_C(), port);

  /*
    don't try to print properties, that is too much hassle.
   */
  scm_puts (" >", port);
  return 1;
}

SCM
Score_element::do_derived_mark ()
{
  return SCM_EOL;
}


SCM
ly_set_elt_property (SCM elt, SCM sym, SCM val)
{
  Score_element * sc = unsmob_element (elt);

  if (!gh_symbol_p (sym))
    {
      error ("Not a symbol");
      ly_display_scm (sym);
      return SCM_UNSPECIFIED;
    }

  if (sc)
    {
      sc->set_elt_property (sym, val);
    }
  else
    {
      error ("Not a score element");
      ly_display_scm (elt);
    }

  return SCM_UNSPECIFIED;
}


SCM
ly_get_elt_property (SCM elt, SCM sym)
{
  Score_element * sc = unsmob_element (elt);
  
  if (sc)
    {
      return sc->get_elt_property (sym);
    }
  else
    {
      error ("Not a score element");
      ly_display_scm (elt);
    }
  return SCM_UNSPECIFIED;
}


void
Score_element::discretionary_processing()
{
}



SCM
spanner_get_bound (SCM slur, SCM dir)
{
  return dynamic_cast<Spanner*> (unsmob_element (slur))->get_bound (to_dir (dir))->self_scm ();
}



static SCM interfaces_sym;
static void
init_functions ()
{
  interfaces_sym = scm_permanent_object (ly_symbol2scm ("interfaces"));

  scm_make_gsubr ("ly-get-elt-property", 2, 0, 0, (Scheme_function_unknown)ly_get_elt_property);
  scm_make_gsubr ("ly-set-elt-property", 3, 0, 0, (Scheme_function_unknown)ly_set_elt_property);
  scm_make_gsubr ("ly-get-spanner-bound", 2 , 0, 0, (Scheme_function_unknown) spanner_get_bound);
}

bool
Score_element::has_interface (SCM k)
{
  SCM ifs = get_elt_property (interfaces_sym);

  return scm_memq (k, ifs) != SCM_BOOL_F;
}

void
Score_element::set_interface (SCM k)
{
  if (has_interface (k))
    return ;
  else
    {
      set_elt_property (interfaces_sym,
			gh_cons  (k, get_elt_property (interfaces_sym)));
    }
}


ADD_SCM_INIT_FUNC(scoreelt, init_functions);
IMPLEMENT_TYPE_P(Score_element, "ly-element?");
