/*
  grob.cc -- implement Grob

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include <string.h>
#include <math.h>

#include "main.hh"
#include "input-smob.hh"
#include "warn.hh"
#include "group-interface.hh"
#include "misc.hh"
#include "paper-score.hh"
#include "molecule.hh"
#include "grob.hh"
#include "warn.hh"
#include "spanner.hh"
#include "system.hh"
#include "item.hh"
#include "molecule.hh"
#include "misc.hh"
#include "music.hh"
#include "item.hh"

#include "ly-smobs.icc"

/*
TODO:

remove dynamic_cast<Spanner,Item> and put this code into respective
  subclass.
*/

//#define HASHING_FOR_MUTABLE_PROPS
#define HASH_SIZE 3
#define INFINITY_MSG "Infinity or NaN encountered"

Grob::Grob (SCM basicprops)
{
  /*
    fixme: default should be no callback.
   */

  pscore_=0;
  status_ = 0;
  original_ = 0;
  immutable_property_alist_ =  basicprops;
  mutable_property_alist_ = SCM_EOL;

  /*
    We do smobify_self() as the first step. Since the object lives on
    the heap, none of its SCM variables are protected from GC. After
    smobify_self(), they are.
   */
  smobify_self ();


#ifdef HASHING_FOR_MUTABLE_PROPS
  mutable_property_alist_ = scm_c_make_hash_table (HASH_SIZE);
#endif
  
  SCM meta = get_grob_property ("meta");
  if (gh_pair_p (meta))
    {
      SCM ifs = scm_assoc (ly_symbol2scm ("interfaces"), meta);

      /*
	Switch off interface checks for the moment.
       */
      bool itc = internal_type_checking_global_b;
      internal_type_checking_global_b = false;
      internal_set_grob_property (ly_symbol2scm ("interfaces"), gh_cdr(ifs));
      internal_type_checking_global_b = itc;
    }
  
  /*
    TODO:

    destill this into a function, so we can re-init the immutable
    properties with a new BASICPROPS value after creation. Convenient
    eg. when using \override with StaffSymbol.  */
  
  char const*onames[] = {"X-offset-callbacks", "Y-offset-callbacks"};
  char const*enames[] = {"X-extent-callback", "Y-extent-callback"};
  
  for (int a = X_AXIS; a <= Y_AXIS; a++)
    {
      SCM l = get_grob_property (onames[a]);

      if (scm_ilength (l) >=0)
	{
	  dim_cache_[a].offset_callbacks_ = l;
	  dim_cache_[a].offsets_left_ = scm_ilength (l);
	}
      else
	{
	  programming_error ("[XY]-offset-callbacks must be a list");
	}

      SCM cb = get_grob_property (enames[a]);

      /*
	Should change default to be empty? 
      */
      if (cb != SCM_BOOL_F
	  && !gh_procedure_p (cb) && !gh_pair_p (cb)
	  && gh_procedure_p (get_grob_property ("molecule-callback"))
	  )
	cb = molecule_extent_proc;
    
      dim_cache_[a].dimension_ = cb;
    }

}

Grob::Grob (Grob const&s)
   : dim_cache_ (s.dim_cache_)
{
  original_ = (Grob*) &s;
  immutable_property_alist_ = s.immutable_property_alist_;

  mutable_property_alist_ = SCM_EOL;
  
  /*
    No properties are copied. That is the job of handle_broken_dependencies.
   */
  
  status_ = s.status_;
  pscore_ = s.pscore_;

  smobify_self ();

#ifdef HASHING_FOR_MUTABLE_PROPS
  mutable_property_alist_ = scm_c_make_hash_table (HASH_SIZE);
#endif
}

Grob::~Grob ()
{
  /*
    do nothing scm-ish and no unprotecting here.
   */
}





MAKE_SCHEME_CALLBACK (Grob,molecule_extent,2);
SCM
Grob::molecule_extent (SCM element_smob, SCM scm_axis)
{
  Grob *s = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (scm_axis);

  Molecule *m = s->get_molecule ();
  Interval e ;
  if (m)
    e = m->extent (a);
  return ly_interval2scm (e);
}

MAKE_SCHEME_CALLBACK (Grob,preset_extent,2);
SCM
Grob::preset_extent (SCM element_smob, SCM scm_axis)
{
  Grob *s = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (scm_axis);

  SCM ext = s->get_grob_property ((a == X_AXIS)
				 ? "X-extent"
				 : "Y-extent");
  
  if (gh_pair_p (ext))
    {
      Real l = gh_scm2double (ly_car (ext));
      Real r = gh_scm2double (ly_cdr (ext));
      return ly_interval2scm (Interval (l, r));
    }
  
  return ly_interval2scm (Interval ());
}



Paper_def*
Grob::get_paper ()  const
{
 return pscore_ ? pscore_->paper_ : 0;
}

void
Grob::calculate_dependencies (int final, int busy, SCM funcname)
{
  if (status_ >= final)
    return;

  if (status_== busy)
    {
      programming_error ("Element is busy, come back later");
      return;
    }
  
  status_= busy;

  for (SCM d = get_grob_property ("dependencies"); gh_pair_p (d);
       d = ly_cdr (d))
    {
      unsmob_grob (ly_car (d))
	->calculate_dependencies (final, busy, funcname);
    }

  
  SCM proc = internal_get_grob_property (funcname);
  if (gh_procedure_p (proc))
    gh_call1 (proc, this->self_scm ());
 
  status_= final;
}

Molecule *
Grob::get_molecule ()  const
{
  if (!live())
    {
      return 0;
      
    }
  
  SCM mol = get_grob_property ("molecule");
  if (unsmob_molecule (mol))
    return unsmob_molecule (mol);

  mol =  get_uncached_molecule ();
  
  Grob *me = (Grob*)this;
  me->set_grob_property ("molecule", mol);
  
  return unsmob_molecule (mol);  
}

SCM
Grob::get_uncached_molecule ()const
{
  SCM proc = get_grob_property ("molecule-callback");

  SCM  mol = SCM_EOL;
  if (gh_procedure_p (proc)) 
    mol = gh_apply (proc, scm_list_n (this->self_scm (), SCM_UNDEFINED));

  
  Molecule *m = unsmob_molecule (mol);
  
  if (unsmob_molecule (mol))
    {
      SCM origin = ly_symbol2scm ("no-origin");
      
      if (store_locations_global_b)
	{
	  SCM cause = get_grob_property ("cause");
	  if (Music*m = unsmob_music (cause))
	    {
	      SCM music_origin = m->get_mus_property ("origin");
	      if (unsmob_input (music_origin))
		origin = music_origin;
	    }
      }

      // ugr.
      
      mol = Molecule (m->extent_box (),
		      scm_list_n (origin, m->get_expr (), SCM_UNDEFINED)
		      ). smobbed_copy ();

      m = unsmob_molecule (mol);
    }
  
  /*
    transparent retains dimensions of element.
   */
  if (m && to_boolean (get_grob_property ("transparent")))
    mol = Molecule (m->extent_box (), SCM_EOL).smobbed_copy ();

  return mol;
}

/*
  
  VIRTUAL STUBS

 */
void
Grob::do_break_processing ()
{
}

System *
Grob::get_system () const
{
  return 0;
}

void
Grob::add_dependency (Grob*e)
{
  if (e)
    {
      Pointer_group_interface::add_grob (this, ly_symbol2scm ("dependencies"),e);
    }
  else
    programming_error ("Null dependency added");
}


void
Grob::handle_broken_dependencies ()
{
  Spanner * sp = dynamic_cast<Spanner*> (this);
  if (original_ && sp)
    return;

  if (sp)
    {
      for (SCM s = mutable_property_alist_; gh_pair_p(s);
	   s = gh_cdr(s))
	{
	  sp->substitute_one_mutable_property (gh_caar (s),
					      gh_cdar (s));
	  
	}
    }

  System *system = get_system ();

  if (live ()
      && system && common_refpoint (system, X_AXIS) && common_refpoint (system, Y_AXIS))
    {
      substitute_mutable_properties (system ? system->self_scm () : SCM_UNDEFINED,
				     mutable_property_alist_);
    }
  else if (dynamic_cast <System*> (this))
    {
      substitute_mutable_properties (SCM_UNDEFINED, mutable_property_alist_);
    }
  else
    {
      /*
	This element is `invalid'; it has been removed from all
	dependencies, so let's junk the element itself.

	do not do this for System, since that would remove references
	to the originals of score-grobs, which get then GC'd (a bad
	thing.)
 
      */
      suicide ();
    }
}

/*
 Note that we still want references to this element to be
 rearranged, and not silently thrown away, so we keep pointers
 like {broken_into_{drul,array}, original}
*/
void
Grob::suicide ()
{
  mutable_property_alist_ = SCM_EOL;
  immutable_property_alist_ = SCM_EOL;

  set_extent (SCM_EOL, Y_AXIS);
  set_extent (SCM_EOL, X_AXIS);

  for (int a= X_AXIS; a <= Y_AXIS; a++)
    {
      dim_cache_[a].offset_callbacks_ = SCM_EOL;
      dim_cache_[a].offsets_left_ = 0;
    }
}

void
Grob::handle_prebroken_dependencies ()
{
  /*
    Don't do this in the derived method, since we want to keep access to
    mutable_property_alist_ centralized.
   */
  if (original_)
    {
      Item * it = dynamic_cast<Item*> (this);
      substitute_mutable_properties (gh_int2scm (it->break_status_dir ()),
			       original_->mutable_property_alist_);
    }
}

Grob*
Grob::find_broken_piece (System*) const
{
  return 0;
}

/*
  translate in one direction
*/
void
Grob::translate_axis (Real y, Axis a)
{
  if (isinf (y) || isnan (y))
    programming_error (_ (INFINITY_MSG));
  else
    {
      dim_cache_[a].offset_ += y;
    }
}  


/*
  Find the offset relative to D.  If   D equals THIS, then it is 0.
  Otherwise, it recursively defd as
  
  OFFSET_ + PARENT_L_->relative_coordinate (D)
*/
Real
Grob::relative_coordinate (Grob const*refp, Axis a) const
{
  if (refp == this)
    return 0.0;

  /*
    We catch PARENT_L_ == nil case with this, but we crash if we did
    not ask for the absolute coordinate (ie. REFP == nil.)
    
   */
  if (refp == dim_cache_[a].parent_)
    return get_offset (a);
  else
    return get_offset (a) + dim_cache_[a].parent_->relative_coordinate (refp, a);
}


  
/*
  Invoke callbacks to get offset relative to parent.
*/
Real
Grob::get_offset (Axis a) const
{
  Grob *me = (Grob*) this;
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


MAKE_SCHEME_CALLBACK (Grob,point_dimension_callback,2);
SCM
Grob::point_dimension_callback (SCM , SCM)
{
  return ly_interval2scm (Interval (0,0));
}

bool
Grob::empty_b (Axis a)const
{
  return ! (gh_pair_p (dim_cache_[a].dimension_) ||
	    gh_procedure_p (dim_cache_[a].dimension_));
}

Interval
Grob::extent (Grob * refp, Axis a) const
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
      d->dimension_= gh_call2 (d->dimension_, self_scm (), gh_int2scm (a));
    }
  else
    return ext;

  if (!gh_pair_p (d->dimension_))
    return ext;
  
  ext = ly_scm2interval (d->dimension_);

  SCM extra = get_grob_property (a == X_AXIS
				? "extra-X-extent"
				: "extra-Y-extent");

  /*
    signs ?
   */
  if (gh_pair_p (extra))
    {
      ext[BIGGER] +=  gh_scm2double (ly_cdr (extra));
      ext[SMALLER] +=   gh_scm2double (ly_car (extra));
    }
  
  extra = get_grob_property (a == X_AXIS
				? "minimum-X-extent"
				: "minimum-Y-extent");
  if (gh_pair_p (extra))
    {
      ext.unite (Interval (gh_scm2double (ly_car (extra)),
			   gh_scm2double (ly_cdr (extra))));
    }

  ext.translate (x);
  
  return ext;
}

/*
  Find the group-element which has both #this# and #s#
*/
Grob * 
Grob::common_refpoint (Grob const* s, Axis a) const
{
  /*
    I don't like the quadratic aspect of this code, but I see no other
    way. The largest chain of parents might be 10 high or so, so
    it shouldn't be a real issue. */
  for (Grob const *c = this; c; c = c->dim_cache_[a].parent_)
    for (Grob const * d = s; d; d = d->dim_cache_[a].parent_)
      if (d == c)
	return (Grob*)d;

  return 0;
}


Grob *
common_refpoint_of_list (SCM elist, Grob *common, Axis a) 
{
  for (; gh_pair_p (elist); elist = ly_cdr (elist))
    {
      Grob * s = unsmob_grob (ly_car (elist));
      if (!s)
	continue;
      if (common)
	common = common->common_refpoint (s, a);
      else
	common = s;
    }

  return common;
}



Grob *
common_refpoint_of_array (Link_array<Grob> const &arr, Grob *common, Axis a) 
{
  for (int i = arr.size() ; i--; )
    {
      Grob * s = arr[i];
      if (!s)
	continue;

      if (common)
	common = common->common_refpoint (s, a);
      else
	common = s;
    }

  return common;
}

String
Grob::name () const
{
  SCM meta = get_grob_property ("meta");
  SCM nm = scm_assoc (ly_symbol2scm ("name"), meta);
  nm = (gh_pair_p (nm)) ? ly_cdr (nm) : SCM_EOL;
  return  gh_symbol_p (nm) ? ly_symbol2string (nm) :  classname (this);  
}

void
Grob::add_offset_callback (SCM cb, Axis a)
{
  if (!has_offset_callback_b (cb, a))
  {
    dim_cache_[a].offset_callbacks_ = gh_cons (cb, dim_cache_[a].offset_callbacks_);
    dim_cache_[a].offsets_left_ ++;
  }
}

bool
Grob::has_extent_callback_b (SCM cb, Axis a)const
{
  return scm_equal_p (cb, dim_cache_[a].dimension_) == SCM_BOOL_T;
}


bool
Grob::has_offset_callback_b (SCM cb, Axis a)const
{
  return scm_memq (cb, dim_cache_[a].offset_callbacks_) != SCM_BOOL_F;
}

void
Grob::set_extent (SCM dc, Axis a)
{
  dim_cache_[a].dimension_ =dc;
}

void
Grob::set_parent (Grob *g, Axis a)
{
  dim_cache_[a].parent_ = g;
}

MAKE_SCHEME_CALLBACK (Grob,fixup_refpoint,1);
SCM
Grob::fixup_refpoint (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  for (int a = X_AXIS; a < NO_AXES; a ++)
    {
      Axis ax = (Axis)a;
      Grob * parent = me->get_parent (ax);

      if (!parent)
	continue;
      
      if (parent->get_system () != me->get_system () && me->get_system ())
	{
	  Grob * newparent = parent->find_broken_piece (me->get_system ());
	  me->set_parent (newparent, ax);
	}

      if (Item * i  = dynamic_cast<Item*> (me))
	{
	  Item *parenti = dynamic_cast<Item*> (parent);

	  if (parenti && i)
	    {
	      Direction  my_dir = i->break_status_dir () ;
	      if (my_dir!= parenti->break_status_dir ())
		{
		  Item *newparent =  parenti->find_prebroken_piece (my_dir);
		  me->set_parent (newparent, ax);
		}
	    }
	}
    }
  return smob;
}

void
Grob::warning (String s)const
{
  SCM cause = self_scm();
  while (cause != SCM_EOL && !unsmob_music (cause))
    {
      Grob * g = unsmob_grob (cause);
      cause = g->get_grob_property ("cause");
    }

  if (Music *m = unsmob_music (cause))
    {
      m->origin()->warning (s);
    }
  else
    ::warning (s);
}

void
Grob::programming_error (String s)const
{
  s = "Programming error: "  + s;
  warning (s);
}


/****************************************************
  SMOB funcs
 ****************************************************/



IMPLEMENT_SMOBS (Grob);
IMPLEMENT_DEFAULT_EQUAL_P (Grob);

SCM
Grob::mark_smob (SCM ses)
{
  Grob * s = (Grob*) SCM_CELL_WORD_1 (ses);
  scm_gc_mark (s->immutable_property_alist_);

  for (int a =0 ; a < 2; a++)
    {
      scm_gc_mark (s->dim_cache_[a].offset_callbacks_);
      scm_gc_mark (s->dim_cache_[a].dimension_);
      
      /*
	don't mark the parents. The pointers in the mutable property
	list form two tree like structures (one for X relations, one
	for Y relations). Marking these can be done in limited stack
	space.  If we add the parents, we will jump between X and Y in
	an erratic manner, leading to much more recursion depth (and
	core dumps if we link to pthreads.)
       */
    }
  
  if (s->original_)
    scm_gc_mark (s->original_->self_scm ());

  s->do_derived_mark ();  
  return s->mutable_property_alist_;
}

int
Grob::print_smob (SCM s, SCM port, scm_print_state *)
{
  Grob *sc = (Grob *) ly_cdr (s);
     
  scm_puts ("#<Grob ", port);
  scm_puts ((char *)sc->name ().to_str0 (), port);

  /*
    don't try to print properties, that is too much hassle.
   */
  scm_puts (" >", port);
  return 1;
}

SCM
Grob::do_derived_mark ()
{
  return SCM_EOL;
}



void
Grob::discretionary_processing ()
{
}

bool
Grob::internal_has_interface (SCM k)
{
  SCM ifs = get_grob_property ("interfaces");

  return scm_memq (k, ifs) != SCM_BOOL_F;
}


/** Return Array of Grobs in SCM list L */
Link_array<Grob>
ly_scm2grobs (SCM l)
{
  Link_array<Grob> arr;

  for (SCM s = l; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM e = gh_car (s);
      arr.push (unsmob_grob (e));
    }

  arr.reverse ();
  return arr;
}

/** Return SCM list of Grob array A */
SCM
ly_grobs2scm (Link_array<Grob> a)
{
  SCM s = SCM_EOL;
  for (int i = a.size (); i; i--)
    s = gh_cons (a[i-1]->self_scm (), s);

  return s;
}


IMPLEMENT_TYPE_P (Grob, "ly-grob?");

ADD_INTERFACE (Grob, "grob-interface",
  "In music notation, lots of symbols are related in some way.  You can
think of music notation as a graph where nodes are formed by the
symbols, and the arcs by their relations. A grob is a node in that
graph.  The directed edges in the graph are formed by references to
other grobs (i.e. pointers).  This big graph of grobs specifies the
notation problem. The solution of this problem is a description of the
printout in closed form, i.e. a list of values.  These values are
Molecules.

All grobs have an X and Y-position on the page.  These X and Y positions
are stored in a relative format, so they can easily be combined by
stacking them, hanging one grob to the side of another, and coupling
them into a grouping-grob.

Each grob has a reference point (a.k.a.  parent): the position of a grob
is stored relative to that reference point. For example the X-reference
point of a staccato dot usually is the note head that it applies
to. When the note head is moved, the staccato dot moves along
automatically.

A grob is often associated with a symbol, but some grobs do not print
any symbols. They take care of grouping objects. For example, there is a
separate grob that stacks staves vertically. The @ref{NoteCollision}
is also an abstract grob: it only moves around chords, but doesn't print
anything.
",
  "X-offset-callbacks Y-offset-callbacks X-extent-callback molecule cause
Y-extent-callback molecule-callback extra-offset spacing-procedure
staff-symbol interfaces dependencies X-extent Y-extent extra-X-extent
causes meta layer before-line-breaking-callback
after-line-breaking-callback extra-Y-extent minimum-X-extent
minimum-Y-extent transparent");


