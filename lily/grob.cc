/*
  grob.cc -- implement Grob

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include <string.h>
#include <math.h>

#include "main.hh"
#include "input-smob.hh"

#include "group-interface.hh"
#include "misc.hh"
#include "paper-score.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "grob.hh"
#include "debug.hh"
#include "spanner.hh"
#include "system.hh"
#include "item.hh"
#include "paper-column.hh"
#include "molecule.hh"
#include "misc.hh"
#include "paper-outputter.hh"
#include "music.hh"
#include "item.hh"

#include "ly-smobs.icc"

/*
TODO:

remove dynamic_cast<Spanner,Item> and put this code into respective
  subclass.
*/


#define INFINITY_MSG "Infinity or NaN encountered"

Grob::Grob (SCM basicprops)
{
  /*
    fixme: default should be no callback.
   */

  pscore_l_=0;
  status_c_ = 0;
  original_l_ = 0;
  immutable_property_alist_ =  basicprops;
  mutable_property_alist_ = SCM_EOL;

  /*
    We do smobify_self() as the first step. Since the object lives on
    the heap, none of its SCM variables are protected from GC. After
    smobify_self(), they are.
   */
  smobify_self ();


  SCM meta = get_grob_property ("meta");
  if (gh_pair_p (meta))
    {
      SCM ifs = scm_assoc (ly_symbol2scm ("interfaces"), meta);

      /*
	do it directly to bypass interface checks.
       */
      mutable_property_alist_ = gh_cons (gh_cons (ly_symbol2scm ("interfaces"),
						  gh_cdr (ifs)),
					 mutable_property_alist_);
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
  original_l_ = (Grob*) &s;
  immutable_property_alist_ = s.immutable_property_alist_;
  mutable_property_alist_ = SCM_EOL;

  /*
    No properties are copied. That is the job of handle_broken_dependencies.
   */
  
  status_c_ = s.status_c_;
  pscore_l_ = s.pscore_l_;

  smobify_self ();


}

Grob::~Grob ()
{
  /*
    do nothing scm-ish and no unprotecting here.
   */
}



extern void check_interfaces_for_property (Grob const *me, SCM sym);

void
Grob::internal_set_grob_property (SCM s, SCM v)
{
#ifndef NDEBUG
  if (internal_type_checking_global_b)
    {
      assert (type_check_assignment (s, v, ly_symbol2scm ("backend-type?")));
      check_interfaces_for_property(this, s);
    }
#endif

  
  mutable_property_alist_ = scm_assq_set_x (mutable_property_alist_, s, v);
}


SCM
Grob::internal_get_grob_property (SCM sym) const
{
  SCM s = scm_sloppy_assq (sym, mutable_property_alist_);
  if (s != SCM_BOOL_F)
    return ly_cdr (s);

  s = scm_sloppy_assq (sym, immutable_property_alist_);
  
#ifndef NDEBUG
  if (internal_type_checking_global_b && gh_pair_p (s))
    {
      assert (type_check_assignment (sym, gh_cdr (s), ly_symbol2scm ("backend-type?")));
      check_interfaces_for_property(this, sym);
    }
#endif

  return (s == SCM_BOOL_F) ? SCM_EOL : ly_cdr (s); 
}

/*
  Remove the value associated with KEY, and return it. The result is
  that a next call will yield SCM_EOL (and not the underlying
  `basic' property.
*/
SCM
Grob::remove_grob_property (const char* key)
{
  SCM val = get_grob_property (key);
  if (val != SCM_EOL)
    set_grob_property (key, SCM_EOL);
  return val;
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
				 ? "extent-X"
				 : "extent-Y");
  
  if (gh_pair_p (ext))
    {
      Real l = gh_scm2double (ly_car (ext));
      Real r = gh_scm2double (ly_cdr (ext));
      return ly_interval2scm (Interval (l, r));
    }
  
  return ly_interval2scm (Interval ());
}



Paper_def*
Grob::paper_l ()  const
{
 return pscore_l_ ? pscore_l_->paper_l_ : 0;
}

void
Grob::calculate_dependencies (int final, int busy, SCM funcname)
{
  if (status_c_ >= final)
    return;

  if (status_c_== busy)
    {
      programming_error ("Element is busy, come back later");
      return;
    }
  
  status_c_= busy;

  for (SCM d = get_grob_property ("dependencies"); gh_pair_p (d);
       d = ly_cdr (d))
    {
      unsmob_grob (ly_car (d))
	->calculate_dependencies (final, busy, funcname);
    }

  
  SCM proc = internal_get_grob_property (funcname);
  if (gh_procedure_p (proc))
    gh_call1 (proc, this->self_scm ());
 
  status_c_= final;
}

Molecule *
Grob::get_molecule ()  const
{
  if (immutable_property_alist_ == SCM_EOL)
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
      
      if (store_locations_global_b){
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
Grob::line_l () const
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




/**
      Do break substitution in S, using CRITERION. Return new value.
      CRITERION is either a SMOB pointer to the desired line, or a number
      representing the break direction. Do not modify SRC.

      It is rather tightly coded, since it takes a lot of time; it is
      one of the top functions in the profile.

*/
SCM
Grob::handle_broken_grobs (SCM src, SCM criterion)
{
 again:
  Grob *sc = unsmob_grob (src);
  if (sc)
    {
      if (SCM_INUMP (criterion))
	{
	  Item * i = dynamic_cast<Item*> (sc);
	  Direction d = to_dir (criterion);
	  if (i && i->break_status_dir () != d)
	    {
	      Item *br = i->find_prebroken_piece (d);
	      return (br) ? br->self_scm () : SCM_UNDEFINED;
	    }
	}
      else
	{
	  System * line
	    = dynamic_cast<System*> (unsmob_grob (criterion));
	  if (sc->line_l () != line)
	    {
	      sc = sc->find_broken_piece (line);

	    }

	  /* now: !sc || (sc && sc->line_l () == line) */
	  if (!sc)
	    return SCM_UNDEFINED;

	  /* now: sc && sc->line_l () == line */
	  if (!line)
	    return sc->self_scm();
	  /*
	    This was introduced in 1.3.49 as a measure to prevent
	    programming errors. It looks expensive (?).

	    TODO:
		
	    benchmark , document when (what kind of programming
	    errors) this happens.
	  */
	  if (sc->common_refpoint (line, X_AXIS)
	       && sc->common_refpoint (line, Y_AXIS))
	    {
	      return sc->self_scm ();
	    }
	  return SCM_UNDEFINED;
	}
    }
  else if (ly_pair_p (src)) // SCM_CONSP (src))  // huh?
    {
      SCM oldcar =ly_car (src);
      /*
	UGH! breaks on circular lists.
      */
      SCM newcar = handle_broken_grobs (oldcar, criterion);
      SCM oldcdr = ly_cdr (src);
      
      if (newcar == SCM_UNDEFINED
	  && (gh_pair_p (oldcdr) || oldcdr == SCM_EOL))
	{
	  /*
	    This is tail-recursion, ie. 
	    
	    return handle_broken_grobs (cdr, criterion);

	    We don't want to rely on the compiler to do this.  Without
	    tail-recursion, this easily crashes with a stack overflow.  */
	  src =  oldcdr;
	  goto again;
	}

      SCM newcdr = handle_broken_grobs (oldcdr, criterion);
      return scm_cons (newcar, newcdr);
    }
  else
    return src;

  return src;
}

void
Grob::handle_broken_dependencies ()
{
  Spanner * s= dynamic_cast<Spanner*> (this);
  if (original_l_ && s)
    return;

  if (s)
    {
      for (int i = 0;  i< s->broken_into_l_arr_ .size (); i++)
	{
	  Grob * sc = s->broken_into_l_arr_[i];
	  System * l = sc->line_l ();
	  sc->mutable_property_alist_ =
	    handle_broken_grobs (mutable_property_alist_,
				 l ? l->self_scm () : SCM_UNDEFINED);
	}
    }


  System *line = line_l ();

  if (line && common_refpoint (line, X_AXIS) && common_refpoint (line, Y_AXIS))
    {
      mutable_property_alist_
	= handle_broken_grobs (mutable_property_alist_,
			       line ? line->self_scm () : SCM_UNDEFINED);
    }
  else if (dynamic_cast <System*> (this))
    {
      mutable_property_alist_ = handle_broken_grobs (mutable_property_alist_,
					    SCM_UNDEFINED);
    }
  else
    {
      /*
	This element is `invalid'; it has been removed from all
	dependencies, so let's junk the element itself.

	do not do this for System, since that would remove
	references to the originals of score-grobs, which get then GC'd
 (a bad thing.)
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
}

Grob*
Grob::find_broken_piece (System*) const
{
  return 0;
}

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

Real
Grob::relative_coordinate (Grob const*refp, Axis a) const
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
				? "extra-extent-X"
				: "extra-extent-Y");

  /*
    signs ?
   */
  if (gh_pair_p (extra))
    {
      ext[BIGGER] +=  gh_scm2double (ly_cdr (extra));
      ext[SMALLER] +=   gh_scm2double (ly_car (extra));
    }
  
  extra = get_grob_property (a == X_AXIS
				? "minimum-extent-X"
				: "minimum-extent-Y");
  if (gh_pair_p (extra))
    {
      ext.unite (Interval (gh_scm2double (ly_car (extra)),
			   gh_scm2double (ly_cdr (extra))));
    }

  ext.translate (x);
  
  return ext;
}

Grob * 
Grob::common_refpoint (Grob const* s, Axis a) const
{
  /*
    I don't like the quadratic aspect of this code, but I see no other
    way. The largest chain of parents might be 10 high or so, so
    it shouldn't be a real issue. */
  for (Grob const *c = this; c; c = c->dim_cache_[a].parent_l_)
    for (Grob const * d = s; d; d = d->dim_cache_[a].parent_l_)
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
  dim_cache_[a].parent_l_ = g;
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
      
      if (parent->line_l () != me->line_l () && me->line_l ())
	{
	  Grob * newparent = parent->find_broken_piece (me->line_l ());
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
  scm_gc_mark (s->mutable_property_alist_);

  for (int a =0 ; a < 2; a++)
    {
      scm_gc_mark (s->dim_cache_[a].offset_callbacks_);
      scm_gc_mark (s->dim_cache_[a].dimension_);
      Grob *p = s->get_parent (Y_AXIS);
      if (p)
	scm_gc_mark (p->self_scm ());
    }
  
  if (s->original_l_)
    scm_gc_mark (s->original_l_->self_scm ());

  return s->do_derived_mark ();
}

int
Grob::print_smob (SCM s, SCM port, scm_print_state *)
{
  Grob *sc = (Grob *) ly_cdr (s);
     
  scm_puts ("#<Grob ", port);
  scm_puts ((char *)sc->name ().ch_C (), port);

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

LY_DEFINE(ly_set_grob_property,"ly-set-grob-property", 3, 0, 0,
(SCM grob, SCM sym, SCM val),
"
Set @var{sym} in grob @var{grob} to value @var{val}")
{
  Grob * sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE(sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE(gh_symbol_p(sym), sym, SCM_ARG2, __FUNCTION__, "symbol");  

  if (!type_check_assignment (sym, val, ly_symbol2scm ("backend-type?")))
    error ("typecheck failed");
      
  sc->internal_set_grob_property (sym, val);
  return SCM_UNSPECIFIED;
}

LY_DEFINE(ly_get_grob_property,
	  "ly-get-grob-property", 2, 0, 0, (SCM grob, SCM sym),
	  "  Get the value of a value in grob @var{g} of property @var{sym}. It
will return @code{'()} (end-of-list) if @var{g} doesn't have @var{sym} set.
")
{
  Grob * sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE(sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE(gh_symbol_p(sym), sym, SCM_ARG2, __FUNCTION__, "symbol");  

  return sc->internal_get_grob_property (sym);
}


void
Grob::discretionary_processing ()
{
}


LY_DEFINE(spanner_get_bound, "ly-get-spanner-bound", 2 , 0, 0,
	  (SCM slur, SCM dir),
	  "Get one of the bounds of @var{spanner}. @var{dir} may be @code{-1} for
left, and @code{1} for right.
")
{
  Spanner * sl = dynamic_cast<Spanner*> (unsmob_grob (slur));
  SCM_ASSERT_TYPE(sl, slur, SCM_ARG1, __FUNCTION__, "spanner grob");
  SCM_ASSERT_TYPE(ly_dir_p (dir), slur, SCM_ARG2, __FUNCTION__, "dir");
  return sl->get_bound (to_dir (dir))->self_scm ();
}

LY_DEFINE(ly_get_paper_var,"ly-get-paper-variable", 2, 0, 0,
  (SCM grob, SCM sym),
  "Get a variable from the \\paper block.")
{
  Grob * sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE(sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE(gh_symbol_p(sym), sym, SCM_ARG2, __FUNCTION__, "symbol");  

  return sc->paper_l() ->get_scmvar_scm (sym);
}



LY_DEFINE(ly_get_extent, "ly-get-extent", 3, 0, 0,
	  (SCM grob, SCM refp, SCM axis),
	  "Get the extent in @var{axis} direction of @var{grob} relative to the
grob @var{refp}")
{
  Grob * sc = unsmob_grob (grob);
  Grob * ref = unsmob_grob (refp);
  SCM_ASSERT_TYPE(sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE(ref, refp, SCM_ARG2, __FUNCTION__, "grob");
  
  SCM_ASSERT_TYPE(ly_axis_p(axis), axis, SCM_ARG3, __FUNCTION__, "axis");

  return ly_interval2scm ( sc->extent (ref, Axis (gh_scm2int (axis))));
}

LY_DEFINE (ly_get_parent,   "ly-get-parent", 2, 0, 0, (SCM grob, SCM axis),
	   "Get the parent of @var{grob}.  @var{axis} can be 0 for the X-axis, 1
for the Y-axis.")
{
  Grob * sc = unsmob_grob (grob);
  SCM_ASSERT_TYPE(sc, grob, SCM_ARG1, __FUNCTION__, "grob");
  SCM_ASSERT_TYPE(ly_axis_p(axis), axis, SCM_ARG2, __FUNCTION__, "axis");

  return sc->get_parent (Axis (gh_scm2int (axis)))->self_scm();
}


bool
Grob::internal_has_interface (SCM k)
{
  SCM ifs = get_grob_property ("interfaces");

  return scm_memq (k, ifs) != SCM_BOOL_F;
}

IMPLEMENT_TYPE_P (Grob, "ly-grob?");

ADD_INTERFACE (Grob, "grob-interface",
  "All grobs support this",
  "X-offset-callbacks Y-offset-callbacks X-extent-callback molecule cause
Y-extent-callback molecule-callback extra-offset
spacing-procedure
staff-symbol interfaces dependencies extra-extent-X causes meta
layer before-line-breaking-callback after-line-breaking-callback extra-extent-Y minimum-extent-X minimum-extent-Y transparent");

