/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "grob.hh"

#include <cstring>

#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "input.hh"
#include "international.hh"
#include "item.hh"
#include "main.hh"
#include "misc.hh"
#include "music.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"
#include "program-option.hh"
#include "stencil.hh"
#include "stream-event.hh"
#include "system.hh"
#include "warn.hh"

#include "ly-smobs.icc"

Grob *
Grob::clone () const
{
  return new Grob (*this);
}

Grob::Grob (SCM basicprops)	    
{
  
  /* FIXME: default should be no callback.  */
  self_scm_ = SCM_EOL;
  layout_ = 0;
  original_ = 0;
  interfaces_ = SCM_EOL;
  immutable_property_alist_ = basicprops;
  mutable_property_alist_ = SCM_EOL;
  object_alist_ = SCM_EOL;
  
  /* We do smobify_self () as the first step.  Since the object lives
     on the heap, none of its SCM variables are protected from
     GC. After smobify_self (), they are.  */
  smobify_self ();

  SCM meta = get_property ("meta");
  if (scm_is_pair (meta))
    {
      interfaces_ = scm_cdr (scm_assq (ly_symbol2scm ("interfaces"), meta));

      SCM object_cbs = scm_assq (ly_symbol2scm ("object-callbacks"), meta);
      if (scm_is_pair (object_cbs))
	{
	  for (SCM s = scm_cdr (object_cbs); scm_is_pair (s); s = scm_cdr (s))
	    set_object (scm_caar (s), scm_cdar (s)); 
	}
    }
  
  if (get_property_data ("X-extent") == SCM_EOL)
    set_property ("X-extent", Grob::stencil_width_proc);
  if (get_property_data ("Y-extent") == SCM_EOL)
    set_property ("Y-extent", Grob::stencil_height_proc);
}

Grob::Grob (Grob const &s)
  : dim_cache_ (s.dim_cache_)
{
  original_ = (Grob *) & s;
  self_scm_ = SCM_EOL;

  immutable_property_alist_ = s.immutable_property_alist_;
  mutable_property_alist_ = ly_deep_copy (s.mutable_property_alist_);
  interfaces_ = s.interfaces_;
  object_alist_ = SCM_EOL;

  layout_ = 0;

  smobify_self ();
}

Grob::~Grob ()
{
}
/****************************************************************
  STENCILS
****************************************************************/

Stencil *
Grob::get_stencil () const
{
  if (!is_live ())
    return 0;

  SCM stil = get_property ("stencil");
  return unsmob_stencil (stil);
}

Stencil
Grob::get_print_stencil () const
{
  SCM stil = get_property ("stencil");

  Stencil retval;
  if (Stencil *m = unsmob_stencil (stil))
    {
      retval = *m;
      if (to_boolean (get_property ("transparent")))
	retval = Stencil (m->extent_box (), SCM_EOL);
      else
	{
	  SCM expr = m->expr ();
	  expr = scm_list_3 (ly_symbol2scm ("grob-cause"),
			     self_scm (), expr);

	  retval = Stencil (m->extent_box (), expr);
	}

      SCM rot = get_property ("rotation");
      if (scm_is_pair (rot))
	{
	  Real angle = scm_to_double (scm_car (rot));
	  Real x = scm_to_double (scm_cadr (rot));
	  Real y = scm_to_double (scm_caddr (rot));

	  retval.rotate_degrees (angle, Offset (x, y));
	}

      /* color support... see interpret_stencil_expression () for more... */
      SCM color = get_property ("color");
      if (scm_is_pair (color))
	{
	  SCM expr = scm_list_3 (ly_symbol2scm ("color"),
				 color,
				 retval.expr ());

	  retval = Stencil (retval.extent_box (), expr);
	}

      /* process whiteout */
      if (to_boolean (get_property ("whiteout")))
        {
          /* Call the scheme procedure stencil-whiteout in scm/stencils.scm */
          /* to add a round-filled-box stencil to the stencil list */
          retval
            = *unsmob_stencil (scm_call_1 (ly_lily_module_constant ("stencil-whiteout"),
                                           retval.smobbed_copy()));
        }
    }

  return retval;
}

/****************************************************************
  VIRTUAL STUBS
****************************************************************/
void
Grob::do_break_processing ()
{
}

void
Grob::discretionary_processing ()
{
}

System *
Grob::get_system () const
{
  return 0;
}


void
Grob::handle_broken_dependencies ()
{
  Spanner *sp = dynamic_cast<Spanner *> (this);
  if (original () && sp)
    return;

  if (sp)
    /* THIS, SP is the original spanner.  We use a special function
       because some Spanners have enormously long lists in their
       properties, and a special function fixes FOO  */
    {
      for (SCM s = object_alist_; scm_is_pair (s); s = scm_cdr (s))
	sp->substitute_one_mutable_property (scm_caar (s), scm_cdar (s));
    }
  System *system = get_system ();

  if (is_live ()
      && system
      && common_refpoint (system, X_AXIS)
      && common_refpoint (system, Y_AXIS))
    substitute_object_links (system->self_scm (), object_alist_);
  else if (dynamic_cast<System *> (this))
    substitute_object_links (SCM_UNDEFINED, object_alist_);
  else
    /* THIS element is `invalid'; it has been removed from all
       dependencies, so let's junk the element itself.

       Do not do this for System, since that would remove references
       to the originals of score-grobs, which get then GC'd (a bad
       thing).  */
    suicide ();
}

/* Note that we still want references to this element to be
   rearranged, and not silently thrown away, so we keep pointers like
   {broken_into_{drul, array}, original}
*/
void
Grob::suicide ()
{
  if (!is_live ())
    return;

  for (int a = X_AXIS; a < NO_AXES; a++)
    dim_cache_[a].clear ();

  mutable_property_alist_ = SCM_EOL;
  object_alist_ = SCM_EOL;
  immutable_property_alist_ = SCM_EOL;
  interfaces_ = SCM_EOL;
}

void
Grob::handle_prebroken_dependencies ()
{
  /* Don't do this in the derived method, since we want to keep access to
     object_alist_ centralized.  */
  if (original ())
    {
      Item *it = dynamic_cast<Item *> (this);
      substitute_object_links (scm_from_int (it->break_status_dir ()),
			       original ()->object_alist_);
    }
}

Grob *
Grob::find_broken_piece (System *) const
{
  return 0;
}

/****************************************************************
   OFFSETS
****************************************************************/

void
Grob::translate_axis (Real y, Axis a)
{
  if (isinf (y) || isnan (y))
    {
      programming_error (_ ("Infinity or NaN encountered"));
      return ;
    }
  
  if (!dim_cache_[a].offset_)
    dim_cache_[a].offset_ = new Real (y);
  else
    *dim_cache_[a].offset_ += y;  
}

/* Find the offset relative to D.  If D equals THIS, then it is 0.
   Otherwise, it recursively defd as

   OFFSET_ + PARENT_L_->relative_coordinate (D) */
Real
Grob::relative_coordinate (Grob const *refp, Axis a) const
{
  /* eaa - hmmm, should we do a programming_error() here? */
  if ((this == NULL) || (refp == this))
    return 0.0;

  /* We catch PARENT_L_ == nil case with this, but we crash if we did
     not ask for the absolute coordinate (ie. REFP == nil.)  */
  Real off = get_offset (a);
  if (refp == dim_cache_[a].parent_)
    return off;
  
  off += dim_cache_[a].parent_->relative_coordinate (refp, a);

  return off;
}

Real
Grob::pure_relative_y_coordinate (Grob const *refp, int start, int end)
{
  if (refp == this)
    return 0.0;

  Real off = 0;

  if (dim_cache_[Y_AXIS].offset_)
    {
      if (to_boolean (get_property ("pure-Y-offset-in-progress")))
	programming_error ("cyclic chain in pure-Y-offset callbacks");

      off = *dim_cache_[Y_AXIS].offset_;
    }
  else
    {
      SCM proc = get_property_data ("Y-offset");

      dim_cache_[Y_AXIS].offset_ = new Real (0.0);
      set_property ("pure-Y-offset-in-progress", SCM_BOOL_T);
      off = robust_scm2double (call_pure_function (proc,
						   scm_list_1 (self_scm ()),
						   start, end),
			       0.0);
      del_property ("pure-Y-offset-in-progress");
      delete dim_cache_[Y_AXIS].offset_;
      dim_cache_[Y_AXIS].offset_ = 0;
    }

  /* we simulate positioning-done if we are the child of a VerticalAlignment,
     but only if we don't have a cached offset. If we do have a cached offset,
     it probably means that the Alignment was fixed and it has already been
     calculated.
  */
  if (Grob *p = get_parent (Y_AXIS))
    {
      Real trans = 0;
      if (Align_interface::has_interface (p) && !dim_cache_[Y_AXIS].offset_)
	trans = Align_interface::get_pure_child_y_translation (p, this, start, end);

      return off + trans + p->pure_relative_y_coordinate (refp, start, end);
    }
  return off;
}

/* Invoke callbacks to get offset relative to parent.  */
Real
Grob::get_offset (Axis a) const
{
  if (dim_cache_[a].offset_)
    return *dim_cache_[a].offset_;

  Grob *me = (Grob *) this;

  SCM sym = axis_offset_symbol (a);
  me->dim_cache_[a].offset_ = new Real (0.0);

  /*
    UGH: can't fold next 2 statements together. Apparently GCC thinks
    dim_cache_[a].offset_ is unaliased.
  */
  Real off = robust_scm2double (internal_get_property (sym), 0.0);
  if (me->dim_cache_[a].offset_)
    {
      *me->dim_cache_[a].offset_ += off;
      me->del_property (sym);
      return *me->dim_cache_[a].offset_;
    }
  else
    return 0.0;
}

Real
Grob::maybe_pure_coordinate (Grob const *refp, Axis a, bool pure, int start, int end)
{
  if (pure && a != Y_AXIS)
    programming_error ("tried to get pure X-offset");
  return (pure && a == Y_AXIS) ? pure_relative_y_coordinate (refp, start, end)
    : relative_coordinate (refp, a);
}

/****************************************************************
  extents
****************************************************************/

void
Grob::flush_extent_cache (Axis axis)
{
  if (dim_cache_[axis].extent_)
    {
      /*
	Ugh, this is not accurate; will flush property, causing
	callback to be called if.
       */
      del_property ((axis == X_AXIS) ? ly_symbol2scm ("X-extent") : ly_symbol2scm ("Y-extent"));
      delete dim_cache_[axis].extent_;
      dim_cache_[axis].extent_ = 0;
      if (get_parent (axis))
	get_parent (axis)->flush_extent_cache (axis);
    }
}


Interval
Grob::extent (Grob *refp, Axis a) const
{
  Real offset = relative_coordinate (refp, a);
  Interval real_ext;
  if (dim_cache_[a].extent_)
    {
      real_ext = *dim_cache_[a].extent_;
    }
  else
    {
      /*
	Order is significant: ?-extent may trigger suicide.
       */
      SCM ext_sym =
	(a == X_AXIS)
	? ly_symbol2scm ("X-extent")
	: ly_symbol2scm ("Y-extent");
	
      SCM ext = internal_get_property (ext_sym);
      if (is_number_pair (ext))
	real_ext.unite (ly_scm2interval (ext));

      SCM min_ext_sym =
	(a == X_AXIS)
	? ly_symbol2scm ("minimum-X-extent")
	: ly_symbol2scm ("minimum-Y-extent");
      SCM min_ext = internal_get_property (min_ext_sym);
      if (is_number_pair (min_ext))
	real_ext.unite (ly_scm2interval (min_ext));

      ((Grob*)this)->dim_cache_[a].extent_ = new Interval (real_ext);  
    }
  
  real_ext.translate (offset);
  
  return real_ext;
}

Interval
Grob::pure_height (Grob *refp, int start, int end)
{
  SCM proc = get_property_data (ly_symbol2scm ("Y-extent"));
  SCM iv_scm = call_pure_function (proc,
				   scm_list_1 (self_scm ()),
				   start, end);
  Interval iv = robust_scm2interval (iv_scm, Interval (0, 0));
  Real offset = pure_relative_y_coordinate (refp, start, end);

  SCM min_ext = get_property ("minimum-Y-extent");

  /* we don't add minimum-Y-extent if the extent is empty. This solves
     a problem with Hara-kiri spanners. They would request_suicide and
     return empty extents, but we would force them here to be large. */
  if (!iv.is_empty () && is_number_pair (min_ext))
    iv.unite (ly_scm2interval (min_ext));

  if (!iv.is_empty ())
    iv.translate (offset);
  return iv;
}

Interval
Grob::maybe_pure_extent (Grob *refp, Axis a, bool pure, int start, int end)
{
  if (pure && a != Y_AXIS)
    programming_error ("tried to get pure width");
  return (pure && a == Y_AXIS) ? pure_height (refp, start, end) : extent (refp, a);
}

Interval_t<int>
Grob::spanned_rank_interval () const
{
  return Interval_t<int> (-1, 0);
}

/****************************************************************
  REFPOINTS
****************************************************************/

/* Find the group-element which has both #this# and #s#  */
Grob *
Grob::common_refpoint (Grob const *s, Axis a) const
{
  /* I don't like the quadratic aspect of this code, but I see no
     other way.  The largest chain of parents might be 10 high or so,
     so it shouldn't be a real issue.  */
  for (Grob const *c = this; c; c = c->dim_cache_[a].parent_)
    for (Grob const *d = s; d; d = d->dim_cache_[a].parent_)
      if (d == c)
	return (Grob *) d;

  return 0;
}

void
Grob::set_parent (Grob *g, Axis a)
{
  dim_cache_[a].parent_ = g;
}

Grob *
Grob::get_parent (Axis a) const
{
  return dim_cache_[a].parent_;
}


void
Grob::fixup_refpoint ()
{
  for (int a = X_AXIS; a < NO_AXES; a++)
    {
      Axis ax = (Axis)a;
      Grob *parent = get_parent (ax);

      if (!parent)
	continue;

      if (parent->get_system () != get_system () && get_system ())
	{
	  Grob *newparent = parent->find_broken_piece (get_system ());
	  set_parent (newparent, ax);
	}

      if (Item *i = dynamic_cast<Item *> (this))
	{
	  Item *parenti = dynamic_cast<Item *> (parent);

	  if (parenti && i)
	    {
	      Direction my_dir = i->break_status_dir ();
	      if (my_dir != parenti->break_status_dir ())
		{
		  Item *newparent = parenti->find_prebroken_piece (my_dir);
		  set_parent (newparent, ax);
		}
	    }
	}
    }
}


/****************************************************************
  MESSAGES
****************************************************************/
void
Grob::warning (string s) const
{
  if (get_program_option ("warning-as-error"))
    error (s);

  SCM cause = self_scm ();
  while (Grob *g = unsmob_grob (cause))
    cause = g->get_property ("cause");

  /* ES TODO: cause can't be Music*/
  if (Music *m = unsmob_music (cause))
    m->origin ()->warning (s);
  else if (Stream_event *ev = unsmob_stream_event (cause))
    ev->origin ()->warning (s);
  else
    ::warning (s);
}


string
Grob::name () const
{
  SCM meta = get_property ("meta");
  SCM nm = scm_assq (ly_symbol2scm ("name"), meta);
  nm = (scm_is_pair (nm)) ? scm_cdr (nm) : SCM_EOL;
  return scm_is_symbol (nm) ? ly_symbol2string (nm) : this->class_name ();
}

void
Grob::programming_error (string s) const
{
  if (get_program_option ("warning-as-error"))
    error (s);

  SCM cause = self_scm ();
  while (Grob *g = unsmob_grob (cause))
    cause = g->get_property ("cause");

  s = _f ("programming error: %s", s);

  /* ES TODO: cause can't be Music*/
  if (Music *m = unsmob_music (cause))
    m->origin ()->message (s);
  else if (Stream_event *ev = unsmob_stream_event (cause))
    ev->origin ()->message (s);
  else
    ::message (s);
}


ADD_INTERFACE (Grob,
	       "A grob represents a piece of music notation.\n"
	       "\n"
	       "All grobs have an X and Y@tie{}position on the page.  These"
	       " X and Y@tie{}positions are stored in a relative format, thus"
	       " they can easily be combined by stacking them, hanging one"
	       " grob to the side of another, or coupling them into grouping"
	       " objects.\n"
	       "\n"
	       "Each grob has a reference point (a.k.a.@: parent): The"
	       " position of a grob is stored relative to that reference"
	       " point.  For example, the X@tie{}reference point of a staccato"
	       " dot usually is the note head that it applies to.  When the"
	       " note head is moved, the staccato dot moves along"
	       " automatically.\n"
	       "\n"
	       "A grob is often associated with a symbol, but some grobs do"
	       " not print any symbols.  They take care of grouping objects."
	       " For example, there is a separate grob that stacks staves"
	       " vertically.  The @ref{NoteCollision} object is also an"
	       " abstract grob: It only moves around chords, but doesn't print"
	       " anything.\n"
	       "\n"
	       "Grobs have properties (Scheme variables) that can be read and"
	       " set.  Two types of them exist: immutable and mutable."
	       "  Immutable variables define the default style and behavior."
	       "  They are shared between many objects.  They can be changed"
	       " using @code{\\override} and @code{\\revert}.  Mutable"
	       " properties are variables that are specific to one grob."
	       "  Typically, lists of other objects, or results from"
	       " computations are stored in mutable properties.  In"
	       " particular, every call to @code{ly:grob-set-property!}"
	       " (or its C++ equivalent) sets a mutable property.\n"
	       "\n"
	       "The properties @code{after-line-breaking} and"
	       " @code{before-line-breaking} are dummies that are not"
	       " user-serviceable.",

	       /* properties */
	       "X-extent "
	       "X-offset "
	       "Y-extent "
	       "Y-offset "
	       "after-line-breaking "
	       "avoid-slur "
	       "axis-group-parent-X "
	       "axis-group-parent-Y "
	       "before-line-breaking "
	       "cause "
	       "color "
	       "cross-staff "
	       "extra-X-extent "
	       "extra-Y-extent "
	       "extra-offset "
	       "interfaces "
	       "layer "
	       "meta "
	       "minimum-X-extent "
	       "minimum-Y-extent "
	       "outside-staff-horizontal-padding "
	       "outside-staff-padding "
	       "outside-staff-priority "
	       "pure-Y-offset-in-progress "
	       "rotation "
	       "springs-and-rods "
	       "staff-symbol "
	       "stencil "
	       "transparent "
	       "whiteout "
	       );

/****************************************************************
  CALLBACKS
****************************************************************/

static SCM
grob_stencil_extent (Grob *me, Axis a)
{
  Stencil *m = me->get_stencil ();
  Interval e;
  if (m)
    e = m->extent (a);
  return ly_interval2scm (e);
}


MAKE_SCHEME_CALLBACK (Grob, stencil_height, 1);
SCM
Grob::stencil_height (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  return grob_stencil_extent (me, Y_AXIS);
}

MAKE_SCHEME_CALLBACK (Grob, y_parent_positioning, 1);
SCM
Grob::y_parent_positioning (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Grob *par = me->get_parent (Y_AXIS);
  if (par)
    (void) par->get_property ("positioning-done");

  return scm_from_double (0.0);
}


MAKE_SCHEME_CALLBACK (Grob, x_parent_positioning, 1);
SCM
Grob::x_parent_positioning (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  
  Grob *par = me->get_parent (X_AXIS);
  if (par)
    (void) par->get_property ("positioning-done");

  return scm_from_double (0.0);
}

MAKE_SCHEME_CALLBACK (Grob, stencil_width, 1);
SCM
Grob::stencil_width (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  return grob_stencil_extent (me, X_AXIS);
}


Grob *
common_refpoint_of_list (SCM elist, Grob *common, Axis a)
{
  for (; scm_is_pair (elist); elist = scm_cdr (elist))
    if (Grob *s = unsmob_grob (scm_car (elist)))
      {
	if (common)
	  common = common->common_refpoint (s, a);
	else
	  common = s;
      }

  return common;
}

Grob *
common_refpoint_of_array (vector<Grob*> const &arr, Grob *common, Axis a)
{
  for (vsize i = 0; i < arr.size (); i++)
    if (common)
      common = common->common_refpoint (arr[i], a);
    else
      common = arr[i];

  return common;
}

Interval
robust_relative_extent (Grob *me, Grob *refpoint, Axis a)
{
  Interval ext = me->extent (refpoint, a);
  if (ext.is_empty ())
    ext.add_point (me->relative_coordinate (refpoint, a));

  return ext;
}

// Checks whether there is a vertical alignment in the chain of
// parents between this and commony.
bool
Grob::check_cross_staff (Grob *commony)
{
  if (Align_interface::has_interface (commony))
    return true;

  for (Grob *g = this; g && g != commony; g = g->get_parent (Y_AXIS))
    if (Align_interface::has_interface (g))
      return true;

  return false;
}

