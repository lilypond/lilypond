/*   
accidental-placement.cc --  implement Accidental_placement

source file of the GNU LilyPond music typesetter

(c) 2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include <math.h>
#include <libc-extension.hh>
#include "item.hh"
#include "skyline.hh"
#include "music.hh"
#include "pitch.hh"
#include "warn.hh"
#include "accidental-placement.hh"


MAKE_SCHEME_CALLBACK(Accidental_placement,extent_callback, 2);
SCM
Accidental_placement::extent_callback(SCM s, SCM axis)
{
  Grob * me =unsmob_grob (s);
  Axis a = Axis (gh_scm2int (axis));

  assert (a == X_AXIS);

  SCM w = position_accidentals (me);
  return w;
}

MAKE_SCHEME_CALLBACK(Accidental_placement,alignment_callback, 2);
SCM
Accidental_placement::alignment_callback(SCM s, SCM )
{
  Grob * me =unsmob_grob (s);

  Grob * par = me->get_parent (X_AXIS);
  if (!to_boolean (par->get_grob_property ("alignment-done")))
    {
      par->set_grob_property ("alignment-done", SCM_BOOL_T);
      position_accidentals (par);
    }

  return gh_int2scm (0);
}



void
Accidental_placement::add_accidental (Grob* me, Grob* a)
{
  a->set_parent (me, X_AXIS);
  a->add_offset_callback (alignment_callback_proc, X_AXIS);
  SCM cause = a->get_parent (Y_AXIS)->get_grob_property("cause");

  Music *mcause =unsmob_music (cause); 
  if (!mcause)
    {
      programming_error ("Note head has no music cause!");
      return; 
    }

  Pitch *p= unsmob_pitch (mcause->get_mus_property ("pitch"));

  int n = p->notename_i_;

  SCM accs = me->get_grob_property ("accidentals");
  SCM key = gh_int2scm (n);
  SCM entry = scm_assq (key, accs);
  if (entry == SCM_BOOL_F)
    {
      entry = SCM_EOL;
    }
  else
    entry = gh_cdr (entry);

  entry = gh_cons (a->self_scm (), entry);

  accs = scm_assq_set_x (accs,  key, entry);

  me->set_grob_property ("accidentals", accs);
}

struct Accidental_placement_entry
{
  Array<Skyline_entry> left_skyline_;
  Array<Skyline_entry> right_skyline_;
  Interval vertical_extent_;
  Array<Box> extents_;
  Link_array<Grob> grobs_;
  Real offset_; 
  int notename_;
  Accidental_placement_entry()
  {
    offset_ =0.0;
    notename_ = -1;
  }
};


int ape_compare (Accidental_placement_entry *const &a,
		 Accidental_placement_entry *const &b)
{
  return sign (a->vertical_extent_.length () - b->vertical_extent_.length());
}

/*
  Return: width as SCM interval.


  This routine computes placements of accidentals. During
  add_accidental(), accidentals are already grouped by note, so that
  octaves are placed above each other; they form columns. Then the
  columns are sorted: the biggest columns go closest to the note.
  Then the columns are spaced as closely as possible (using skyline
  spacing).
  
  
  TODO: more advanced placement. Typically, the accs should be placed
  to form a C shape, like this

  
           ##
        b b
       # #
        b
          b b

   The naturals should be left of the C as well; they should
   be separate accs.

   TODO: Try to combine Apes before sorting them: this will allow a
   better placement.

   Note that this placement problem is almost certainly NP hard, so we
   just use a simple strategy, not an optimal choice.
*/

SCM
Accidental_placement::position_accidentals (Grob * me)
{
  SCM accs = me->get_grob_property ("accidentals");

  Link_array<Accidental_placement_entry> apes;
  for (SCM s = accs; gh_pair_p (s); s =gh_cdr (s))
    {
      Accidental_placement_entry *ape = new Accidental_placement_entry;
      ape->notename_ = gh_scm2int (gh_caar (s));
      
      for (SCM t = gh_cdar (s); gh_pair_p (t); t =gh_cdr (t))
	ape->grobs_.push (unsmob_grob (gh_car (t)));

      apes.push (ape);
    }


  Grob *commony =0 ;
  for (int i= apes.size (); i--;)
    commony = common_refpoint_of_array  (apes[i]->grobs_, commony, Y_AXIS);

  Link_array<Grob> heads;
  for (int i= apes.size (); i--;)
    {
      Accidental_placement_entry * ape = apes[i];
      ape->left_skyline_ = empty_skyline ( LEFT);
      ape->right_skyline_ = empty_skyline ( RIGHT);
   
      for (int j = apes[i]->grobs_.size(); j--;)
	{
	  Grob * a = apes[i]->grobs_[j];
	  Box b;
	  b[X_AXIS] = a->extent (me, X_AXIS);
	  b[Y_AXIS] = a->extent (commony, Y_AXIS);

	  Grob *head = a->get_parent (Y_AXIS);
	  heads.push (head);
	  commony = commony->common_refpoint (head, Y_AXIS);
	  
	  ape->extents_.push (b);

	  
	  /*
	    TODO: replace the extents of a flat by combination of two
	    bboxes, so that we use the shape of the flat better.
	  */
	  insert_extent_into_skyline (&ape->left_skyline_, b, Y_AXIS, LEFT);
	  insert_extent_into_skyline (&ape->right_skyline_ , b,Y_AXIS, RIGHT);
	}
    }

  for (int i = apes.size(); i--;)
    {
      Interval y ;
      
      for (int j = apes[i]->extents_.size(); j--;)
	{
	  y.unite (apes[i]->extents_[j][Y_AXIS]);
	}
      apes[i]->vertical_extent_ = y;
    }
  
  apes.sort (&ape_compare);  

  Accidental_placement_entry * head_ape = new Accidental_placement_entry;
  Grob *commonx = common_refpoint_of_array (heads, me, X_AXIS);  
  Array<Skyline_entry> head_skyline (empty_skyline (LEFT));
  Array<Box> head_extents;
  for (int i = heads.size(); i--;)
    {
      Box b(heads[i]->extent (commonx, X_AXIS),
	    heads[i]->extent (commony, Y_AXIS));

      insert_extent_into_skyline (&head_skyline, b , Y_AXIS, LEFT);
    }

  head_ape-> left_skyline_ = head_skyline;
  head_ape->offset_ = 0.0;

  SCM rs = me->get_grob_property ("right-padding");
  if (gh_number_p (rs))
    head_ape->offset_ -= gh_scm2double (rs);

  Real padding = 0.1;
  apes.push (head_ape);
  for (int i= apes.size () -1 ; i-- > 0;)
    {
      Accidental_placement_entry *ape = apes[i];
      Real d = 0.0;
      /*
	confusing naming: left_skyline is a skyline pointing to the
	left. It is on the right of the curent entry.
       */

      int j = i+1;
      do {
	Array<Skyline_entry> const *right_sky =
	  (j < apes.size())
	  ? &apes[j]->left_skyline_
	  : &head_skyline;

	d = - skyline_meshing_distance (ape->right_skyline_,
					     *right_sky);

	if (!isinf(d)
	    || j + 1 == apes.size())
	  break;
	
	j = j+ 1;
      } while (1);

      if (isinf(d))
	d = 0.0;
      
      d -= padding;
      ape->offset_ += apes[j]->offset_ + d;
    }

  apes.pop();
  for (int i = apes.size(); i--;)
    {
      Accidental_placement_entry* ape = apes[i];
      for (int j  = ape->grobs_.size(); j--;)
	{
	  ape->grobs_[j]->translate_axis (ape->offset_, X_AXIS);
	}
    }

  
  Interval left_extent, right_extent;
  Accidental_placement_entry *ape = apes[0];

  for (int i = ape->extents_.size(); i--;)
    left_extent.unite (ape->offset_ +  ape->extents_[i][X_AXIS]);

  ape = apes.top();
  for (int i = ape->extents_.size(); i--;)
    right_extent.unite (ape->offset_  + ape->extents_[i][X_AXIS]);

  SCM ls = me->get_grob_property ("left-padding");
  if (gh_number_p (rs))
    left_extent[LEFT] -= gh_scm2double (ls);

  
  Interval width(left_extent[LEFT], right_extent[RIGHT]);

  SCM scm_width = ly_interval2scm (width);
  me->set_extent (scm_width, X_AXIS);
  
  for (int i = apes.size(); i--;)
    delete apes[i];

  return scm_width;
}

ADD_INTERFACE(Accidental_placement,
	      "accidental-placement-interface",
	      "Take care of complex accidental collisions.",
	      "left-padding right-padding accidentals alignment-done")
