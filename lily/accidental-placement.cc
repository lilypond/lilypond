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
#include "note-column.hh"
#include "group-interface.hh"
#include "note-collision.hh"
#include "accidental-interface.hh"

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

  return scm_int2num (0);
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

  int n = p->notename_;

  SCM accs = me->get_grob_property ("accidental-grobs");
  SCM key = scm_int2num (n);
  SCM entry = scm_assq (key, accs);
  if (entry == SCM_BOOL_F)
    {
      entry = SCM_EOL;
    }
  else
    entry = gh_cdr (entry);

  entry = gh_cons (a->self_scm (), entry);

  accs = scm_assq_set_x (accs,  key, entry);

  me->set_grob_property ("accidental-grobs", accs);
}

/*
  Split into break reminders.
 */
void
Accidental_placement::split_accidentals (Grob * accs,
					 Link_array<Grob> *break_reminder,
					 Link_array<Grob> *real_acc)
{
  for (SCM acs =accs->get_grob_property ("accidental-grobs"); gh_pair_p (acs);
       acs =gh_cdr (acs))
    for (SCM s = gh_cdar (acs); gh_pair_p (s); s = gh_cdr (s))
      {
	Grob *a = unsmob_grob (gh_car (s));

	if (unsmob_grob (a->get_grob_property ("tie")))
	  break_reminder->push (a);
	else
	  real_acc->push (a);
      }
}

/*
  Accidentals are special, because they appear and disappear after
  ties at will.
*/
Interval
Accidental_placement::get_relevant_accidental_extent (Grob *me,
						      Item *item_col,
						      Grob *left_object)
{
  Link_array<Grob> br, ra;
  Link_array<Grob> *which = 0;

  Accidental_placement::split_accidentals (me, &br, &ra);
  br.concat (ra);
  
  if (dynamic_cast<Item*>(left_object)->break_status_dir () == RIGHT)
    which = & br;
  else
    which = & ra;
  
  Interval extent;
  for (int i = 0; i < which->size(); i++)
    {
      extent.unite (which->elem(i)->extent (item_col, X_AXIS));
    }

  if (!extent.empty_b())
    {
      Real p = gh_scm2double (me->get_grob_property ("left-padding"));
      extent[LEFT] -= p;
    }
  
  return extent;
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

static Interval all_accidental_vertical_extent;
Real ape_priority (Accidental_placement_entry const * a)
{
  return a->vertical_extent_[UP];
}

  

int ape_compare (Accidental_placement_entry *const &a,
		 Accidental_placement_entry *const &b)
{
  return sign (ape_priority (a) - ape_priority(b));
}

int ape_rcompare (Accidental_placement_entry *const &a,
		 Accidental_placement_entry *const &b)
{
  return -sign (ape_priority (a) - ape_priority(b));
}


/*

TODO: should favor

  b
 b

placement
 
*/
void
stagger_apes (Link_array<Accidental_placement_entry> *apes)
{
  Link_array<Accidental_placement_entry> asc = *apes;


  asc.sort (&ape_compare);

  apes->clear();

  int i =0;
  int parity = 1;
  while (i < asc.size())
    {
      Accidental_placement_entry * a = 0;      
      if (parity)
	a = asc.pop();
      else
	a = asc[i++];

      apes->push (a);
      parity = !parity;
    }

  apes->reverse();
}

  

/*

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

   Note that this placement problem looks NP hard, so we just use a
   simple strategy, not an optimal choice.

*/

SCM
Accidental_placement::position_accidentals (Grob * me)
{
  if (!me->live ())
    return SCM_UNSPECIFIED;
  
  SCM accs = me->get_grob_property ("accidental-grobs");

  /*
    TODO: there is a bug in this code. If two accs are on the same
    Y-position, they share an Ape, and will be pritned in overstrike.
   */
  Link_array<Accidental_placement_entry> apes;
  for (SCM s = accs; gh_pair_p (s); s =gh_cdr (s))
    {
      Accidental_placement_entry *ape = new Accidental_placement_entry;
      ape->notename_ = gh_scm2int (gh_caar (s));
      
      for (SCM t = gh_cdar (s); gh_pair_p (t); t =gh_cdr (t))
	ape->grobs_.push (unsmob_grob (gh_car (t)));

      apes.push (ape);
    }


  Grob *common[] = {me, 0};

  /*
    First we must extract *all* pointers. We can only determine
    extents if we're sure that we've found the right common refpoint
   */
  Link_array<Grob> note_cols, heads;
  for (int i= apes.size (); i--;)
    { 
      Accidental_placement_entry * ape = apes[i];
      for (int j = ape->grobs_.size(); j--;)
	{
	  Grob * a = ape->grobs_[j];

	  if (common[Y_AXIS])
	    common[Y_AXIS] = common[Y_AXIS]->common_refpoint (a, Y_AXIS);
	  else
	    common[Y_AXIS] = a;
	  
	  Grob *head = a->get_parent (Y_AXIS);

	  Grob * col = head->get_parent (X_AXIS);
	  if (Note_column::has_interface (col))
	    note_cols.push (col);
	  else
	    heads.push (head);
	}
    }

  /*
    This is a little kludgy: to get all notes, we look if there are
    collisions as well.
   */
  for (int i = note_cols.size() ; i--;)
    {
      Grob *c = note_cols[i]->get_parent (X_AXIS);
      if (Note_collision_interface::has_interface (c))
	{
	  Link_array<Grob> gs =
	    Pointer_group_interface__extract_grobs (c, (Grob*)0, "elements");
      
	  note_cols.concat (gs);
	}
    }
  
  for (int i = note_cols.size() ; i--;)
    {
      heads.concat (Pointer_group_interface__extract_grobs (note_cols[i],
							    (Grob*)0,
							    "note-heads"));
      
    }
  heads.default_sort();
  heads.uniq();
  common[Y_AXIS] = common_refpoint_of_array (heads, common[Y_AXIS], Y_AXIS);

  
  for (int i= apes.size (); i--;)
    {
      Accidental_placement_entry * ape = apes[i];
      ape->left_skyline_ = empty_skyline (LEFT);
      ape->right_skyline_ = empty_skyline (RIGHT);
   
      for (int j = apes[i]->grobs_.size(); j--;)
	{
	  Grob * a = apes[i]->grobs_[j];

	  Array<Box> boxes = Accidental_interface::accurate_boxes (a, common);
	  
	  ape->extents_.concat (boxes);
	  for (int j  = boxes.size(); j--;)
	    {
	      insert_extent_into_skyline (&ape->left_skyline_, boxes[j], Y_AXIS, LEFT);
	      insert_extent_into_skyline (&ape->right_skyline_ , boxes[j], Y_AXIS, RIGHT);
	    }
	}
    }


  Interval total;
  for (int i = apes.size(); i--;)
    {
      Interval y ;
      
      for (int j = apes[i]->extents_.size(); j--;)
	{
	  y.unite (apes[i]->extents_[j][Y_AXIS]);
	}
      apes[i]->vertical_extent_ = y;
      total.unite (y);
    }
  all_accidental_vertical_extent = total;
  stagger_apes (&apes);

  Accidental_placement_entry * head_ape = new Accidental_placement_entry;
  common[X_AXIS] = common_refpoint_of_array (heads, common[X_AXIS], X_AXIS);  
  Array<Skyline_entry> head_skyline (empty_skyline (LEFT));
  Array<Box> head_extents;
  for (int i = heads.size(); i--;)
    {
      Box b(heads[i]->extent (common[X_AXIS] , X_AXIS),
	    heads[i]->extent (common[Y_AXIS], Y_AXIS));

      insert_extent_into_skyline (&head_skyline, b , Y_AXIS, LEFT);
    }

  head_ape-> left_skyline_ = head_skyline;
  head_ape->offset_ = 0.0;

  SCM rs = me->get_grob_property ("right-padding");
  if (gh_number_p (rs))
    head_ape->offset_ -= gh_scm2double (rs);

  
  Real padding = 0.2;
  SCM spad = me->get_grob_property ("padding");
  if (gh_number_p (spad))
    padding = gh_scm2double (spad);


  /*
    TODO:

    There is a bug in this code: the left_skylines should be
    accumulated, otherwise the b will collide with bb in

      bb
    b 
      n 
    
   */
  apes.push (head_ape);
  for (int i= apes.size () -1 ; i-- > 0;)
    {
      Accidental_placement_entry *ape = apes[i];
      Real d = 0.0;
      int j = i+1;
      do {
	d = - skyline_meshing_distance (ape->right_skyline_,
					apes[j]->left_skyline_);

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

  return SCM_UNSPECIFIED;
}

ADD_INTERFACE(Accidental_placement,
	      "accidental-placement-interface",
	      "Take care of complex accidental collisions.",
	      "left-padding padding right-padding accidental-grobs alignment-done")
