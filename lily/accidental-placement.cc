/*   
accidental-placement.cc --  implement Accidental_placement

source file of the GNU LilyPond music typesetter

(c) 2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */


#include "item.hh"
#include "skyline.hh"
#include "music.hh"
#include "pitch.hh"
#include "warn.hh"
#include "accidental-placement.hh"


MAKE_SCHEME_CALLBACK(Accidental_placement,alignment_callback, 2);
SCM
Accidental_placement::alignment_callback(SCM s, SCM )
{
  Grob * me =unsmob_grob (s);

  Grob * par = me->get_parent (X_AXIS);
  if (!to_boolean (par->get_grob_property ("done")))
  {
    par->set_grob_property ("done", SCM_BOOL_T);
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
  int notename_;
};

int ape_compare (Accidental_placement_entry *const &a,
		 Accidental_placement_entry *const &b)
{
  return sign (a->vertical_extent_.length () - b->vertical_extent_.length());
}

void
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
    for (int j = apes[i]->grobs_.size(); j--;)
      {
	Grob * a = apes[i]->grobs_[j];

	if (commony)
	  commony =commony->common_refpoint (a, Y_AXIS);
	else
	  commony =a;
      }

  for (int i= apes.size (); i--;)
    {
      Interval y ;
      
      for (int j = apes[i]->grobs_.size(); j--;)
	{
	  Grob * a = apes[i]->grobs_[j];
	  Box b;
	  b[X_AXIS] = a->extent (me, X_AXIS);
	  b[Y_AXIS] = a->extent (commony, Y_AXIS);

	  y.unite (b[Y_AXIS]);
	  apes[i]->extents_.push (b);
	}

      apes[i]->vertical_extent_ = y;
    }
  
  for (int i= apes.size (); i--;)
    {
      Accidental_placement_entry * ape = apes[i];
      ape->left_skyline_ = extents_to_skyline (apes[i]->extents_, Y_AXIS, LEFT);
      ape->right_skyline_ = extents_to_skyline (apes[i]->extents_, Y_AXIS, RIGHT);
    }

  apes.sort (&ape_compare);  

  for (int i= apes.size ()-1; i-- > 0;)
    {
      Accidental_placement_entry *ape = apes[i];
      Real here = 0.0;

      Real d = skyline_meshing_distance (ape->right_skyline_, apes[i+1]->left_skyline_);

      here += d;
      for (int j  = ape->grobs_.size(); j--;)
	{
	  ape->grobs_[j]->translate_axis (here, X_AXIS);
	}
    }

  for (int i = apes.size(); i--;)
    delete apes[i];
}

ADD_INTERFACE(Accidental_placement,
	      "accidental-placement-interface",
	      "Take care of complex accidental collisions.",
	      "accidentals done")
