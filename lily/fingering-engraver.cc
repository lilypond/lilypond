/*   
  fingering-engraver.cc --  implement Fingering_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "engraver.hh"
#include "side-position-interface.hh"
#include "item.hh"
#include "event.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "self-alignment-interface.hh"

class Fingering_engraver : public Engraver
{
  Link_array<Music> reqs_;
  Link_array<Item> fingerings_;

public:
  TRANSLATOR_DECLARATIONS(Fingering_engraver);
protected:
  virtual bool try_music (Music* m);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);

  void make_script (Direction, Music*,Axis, int);
};

bool
Fingering_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("fingering-event"))
    {
      reqs_.push (m);
      return true;
    }
  return false;
}

void
Fingering_engraver::acknowledge_grob (Grob_info inf)
{
  if (Stem::has_interface (inf.grob_))
    {
      for (int i=0; i < fingerings_.size (); i++)
	{
	  Side_position_interface::add_support (fingerings_[i],inf.grob_);
	}
    }
  else if (Rhythmic_head::has_interface (inf.grob_))
    {
      for (int i=0; i < fingerings_.size (); i++)
	{
	  Grob*t = fingerings_[i];
	  Side_position_interface::add_support (t,inf.grob_);
	  if (!t->get_parent (X_AXIS))
		t->set_parent (inf.grob_, X_AXIS);
	}
    }
}

void
Fingering_engraver::process_music ()
{
  for (int i= reqs_.size(); i--;)
    {
      SCM dir = reqs_[i]->get_mus_property ("direction");
      make_script (to_dir (dir), reqs_[i], Y_AXIS, i);
    }
}

void
Fingering_engraver::make_script (Direction d, Music *r,Axis a,  int i)
{
  Item *fingering = make_item ("Fingering");

  Axis other = other_axis (a);

  SCM pitch = r->get_mus_property ("pitch");
  if (unsmob_pitch (pitch))
    fingering->set_grob_property ("pitch", pitch);
  
  Side_position_interface::set_axis (fingering, a);
      
  fingering->add_offset_callback (Self_alignment_interface::aligned_on_self_proc, other);
  fingering->add_offset_callback (Self_alignment_interface::centered_on_parent_proc, other);

  // Hmm
  int priority = 200;
  SCM s = fingering->get_grob_property ("script-priority");
  if (gh_number_p (s))
    priority = gh_scm2int (s);
  
  /* See script-engraver.cc */
  priority += i;

  fingering->set_grob_property ("script-priority", gh_int2scm (priority));


  if (!is_direction (fingering->get_grob_property ("direction")))
    {
      if (d)
	fingering->set_grob_property ("direction", gh_int2scm (d));
      else
	fingering->set_grob_property ("direction",  gh_int2scm (RIGHT));
    }

  SCM dig =  r->get_mus_property ("digit");
  fingering->set_grob_property ("text", scm_number_to_string (dig, gh_int2scm (10)));

  announce_grob (fingering, r->self_scm());
  fingerings_.push (fingering);
}

void
Fingering_engraver::stop_translation_timestep ()
{
  if (!fingerings_.size ())
    return;
  
  for (int i=0; i < fingerings_.size (); i++)
    {
      Item *ti = fingerings_[i];
      Side_position_interface::add_staff_support (ti);
      typeset_grob (ti);
    }
  fingerings_.clear ();
}

void
Fingering_engraver::start_translation_timestep ()
{
  reqs_.clear ();
}

Fingering_engraver::Fingering_engraver()
{

}

ENTER_DESCRIPTION(Fingering_engraver,
/* descr */       "Create fingering-scripts",
/* creats*/       "Fingering",
/* accepts */     "fingering-event",
/* acks  */      "rhythmic-head-interface stem-interface",
/* reads */       "",
/* write */       "");
