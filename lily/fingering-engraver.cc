/*   
  fingering-engraver.cc --  implement Fingering_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "side-position-interface.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "self-alignment-interface.hh"

class Fingering_engraver : public Engraver
{
  Link_array<Music> reqs_;
  Link_array<Item> fingerings_;

public:
  TRANSLATOR_DECLARATIONS (Fingering_engraver);
protected:
  virtual bool try_music (Music* m);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);

private:
  void make_script (Direction, Music*, int);
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
      for (int i= 0; i < fingerings_.size (); i++)
	{
	  Side_position_interface::add_support (fingerings_[i], inf.grob_);
	}
    }
  else if (Rhythmic_head::has_interface (inf.grob_))
    {
      for (int i= 0; i < fingerings_.size (); i++)
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
  for (int i= reqs_.size (); i--;)
    {
      SCM dir = reqs_[i]->get_property ("direction");
      make_script (to_dir (dir), reqs_[i], i);
    }
}


void
Fingering_engraver::make_script (Direction d, Music *r, int i)
{
  Item *fingering = make_item ("Fingering", r->self_scm ());
  Axis a = Y_AXIS;
  Axis other = other_axis (a);

  /*
    Huh, what's this for? --hwn.

    junkme.
   */
  SCM pitch = r->get_property ("pitch");
  if (unsmob_pitch (pitch))
    fingering->set_property ("pitch", pitch);

  /*
    We can't fold these definitions into define-grobs since
    fingerings for chords need different settings. 
  */
  Side_position_interface::set_axis (fingering, a);
  fingering->add_offset_callback (Self_alignment_interface::aligned_on_self_proc, other);
  fingering->add_offset_callback (Self_alignment_interface::centered_on_parent_proc, other);

  // Hmm
  int priority = 200;
  SCM s = fingering->get_property ("script-priority");
  if (scm_is_number (s))
    priority = scm_to_int (s);
  
  /* See script-engraver.cc */
  priority += i;

  fingering->set_property ("script-priority", scm_int2num (priority));


  if (!is_direction (fingering->get_property ("direction")))
    {
      if (d)
	fingering->set_property ("direction", scm_int2num (d));
      else
	fingering->set_property ("direction",  scm_int2num (RIGHT));
    }

  SCM dig =  r->get_property ("digit");
  fingering->set_property ("text", scm_number_to_string (dig, scm_int2num (10)));

  fingerings_.push (fingering);
}

void
Fingering_engraver::stop_translation_timestep ()
{
  if (!fingerings_.size ())
    return;
  
  fingerings_.clear ();
}

void
Fingering_engraver::start_translation_timestep ()
{
  reqs_.clear ();
}

Fingering_engraver::Fingering_engraver ()
{

}

ENTER_DESCRIPTION (Fingering_engraver,
/* descr */       "Create fingering-scripts",
/* creats*/       "Fingering",
/* accepts */     "fingering-event",
/* acks  */      "rhythmic-head-interface stem-interface",
/* reads */       "",
/* write */       "");
