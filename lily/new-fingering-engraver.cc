/*   
  fingering-engraver.cc --  implement New_fingering_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "warn.hh"
#include "engraver.hh"
#include "side-position-interface.hh"
#include "item.hh"
#include "event.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "self-alignment-interface.hh"
#include "script.hh"

struct Finger_tuple
{
  Grob *head_;
  Grob *fingering_;
  Music *note_event_;
  Music *finger_event_;

  int position_;
  
  static int compare (Finger_tuple const & c1, Finger_tuple const & c2)
  {
    return c1.position_-  c2.position_;
  }
	       
};

class New_fingering_engraver : public Engraver
{
  Array<Finger_tuple> fingerings_;
public:
  TRANSLATOR_DECLARATIONS(New_fingering_engraver);
protected:
  virtual void stop_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);
  void add_fingering (Grob*, Music*,Music*);
  void position_scripts();
};

void
New_fingering_engraver::acknowledge_grob (Grob_info inf)
{
  if (Rhythmic_head::has_interface (inf.grob_))
    {
      Music * note_ev =inf.music_cause ();

      SCM arts = note_ev->get_mus_property ("articulations");

      for (SCM s = arts; gh_pair_p (s); s = gh_cdr  (s))
	{
	  Music * m = unsmob_music (gh_car (s));

	  if (!m)
	    continue;
	  

	  if (m->is_mus_type ("fingering-event"))
	    {
	      add_fingering (inf.grob_ , m, note_ev);
	    }
	  else if (m->is_mus_type ("script-event"))
	    {

	    }
	}
    }
}

void
New_fingering_engraver::add_fingering (Grob * head,
				       Music * event,
				       Music *hevent)
{
  Finger_tuple ft;

  ft.fingering_ = new Item (get_property ("Fingering"));
  announce_grob (ft.fingering_, event->self_scm());

   Side_position_interface::add_support (ft.fingering_, head);

  int d = gh_scm2int ( event->get_mus_property ("digit"));
  
  /*
    TODO:
    
    Should add support for thumb.  It's a little involved, since
    the thumb lives in a different font. Maybe it should be moved?
    
   */

  if (d > 5)
    {
      /*
	music for the softenon children? 
       */
      event->origin()->warning (_("music for the martians."));
    }
  SCM sstr = scm_number_to_string (gh_int2scm (d), gh_int2scm (10)) ;
  ft.fingering_->set_grob_property ("text", sstr);
       
  ft.finger_event_ = event;
  ft.note_event_ = hevent;
  ft.head_ = head;

  fingerings_.push (ft);
}

void
New_fingering_engraver::position_scripts ()
{

  /*
    This is not extremely elegant, but we have to do a little
    formatting here, because the parent/child relations should be
    known before we move on to the next time step.

    A more sophisticated approach would be to set both X and Y parents
    to the note head, and write a more flexible function for
    positioning the fingerings, setting both X and Y coordinates.
  */

  
  for (int i = 0; i < fingerings_.size(); i++)
    {      
      fingerings_[i].position_ = gh_scm2int (fingerings_[i].head_ -> get_grob_property( "staff-position"));
    }
  
  Array<Finger_tuple> up, down, horiz;
  for (int i = fingerings_.size(); i--;)
    {
      SCM d = fingerings_[i].finger_event_->get_mus_property ("direction");
      if (to_dir (d))
	{
	  if (to_dir (d) == UP)
	    {
	      up.push (fingerings_[i]);
	    }
	  else
	    down.push (fingerings_[i]);
	  fingerings_.del (i);
	}
    }
  
  fingerings_.sort (&Finger_tuple::compare);
  if (to_boolean (get_property ("fingersHorizontal")))
    {
      up.push (fingerings_.pop());
      down.push (fingerings_[0]);
      fingerings_.del(0);

      horiz = fingerings_;
    }
  else
    {
      int center = fingerings_.size() / 2;
      down.concat (fingerings_.slice (0,center));
      up.concat (fingerings_.slice (center, fingerings_.size()));
    }

  for (int i = 0; i < horiz.size(); i++)
    {
      Finger_tuple ft = horiz[i];
      Grob* f = ft.fingering_;
      f->set_parent (ft.head_, X_AXIS);
      f->set_parent (ft.head_, Y_AXIS);
      f->add_offset_callback (Self_alignment_interface::centered_on_parent_proc, Y_AXIS);
      f->add_offset_callback (Self_alignment_interface::aligned_on_self_proc, Y_AXIS);
      f->add_offset_callback (Side_position_interface::aligned_side_proc, X_AXIS);
      f->set_grob_property( "direction", gh_int2scm (RIGHT));
      typeset_grob (f);
    }

  int finger_prio = 200;
  for (int i = 0; i < up.size(); i++)
    {
      Finger_tuple ft = up[i];
      Grob* f = ft.fingering_;
      f->set_parent (ft.head_, X_AXIS);
      f->set_grob_property ("script-priority",
			    gh_int2scm (finger_prio + i));
      f->add_offset_callback (Side_position_interface::aligned_side_proc, Y_AXIS);
      f->add_offset_callback (Self_alignment_interface::centered_on_parent_proc, X_AXIS);
      f->add_offset_callback (Self_alignment_interface::aligned_on_self_proc, X_AXIS);
      
      f->set_grob_property ("direction", gh_int2scm (UP));

      Side_position_interface::add_staff_support (f);
      typeset_grob (f);
    }
  
  for (int i = 0; i < down.size(); i++)
    {
      Finger_tuple ft = down[i];
      Grob* f =       ft.fingering_;
      f->set_parent (ft.head_, X_AXIS);
      f->set_grob_property ("script-priority",
			    gh_int2scm (finger_prio + down.size() - i));

      f->add_offset_callback (Self_alignment_interface::centered_on_parent_proc, X_AXIS);
      f->add_offset_callback (Self_alignment_interface::aligned_on_self_proc, X_AXIS);
      f->add_offset_callback (Side_position_interface::aligned_side_proc, Y_AXIS);
      f->set_grob_property ("direction", gh_int2scm (DOWN));
      Side_position_interface::add_staff_support (f);
      typeset_grob (f);
    }
}

void
New_fingering_engraver::stop_translation_timestep ()
{
  if (!fingerings_.size ())
    return;

  position_scripts();
  fingerings_.clear ();
}


New_fingering_engraver::New_fingering_engraver()
{
  
}

ENTER_DESCRIPTION(New_fingering_engraver,
/* descr */       "Create fingering-scripts for notes in a New Chord.",
/* creats*/       "Fingering",
/* accepts */     "text-script-event",
/* acks  */      "rhythmic-head-interface stem-interface",
/* reads */       "fingersHorizontal",
/* write */       "");
