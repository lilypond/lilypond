/*   
  note-head-line-engraver.cc -- implement Note_head_line_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2004 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "engraver.hh"
#include "group-interface.hh"
#include "item.hh"
#include "event.hh"
#include "spanner.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "context.hh"


/**
   Create line-spanner grobs for lines that connect note heads.

   TODO: have the line commit suicide if the notes are connected with
   either slur or beam.

*/
class Note_head_line_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Note_head_line_engraver);

protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void process_acknowledged_grobs ();
  virtual void stop_translation_timestep ();

private:
  Spanner* line_; 
  Context* last_staff_;
  bool follow_;
  Grob* head_;
  Grob* last_head_;
};

Note_head_line_engraver::Note_head_line_engraver ()
{
  line_ = 0;
  follow_ = false;
  head_ = 0;
  last_head_ = 0;
  last_staff_ = 0;
}

void
Note_head_line_engraver::acknowledge_grob (Grob_info info)
{
  if (Rhythmic_head::has_interface (info.grob_))
    {
      head_ = info.grob_;
      if (to_boolean (get_property ("followVoice")))
	{
	  Context * tr = context ();
	  while (tr && !tr->is_alias (ly_symbol2scm ( "Staff")))
	    tr = tr->get_parent_context () ;

	  if (tr
	      && tr->is_alias (ly_symbol2scm ("Staff")) && tr != last_staff_)
	    {
	      if (last_head_)
		follow_ = true;
	      last_staff_ = tr;
	    }
	}
    }
}


void
Note_head_line_engraver::process_acknowledged_grobs ()
{
  if (!line_ && follow_ && last_head_ && head_)
    {
      /* TODO: Don't follow if there's a beam.

	 We can't do beam-stuff here, since beam doesn't exist yet.
	 Should probably store follow_ in line_, and suicide at some
	 later point */
      if (follow_)
	line_ = make_spanner ("VoiceFollower", head_->self_scm ());
	  
      line_->set_bound (LEFT, last_head_);
      line_->set_bound (RIGHT, head_);
      

      follow_ = false;
    }
}

void
Note_head_line_engraver::stop_translation_timestep ()
{
  if (line_)
    {
      typeset_grob (line_);
      line_ = 0;
    }
  if (head_)
    last_head_ = head_;
  head_ = 0;
}




ENTER_DESCRIPTION (Note_head_line_engraver,
/* descr */       "Engrave a line between two note heads, for example a glissando.  If "
" followVoice is set, staff switches also generate a line.",
/* creats*/       "Glissando VoiceFollower",
/* accepts */     "glissando-event",
/* acks  */       "rhythmic-head-interface",
/* reads */       "followVoice",
/* write */       "");
