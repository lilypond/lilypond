/*   
  note-head-line-engraver.cc -- implement Note_head_line_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "engraver.hh"
#include "group-interface.hh"
#include "item.hh"
#include "musical-request.hh"
#include "spanner.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "protected-scm.hh"

/**
   Create line-spanner grobs for glissandi (and possibly other) lines
   that connect note heads.
 */

class Note_head_line_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS (Translator);
  Note_head_line_engraver ();

protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void create_grobs ();
  virtual void stop_translation_timestep ();
  virtual bool try_music (Music *);

private:
  Spanner* line_; 
  Request* req_;
  Protected_scm heads_;
};

Note_head_line_engraver::Note_head_line_engraver ()
{
  line_ = 0;
  req_ = 0;
  heads_ = SCM_EOL;
}

bool
Note_head_line_engraver::try_music (Music* m)
{
  if (!req_)
    {
      if (Glissando_req *r = dynamic_cast<Glissando_req*> (m))
	{
	  req_ = r;
	  return true;
	}
    }
  return false;
}

void
Note_head_line_engraver::acknowledge_grob (Grob_info info)
{
  if (req_)
    {
      if (Rhythmic_head::has_interface (info.elem_l_))
	{
	  heads_ = gh_cons (info.elem_l_->self_scm (), heads_);
	}
    }
}

void
Note_head_line_engraver::create_grobs ()
{
  if (!line_ && scm_ilength (heads_) > 1)
    {
      /* type Glissando? */
      line_ = new Spanner (get_property ("NoteHeadLine"));
      line_->set_bound (LEFT, unsmob_grob (gh_car (heads_)));
      line_->set_bound (RIGHT, unsmob_grob (ly_last (heads_)));

      line_->set_parent (unsmob_grob (gh_car (heads_)), X_AXIS);
      line_->set_parent (unsmob_grob (gh_car (heads_)), Y_AXIS);

      line_->set_parent (unsmob_grob (ly_last (heads_)), X_AXIS);
      line_->set_parent (unsmob_grob (ly_last (heads_)), Y_AXIS);

      announce_grob (line_, req_);
      req_ = 0;
      heads_ = SCM_EOL;
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
}


ADD_THIS_TRANSLATOR (Note_head_line_engraver);

