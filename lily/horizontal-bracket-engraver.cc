/*   
  horizontal-bracket-engraver.cc -- implement
    Horizontal_bracket_engraver

source file of the GNU LilyPond music typesetter

(c) 2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include "engraver.hh"
#include "side-position-interface.hh"
#include "note-column.hh"
#include "group-interface.hh"

class Horizontal_bracket_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS(Horizontal_bracket_engraver);
  Link_array<Spanner> bracket_stack_;
  Link_array<Music> events_;
  int pop_count_;
  int push_count_;

  virtual  bool try_music(Music*);
  virtual void start_translation_timestep ();
  virtual void stop_translation_timestep ();
  virtual  void process_music ();
  virtual  void acknowledge_grob (Grob_info);
};

ENTER_DESCRIPTION(Horizontal_bracket_engraver,
		  "Create horizontal brackets over notes for musical analysis purposes.",
		  "HorizontalBracket",
		  "note-grouping-event",
		  "note-column-interface",
		  "",
		  "");

Horizontal_bracket_engraver::Horizontal_bracket_engraver()
{
  pop_count_ = 0;
  push_count_ = 0;
}

void
Horizontal_bracket_engraver::start_translation_timestep()
{
  pop_count_ = 0;
  push_count_ = 0;
}

bool
Horizontal_bracket_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("note-grouping-event"))
    {
      Direction d = to_dir (m->get_mus_property ("span-direction"));
      
      if (d == STOP)
	{
	  pop_count_ ++;
	  if (pop_count_ > bracket_stack_.size())
	    m->origin()->warning (_("Don't have that many brackets."));
	}
      else
	{
	  push_count_++;
	  events_.push (m);
	}
      
      if (pop_count_ && push_count_)
	m->origin()->warning (_("Conflicting note group events."));

      return true;
    }
  return false;
}

void
Horizontal_bracket_engraver::acknowledge_grob (Grob_info gi)
{
  if (Note_column::has_interface (gi.grob_))
    {
      for (int i = 0;  i < bracket_stack_.size(); i++)
	{
	  Side_position_interface::add_support (bracket_stack_[i], gi.grob_);
	  Pointer_group_interface::add_grob (bracket_stack_[i],
					     ly_symbol2scm ("columns"), gi.grob_);
	  add_bound_item (bracket_stack_[i],
					     gi.grob_);
	}
    }
}

void
Horizontal_bracket_engraver::process_music ()
{
  for  (int k = 0; k < push_count_; k++)
    {
      Spanner * sp = new Spanner (get_property ("HorizontalBracket"));

      announce_grob (sp, events_[k]->self_scm());
      for (int i = 0; i < bracket_stack_.size(); i++)
	{
	  /*
	    sp is the smallest, it should be added to the bigger brackets.
	   */
	  Side_position_interface::add_support (bracket_stack_[i], sp);
	}
      bracket_stack_.push (sp);
    }
}


void
Horizontal_bracket_engraver::stop_translation_timestep ()
{
  for (int i = pop_count_; i--;)
    {
      if (bracket_stack_.size())
	typeset_grob (bracket_stack_.pop());
    }
}


