/*
  horizontal-bracket-engraver.cc -- implement
  Horizontal_bracket_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2002--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"
#include "international.hh"
#include "note-column.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"

#include "translator.icc"

class Horizontal_bracket_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Horizontal_bracket_engraver);
  vector<Spanner*> bracket_stack_;
  vector<Music*> events_;
  vsize pop_count_;
  vsize push_count_;

  virtual bool try_music (Music *);
  void stop_translation_timestep ();
  void process_music ();
  DECLARE_ACKNOWLEDGER (note_column);
};

ADD_ACKNOWLEDGER (Horizontal_bracket_engraver, note_column);
ADD_TRANSLATOR (Horizontal_bracket_engraver,
		"Create horizontal brackets over notes for musical analysis purposes.",
		"HorizontalBracket",
		"note-grouping-event",
		"",
		"");

Horizontal_bracket_engraver::Horizontal_bracket_engraver ()
{
  pop_count_ = 0;
  push_count_ = 0;
}

bool
Horizontal_bracket_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("note-grouping-event"))
    {
      Direction d = to_dir (m->get_property ("span-direction"));

      if (d == STOP)
	{
	  pop_count_++;
	  if (pop_count_ > bracket_stack_.size ())
	    m->origin ()->warning (_ ("don't have that many brackets"));
	}
      else
	{
	  push_count_++;
	  events_.push_back (m);
	}

      if (pop_count_ && push_count_)
	m->origin ()->warning (_ ("conflicting note group events"));

      return true;
    }
  return false;
}

void
Horizontal_bracket_engraver::acknowledge_note_column (Grob_info gi)
{
  for (vsize i = 0; i < bracket_stack_.size (); i++)
    {
      Side_position_interface::add_support (bracket_stack_[i], gi.grob ());
      Pointer_group_interface::add_grob (bracket_stack_[i],
					 ly_symbol2scm ("columns"), gi.grob ());
      add_bound_item (bracket_stack_[i],
		      gi.grob ());
    }
}

void
Horizontal_bracket_engraver::process_music ()
{
  for (vsize k = 0; k < push_count_; k++)
    {
      Spanner *sp = make_spanner ("HorizontalBracket", events_[k]->self_scm ());

      for (vsize i = 0; i < bracket_stack_.size (); i++)
	/* sp is the smallest, it should be added to the bigger brackets.  */
	Side_position_interface::add_support (bracket_stack_[i], sp);
      bracket_stack_.push_back (sp);
    }
}

void
Horizontal_bracket_engraver::stop_translation_timestep ()
{
  for (int i = pop_count_; i--;)
    if (bracket_stack_.size ())
      bracket_stack_.pop_back ();
  pop_count_ = 0;
  push_count_ = 0;
}

