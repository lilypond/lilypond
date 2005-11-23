/*
  mark-engraver.cc -- implement Mark_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2005 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <cctype>
using namespace std;

#include "bar-line.hh"
#include "context.hh"
#include "engraver-group.hh"
#include "item.hh"
#include "warn.hh"
#include "text-interface.hh"
#include "grob-array.hh"

/**
   put stuff over or next to  bars.  Examples: bar numbers, marginal notes,
   rehearsal marks.
*/
class Mark_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Mark_engraver);
protected:
  Item *text_;
protected:
  void stop_translation_timestep ();
  DECLARE_ACKNOWLEDGER (bar_line);
  void create_items (Music *);
  virtual bool try_music (Music *ev);
  void process_music ();

private:
  Music *mark_ev_;
};

Mark_engraver::Mark_engraver ()
{
  text_ = 0;
  mark_ev_ = 0;
}

void
Mark_engraver::acknowledge_bar_line (Grob_info inf)
{
  Grob *s = inf.grob ();
  if (text_)
    {
      /*
	TODO: make this configurable. RehearsalMark cannot be
	break-aligned, since the width of the object should not be taken
	into alignment considerations.
      */
      text_->set_parent (s, X_AXIS);
    }
}

void
Mark_engraver::stop_translation_timestep ()
{
  if (text_)
    {
      text_->set_object ("side-support-elements",
			 grob_list_to_grob_array (get_property ("stavesFound")));
      text_ = 0;
    }
  mark_ev_ = 0;
}

void
Mark_engraver::create_items (Music *ev)
{
  if (text_)
    return;

  text_ = make_item ("RehearsalMark", ev->self_scm ());
}

bool
Mark_engraver::try_music (Music *r)
{
  mark_ev_ = r;
  return true;
}

/*
  TODO: make the increment function in Scheme.
*/
void
Mark_engraver::process_music ()
{
  if (mark_ev_)
    {
      create_items (mark_ev_);

      /*
	automatic marks.
      */

      SCM m = mark_ev_->get_property ("label");
      SCM proc = get_property ("markFormatter");
      if (!Text_interface::is_markup (m)
	  && ly_is_procedure (proc))
	{
	  if (!scm_is_number (m))
	    m = get_property ("rehearsalMark");

	  if (scm_integer_p (m) == SCM_BOOL_T
	      && scm_exact_p (m) == SCM_BOOL_T)
	    {
	      int mark_count = scm_to_int (m);
	      mark_count++;
	      context ()->set_property ("rehearsalMark",
					scm_from_int (mark_count));
	    }

	  if (scm_is_number (m))
	    m = scm_call_2 (proc, m, context ()->self_scm ());
	  else
	    /* FIXME: constant error message.  */
	    warning (_ ("rehearsalMark must have integer value"));
	}

      if (Text_interface::is_markup (m))
	text_->set_property ("text", m);
      else
	warning (_ ("mark label must be a markup object"));
    }
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Mark_engraver, bar_line);

ADD_TRANSLATOR (Mark_engraver,
		/* doc */ "This engraver will create RehearsalMark objects. "
		"It puts them on top of all staves (which is taken from "
		"the property @code{stavesFound}). If moving this engraver "
		"to a different context, "
		"@ref{Staff_collecting_engraver} must move along, otherwise all marks"
		"end up on the same Y-location",
		/* create */ "RehearsalMark",
		/* accept */ "mark-event",
		/* read */ "rehearsalMark markFormatter stavesFound",
		/* write */ "");
