/*
  mark-engraver.cc -- implement Mark_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2005 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <cctype>

#include "bar-line.hh"
#include "context.hh"
#include "engraver-group-engraver.hh"
#include "item.hh"
#include "warn.hh"
#include "text-item.hh"

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
  virtual void stop_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);
  void create_items (Music *);
  virtual bool try_music (Music *ev);
  virtual void process_music ();

private:
  Music *mark_ev_;
};

Mark_engraver::Mark_engraver ()
{
  text_ = 0;
  mark_ev_ = 0;
}

void
Mark_engraver::acknowledge_grob (Grob_info inf)
{
  Grob *s = inf.grob_;
  if (text_ && Bar_line::has_interface (s))
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
      SCM lst = get_property ("stavesFound");
      text_->set_property ("side-support-elements", lst);
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
      if (!Text_interface::markup_p (m)
	  && ly_c_procedure_p (proc))
	{
	  if (!scm_is_number (m))
	    m = get_property ("rehearsalMark");

	  if (scm_integer_p (m) == SCM_BOOL_T
	      && scm_exact_p (m) == SCM_BOOL_T)
	    {
	      int mark_count = scm_to_int (m);
	      mark_count++;
	      context ()->set_property ("rehearsalMark",
					scm_int2num (mark_count));
	    }

	  if (scm_is_number (m))
	    m = scm_call_2 (proc, m, context ()->self_scm ());
	  else
	    warning ("rehearsalMark does not have integer value.");
	}

      if (Text_interface::markup_p (m))
	text_->set_property ("text", m);
      else
	warning ("Mark label should be markup object.");
    }
}

ADD_TRANSLATOR (Mark_engraver,
		/* descr */ "This engraver will create RehearsalMark objects. "
		"It puts them on top of all staves (which is taken from "
		"the property @code{stavesFound}). If moving this engraver "
		"to a different context, "
		"@ref{Staff_collecting_engraver} must move along, otherwise all marks"
		"end up on the same Y-location",
		/* creats*/ "RehearsalMark",
		/* accepts */ "mark-event",
		/* acks  */ "bar-line-interface",
		/* reads */ "rehearsalMark markFormatter stavesFound",
		/* write */ "");
