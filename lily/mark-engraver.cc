/*
  mark-engraver.cc -- implement Mark_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2003 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <ctype.h>

#include "bar-line.hh"

#include "engraver-group-engraver.hh"
#include "engraver.hh"
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
  TRANSLATOR_DECLARATIONS(Mark_engraver);
protected:
  Item* text_;
  
protected:
  virtual void stop_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);
  void create_items (Music*);
  virtual bool try_music (Music *ev);
  virtual void process_music ();
  
private:
  Music * mark_ev_;
};





Mark_engraver::Mark_engraver ()
{
  text_ =0;
  mark_ev_ = 0;
}

void
Mark_engraver::acknowledge_grob (Grob_info inf)
{
  Grob * s = inf.grob_;
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
      text_->set_grob_property ("side-support-elements" , get_property ("stavesFound"));
      typeset_grob (text_);
      text_ =0;
    }
  mark_ev_ = 0;
}


void
Mark_engraver::create_items (Music *rq)
{
  if (text_)
    return;

  text_ = new Item (get_property ("RehearsalMark"));
  announce_grob(text_, rq->self_scm());
}


bool
Mark_engraver::try_music (Music* r)
{
  mark_ev_ = r;
  return true;
}


/*

  TODO: make the increment function in Scheme.


  TODO: junk the number type for rehearsalMark
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

      
      SCM m = mark_ev_->get_mus_property ("label");
      SCM proc = get_property ("markFormatter");
      if (!Text_item::markup_p (m) &&
	  gh_procedure_p (proc))
	{
	  if (!gh_number_p (m)) 
	    m =  get_property ("rehearsalMark");

	  if (scm_integer_p (m) == SCM_BOOL_T
	      && scm_exact_p (m) == SCM_BOOL_T)
	    {
	      int mark_count = gh_scm2int (m);
	      mark_count ++;
	      daddy_trans_->set_property ("rehearsalMark",
					  gh_int2scm (mark_count));
	    }

	  if (gh_number_p (m))
	    m = scm_call_2 (proc, m, daddy_trans_->self_scm ());
	  else
	    warning ("rehearsalMark does not have integer value.");
	}

      if (Text_item::markup_p (m))
	text_->set_grob_property ("text", m);
      else
	warning ("Mark label should be markup object.");
    }
}

ENTER_DESCRIPTION(Mark_engraver,
/* descr */       "",
/* creats*/       "RehearsalMark",
/* accepts */     "mark-event",
/* acks  */       "bar-line-interface",
/* reads */       "rehearsalMark markFormatter stavesFound",
/* write */       "");
