/*
  mark-engraver.cc -- implement Mark_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2002 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <ctype.h>
#include "bar-line.hh"
#include "command-request.hh"
#include "staff-symbol.hh"
#include "engraver-group-engraver.hh"
#include "engraver.hh"
#include "lily-guile.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "item.hh"
#include "group-interface.hh"

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
  void create_items (Request*);
  virtual bool try_music (Music *req);
  virtual void start_translation_timestep ();
  virtual void process_music ();
  
private:
  Mark_req * mark_req_;
};





Mark_engraver::Mark_engraver ()
{
  text_ =0;
  mark_req_ = 0;
}

void
Mark_engraver::acknowledge_grob (Grob_info inf)
{
  Grob * s = inf.grob_;
  if (text_ && Bar_line::has_interface (s))
    {
      /*
	Ugh. Figure out how to do this right at beginning of line, (without
	creating class Bar_script : public Item).
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
}


void
Mark_engraver::create_items (Request *rq)
{
  if (text_)
    return;

  SCM s = get_property ("RehearsalMark");
  text_ = new Item (s);


  announce_grob(text_, rq->self_scm());
}


void
Mark_engraver::start_translation_timestep ()
{
  mark_req_ = 0;
}


bool
Mark_engraver::try_music (Music* r)
{
  if (Mark_req *mr = dynamic_cast <Mark_req *> (r))
    {
      if (mark_req_ && mr->equal_b (mark_req_))
	return true;
      if (mark_req_)
	return false;
      mark_req_ = mr;
      return true;
    }
  return false;
}


/*

  TODO: make the increment function in Scheme.

*/
void
Mark_engraver::process_music ()
{
  if (mark_req_)
    {
      create_items (mark_req_);

      String t;

      /*
	automatic marks.
       */
      
      SCM m = mark_req_->get_mus_property ("label");
      if (gh_pair_p (m)) // markup text
	text_->set_grob_property ("text",m);
      else 
	{
	  if (!gh_string_p (m) && !gh_number_p (m)) 
	    m =  get_property ("rehearsalMark");
	  
	  if (gh_number_p (m))
	    {
	      int mark_count = gh_scm2int (m);
	      t = to_string (mark_count);
	      mark_count ++;
	      m = gh_int2scm (mark_count);
	    }
	  else if (gh_string_p (m))
	    {
	      t = ly_scm2string (m);
	      String next;
	      if (t.length ())
		{
		  char c = t[0];
		  c++;
		  next = to_string (c);
		}
	      m = scm_makfrom0str (next.to_str0 ());
	    }
	  else
	    {
	      m = gh_int2scm (1);
	    }
	  
	  daddy_trans_->set_property ("rehearsalMark", m);
	  
	  text_->set_grob_property ("text",
				      scm_makfrom0str (t.to_str0 ()));

	  String style = "mark-number";
	  for (int i=0; i < t.length (); i++)
	    {
	      if (!isdigit (t[i])) 
		{
		  style = "mark-letter";
		  break;
		}
	    }
	  SCM st = ly_symbol2scm (style.to_str0 ());
	  text_->set_grob_property ("font-style",  st);
	}

    }
}

ENTER_DESCRIPTION(Mark_engraver,
/* descr */       "",
/* creats*/       "RehearsalMark",
/* accepts */     "general-music",
/* acks  */      "bar-line-interface",
/* reads */       "rehearsalMark stavesFound",
/* write */       "");
