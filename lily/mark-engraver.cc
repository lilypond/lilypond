/*
  mark-engraver.cc -- implement Mark_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2003 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <ctype.h>
#include "bar-line.hh"

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
  virtual bool try_music (Music *req);
  virtual void start_translation_timestep ();
  virtual void process_music ();
  
private:
  Music * mark_req_;
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
Mark_engraver::create_items (Music *rq)
{
  if (text_)
    return;

  text_ = new Item (get_property ("RehearsalMark"));
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
  mark_req_ = r;
  return true;
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

      /*
	automatic marks.
       */
      
      SCM m = mark_req_->get_mus_property ("label");
      if (Text_item::markup_p (m))
	{
	  text_->set_grob_property ("text",m);
	}
      else 
	{
	  String t ;
	  
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
		  t = to_string (c);
		}
	      m = scm_makfrom0str (t.to_str0 ());
	    }
	  else
	    {
	      m = gh_int2scm (1);
	      t = to_string (1);
	    }
	  
	  text_->set_grob_property ("text",
				    scm_makfrom0str (t.to_str0 ()));

	  SCM series = SCM_EOL;
	  SCM family = ly_symbol2scm ("number");
	  for (int i=0; i < t.length (); i++)
	    {
	      if (!isdigit (t[i])) 
		{
		  /*
		    This looks strange, since \mark "A"
		    isn't printed in bold.
		    
		   */
		  
		  // series = ly_symbol2scm ("bold");
		  family = ly_symbol2scm ("roman");
		  break;
		}
	    }
	  if (gh_symbol_p (series))
	    text_->set_grob_property ("font-series",  series);
	  if (gh_symbol_p (family))
	    text_->set_grob_property ("font-family",  family);
	}

      if (gh_number_p (m) || gh_string_p (m))
	daddy_trans_->set_property ("rehearsalMark", m);
    }
}

ENTER_DESCRIPTION(Mark_engraver,
/* descr */       "",
/* creats*/       "RehearsalMark",
/* accepts */     "mark-event",
/* acks  */       "bar-line-interface",
/* reads */       "rehearsalMark stavesFound",
/* write */       "");
