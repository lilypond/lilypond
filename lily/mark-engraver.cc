/*
  mark-engraver.cc -- implement Mark_engraver

  source file of the GNU LilyPond music typesetter

 (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <ctype.h>
#include "bar.hh"
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
  VIRTUAL_COPY_CONS(Translator);
  Mark_engraver ();
protected:
  Item* text_p_;
  
protected:
  virtual void do_pre_move_processing ();
  virtual void acknowledge_element (Score_element_info);
  void create_items(Request*);
  virtual bool do_try_music (Music *req_l);
  virtual void do_process_music ();
  virtual void do_post_move_processing ();
  virtual void do_creation_processing ();
  
private:
  Mark_req * mark_req_l_;
};


ADD_THIS_TRANSLATOR (Mark_engraver);


Mark_engraver::Mark_engraver ()
{
  text_p_ =0;
  mark_req_l_ = 0;
}

void
Mark_engraver::do_creation_processing ()
{
  daddy_trans_l_->set_property ("staffsFound", SCM_EOL); // ugh: sharing with barnumber grav.
}


void
Mark_engraver::acknowledge_element (Score_element_info inf)
{
  Score_element * s = inf.elem_l_;
  if (Staff_symbol::has_interface (s)
      || to_boolean (s->get_elt_property ("invisible-staff")))
    {
      SCM sts = get_property ("staffsFound");
      SCM thisstaff = inf.elem_l_->self_scm ();
      if (scm_memq (thisstaff, sts) == SCM_BOOL_F)
	daddy_trans_l_->set_property ("staffsFound", gh_cons (thisstaff, sts));
    }
  else if (text_p_ && Bar::has_interface (s))
    {
      /*
	Ugh. Figure out how to do this right at beginning of line, (without
	creating class Bar_script : public Item).
      */
      text_p_->set_parent (s, X_AXIS);
    }
}

void 
Mark_engraver::do_pre_move_processing ()
{
  if (text_p_)
    {
      text_p_->set_elt_property("side-support-elements" , get_property ("staffsFound"));
      typeset_element (text_p_);
      text_p_ =0;
    }
}


void
Mark_engraver::create_items (Request *rq)
{
  if (text_p_)
    return;

  SCM s = get_property ("RehearsalMark");
  text_p_ = new Item (s);


  Side_position::set_axis (text_p_, Y_AXIS);

  /*
    -> Generic props.
   */
  SCM prop = get_property ("markDirection");
  if (!isdir_b (prop))
    {
      prop = gh_int2scm (UP);
    }
  text_p_->set_elt_property ("direction", prop);
  
  announce_element (text_p_, rq);
}


void
Mark_engraver::do_post_move_processing ()
{
  mark_req_l_ = 0;
}


bool
Mark_engraver::do_try_music (Music* r_l)
{
  if (Mark_req *mr = dynamic_cast <Mark_req *> (r_l))
    {
      if (mark_req_l_ && mr->equal_b (mark_req_l_))
	return true;
      if (mark_req_l_)
	return false;
      mark_req_l_ = mr;
      return true;
    }
  return false;
}

void
Mark_engraver::do_process_music ()
{
  if (mark_req_l_)
    {
      create_items (mark_req_l_);

      String t;

      /*
	automatic marks.
       */
      
      SCM m = mark_req_l_->get_mus_property ("label");
      if (!gh_string_p (m)) 
	m =  get_property ("rehearsalMark");
;
      
      if (gh_number_p (m))
	{
	  int mark_count = gh_scm2int (m);
	  t = to_str (mark_count);
	  mark_count ++;
	  m = gh_int2scm (mark_count);
	}
      else if (gh_string_p (m))
	{
	  t = ly_scm2string (m);
	  String next;
	  if (t.length_i ())
	    {
	      char c = t[0];
	      c++;
	      next = to_str (c);
	    }
	  m = ly_str02scm (next.ch_C());
	}
      else
	{
	  m = gh_int2scm (1);
	}
	  
      daddy_trans_l_->set_property ("rehearsalMark", m);

      
      text_p_->set_elt_property ("text",
				 ly_str02scm ( t.ch_C()));

      String style = "mark";
      for (int i=0; i < t.length_i(); i++)
	{
	  if (!isdigit(t[i])) 
	    {
	      style = "large";
	      break;
	    }
	}
      SCM st = ly_str02scm (style.ch_C());
      text_p_->set_elt_property ("style",  st);
    }
}

