/*
  mark-engraver.cc -- implement Mark_engraver

  source file of the GNU LilyPond music typesetter

 (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#include "bar.hh"
#include "clef-item.hh"
#include "command-request.hh"
#include "dimension-cache.hh"
#include "engraver-group-engraver.hh"
#include "engraver.hh"
#include "lily-guile.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "protected-scm.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "text-item.hh"

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
  Text_item* text_p_;
  Protected_scm visibility_lambda_;
  Protected_scm staffs_;
  
protected:
  virtual void do_creation_processing ();
  virtual void do_pre_move_processing ();
  virtual void acknowledge_element (Score_element_info);
  void create_items(Request*);
  virtual bool do_try_music (Music *req_l);
  virtual void do_process_music ();
  virtual void do_post_move_processing ();
private:
  Mark_req * mark_req_l_;
};


ADD_THIS_TRANSLATOR (Mark_engraver);


Mark_engraver::Mark_engraver ()
{
  text_p_ =0;
  mark_req_l_ = 0;
  staffs_ = SCM_EOL;
}

void
Mark_engraver::do_creation_processing ()
{
  String t = "markVisibilityFunction";
  SCM proc = get_property (t);

  if (gh_procedure_p (proc))
    visibility_lambda_ = proc;
}



void
Mark_engraver::acknowledge_element (Score_element_info inf)
{
  Score_element * s = inf.elem_l_;
  if (dynamic_cast<Staff_symbol*> (s))
    {
      staffs_ = gh_cons (inf.elem_l_->self_scm_, staffs_);
    }
  else if (text_p_ && dynamic_cast<Bar*> (s))
    {
      /*
	Ugh. Figure out how to do this right at beginning of line, (without
	creating class Bar_script : public Text_item).
      */
      text_p_->set_parent (s, X_AXIS);
    }
}

void 
Mark_engraver::do_pre_move_processing ()
{
  if (text_p_)
    {
      text_p_->set_elt_property ("side-support" , staffs_);
      typeset_element (text_p_);
      text_p_ =0;
    }
}


void
Mark_engraver::create_items (Request *rq)
{
  if (text_p_)
    return;
  
  text_p_ = new Text_item;
  text_p_->set_elt_property ("breakable", SCM_BOOL_T); // ugh
  Side_position_interface staffside(text_p_);
  staffside.set_axis (Y_AXIS);

  SCM prop = get_property ("markDirection");
  if (!isdir_b (prop))
    {
      prop = gh_int2scm (UP);
    }
  text_p_->set_elt_property ("direction", prop);

  SCM padding = get_property ("markScriptPadding");
  if (gh_number_p(padding))
    {
      text_p_->set_elt_property ("padding", padding);
    }
  else
    {
      text_p_
	->set_elt_property ("padding",
			    gh_double2scm(paper_l ()->get_var ("interline")));
    }

  if (gh_procedure_p (visibility_lambda_))
      text_p_->set_elt_property ("visibility-lambda",
				 visibility_lambda_);
  
  announce_element (Score_element_info (text_p_, rq));
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
      SCM m = (mark_req_l_->mark_label_ == SCM_UNDEFINED)
	? get_property ("rehearsalMark")
	: SCM(mark_req_l_->mark_label_);
      
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
      SCM st = ly_str02scm ((t.index_any_i ("0123456789")  >= 0 )
			    ? "mark" : "large");
      text_p_->set_elt_property ("style",  st);
    }
}

