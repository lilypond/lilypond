/*
  mark-engraver.cc -- implement Mark_engraver

  source file of the GNU LilyPond music typesetter

 (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "command-request.hh"
#include "bar-script-engraver.hh"
#include "engraver-group-engraver.hh"
#include "text-item.hh"

/**Print rehearsal marks.
  */
class Mark_engraver : public Bar_script_engraver 
{
public:
  Mark_engraver ();
  VIRTUAL_COPY_CONS(Translator);
protected:
  virtual bool do_try_music (Music *req_l);
  virtual void do_process_music ();
  virtual void do_post_move_processing ();
private:
  Mark_req * mark_req_l_;
};


ADD_THIS_TRANSLATOR (Mark_engraver);

Mark_engraver::Mark_engraver ()
{
  mark_req_l_ = 0;
  axis_ = Y_AXIS;
  type_ = "mark";
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

