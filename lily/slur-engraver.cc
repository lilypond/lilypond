/*
  slur-grav.cc -- implement Slur_engraver

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "musical-request.hh"
#include "slur-engraver.hh"
#include "slur.hh"
#include "debug.hh"
#include "note-column.hh"
#include "translator-group.hh"
#include "engraver.hh"
#include "spanner.hh"

class Slur_engraver :public Engraver {
  Link_array<Span_req> requests_arr_;
  Link_array<Span_req> new_slur_req_l_arr_;
  Link_array<Score_element> slur_l_stack_;
  Link_array<Score_element> end_slur_l_arr_;

  void set_melisma (bool);
protected:
  virtual bool do_try_music (Music*);
  virtual void do_process_music ();
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void do_removal_processing ();

public:
  VIRTUAL_COPY_CONS (Translator);
  
};

bool
Slur_engraver::do_try_music (Music *req_l)
{
  if (Span_req *sl = dynamic_cast <Span_req *> (req_l))
    {
      if (sl->span_type_str_ != "slur")
	return false;
      new_slur_req_l_arr_.push (sl);

      return true;
    }
  return false;
}

void
Slur_engraver::set_melisma (bool m)
{
  daddy_trans_l_->set_property ("slurMelismaBusy", m ? SCM_BOOL_T :SCM_BOOL_F);
}

void
Slur_engraver::acknowledge_element (Score_element_info info)
{
  if (Note_column::has_interface (info.elem_l_))
    {
      Score_element *e =info.elem_l_;
      for (int i = 0; i < slur_l_stack_.size (); i++)
	Slur::add_column (slur_l_stack_[i], e);
      for (int i = 0; i < end_slur_l_arr_.size (); i++)
	Slur::add_column (end_slur_l_arr_[i], e);
    }
}

void
Slur_engraver::do_removal_processing ()
{
  for (int i = 0; i < slur_l_stack_.size (); i++)
    {
      typeset_element (slur_l_stack_[i]);
    }
  slur_l_stack_.clear ();
  SCM wg = get_property ("weAreGraceContext");
  bool wgb = to_boolean (wg);
  if (!wgb)
    for (int i=0; i < requests_arr_.size (); i++)
      {
	requests_arr_[i]->origin ()->warning (_ ("unterminated slur"));
      }
}

void
Slur_engraver::do_process_music ()
{
  Link_array<Score_element> start_slur_l_arr_;
  for (int i=0; i< new_slur_req_l_arr_.size (); i++)
    {
      Span_req* slur_req_l = new_slur_req_l_arr_[i];
      // end slur: move the slur to other array
      if (slur_req_l->span_dir_ == STOP)
	{
	  if (slur_l_stack_.empty ())

	    slur_req_l->origin ()->warning (_f ("can't find both ends of %s", _ ("slur")));
	  else
	    {
	      Score_element* slur = slur_l_stack_.pop ();
	      SCM s = get_property ("slurEndAttachment");
	      if (gh_symbol_p (s))
		{
		  index_set_cell (slur->get_elt_property ("attachment"), STOP, s);
		}
	      end_slur_l_arr_.push (slur);
	      requests_arr_.pop ();
	    }
	}
      else  if (slur_req_l->span_dir_ == START)
	{
	  // push a new slur onto stack.
	  // (use temp. array to wait for all slur STOPs)
	  Score_element* slur = new Spanner (get_property ("basicSlurProperties"));
	  Slur::set_interface (slur);
	  SCM s = get_property ("slurBeginAttachment");
	  if (gh_symbol_p (s))
	    {
	      index_set_cell (slur->get_elt_property ("attachment"), START, s);
	    }
	  start_slur_l_arr_.push (slur);
	  requests_arr_.push (slur_req_l);
	  announce_element (slur, slur_req_l);
	}
    }
  for (int i=0; i < start_slur_l_arr_.size (); i++)
    slur_l_stack_.push (start_slur_l_arr_[i]);
}

void
Slur_engraver::do_pre_move_processing ()
{
  for (int i = 0; i < end_slur_l_arr_.size (); i++)
    {
      typeset_element (end_slur_l_arr_[i]);
    }
  end_slur_l_arr_.clear ();
}

void
Slur_engraver::do_post_move_processing ()
{
  new_slur_req_l_arr_.clear ();
  SCM m = get_property ("automaticMelismata");
  if (to_boolean (m))
    {
      set_melisma (slur_l_stack_.size ());
    }
}


ADD_THIS_TRANSLATOR (Slur_engraver);
