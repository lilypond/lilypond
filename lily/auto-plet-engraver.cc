/*   
  auto-plet-engraver.cc --  implement Auto_plet_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "auto-plet-engraver.hh"
#include "command-request.hh"
#include "plet-spanner.hh"
#include "note-column.hh"

bool
Tuplet_engraver::do_try_request (Request *r)
{
  if (Bracket_req * b = dynamic_cast <Bracket_req *> (r))
    {
      bracket_req_arr_.push (b);
      return true;
    }
  return false;
}

void
Tuplet_engraver::do_process_requests ()
{
  int stopcount =0;
  Link_array<Plet_spanner> start_arr;
  
  for (int i=0; i < bracket_req_arr_.size (); i++)
    {
      if (bracket_req_arr_[i]->spantype == Span_req::STOP)
	stopcount++;
      if (bracket_req_arr_[i]->spantype == Span_req::START)
	{
	  Plet_spanner* glep = new Plet_spanner ();
	  start_arr.push  (glep);
// lots of stuff does info->elem_l_->is_type ()
//	  announce_element (Score_element_info (glep, bracket_req_arr_[i]));
	}
    }

  for (; stopcount--; )
    {
      Plet_spanner* glep = started_span_p_arr_.pop ();
      stop_now_span_p_arr_.push (glep);
    }

  for (int i=0; i < start_arr.size (); i++)
    started_span_p_arr_.push (start_arr.pop ());
}

void
Tuplet_engraver::acknowledge_element (Score_element_info i)
{
  if (Note_column *nc = dynamic_cast<Note_column *> (i.elem_l_))
    {

      for (int j =0; j  <started_span_p_arr_.size (); j++)
	// started_span_p_arr_[j]->add_column (nc);
	;
    }
}

void
Tuplet_engraver::do_pre_move_processing ()
{
  typeset_all ();
}

void
Tuplet_engraver::typeset_all ()
{
  for (int i=0; i < stop_now_span_p_arr_.size (); i++)
    {
      typeset_element (stop_now_span_p_arr_[i]);
    }

  stop_now_span_p_arr_.clear ();
}

void
Tuplet_engraver::do_removal_processing ()
{
  typeset_all ();
  for (int i=0; i < started_span_p_arr_.size (); i++)
    {
      typeset_element (started_span_p_arr_[i]);
    }  
}

ADD_THIS_TRANSLATOR(Tuplet_engraver);
IMPLEMENT_IS_TYPE_B1(Tuplet_engraver, Engraver);

Tuplet_engraver::Tuplet_engraver()
{
}


void
Tuplet_engraver::do_post_move_processing ()
{
  bracket_req_arr_.clear (); 
}
