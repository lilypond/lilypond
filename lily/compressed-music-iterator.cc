/*   
  compressed-music-iterator.cc --  implement Compressed_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "compressed-music-iterator.hh"
#include "compressed-music.hh"
#include "musical-request.hh"
#include "translator-group.hh"
#include "command-request.hh"

IMPLEMENT_IS_TYPE_B1(Compressed_music_iterator, Music_wrapper_iterator);

Compressed_music_iterator::Compressed_music_iterator ()
{
  Bracket_req pr;
  pr.spantype = Span_req::START;
  //  pr.plet_i_ = 1;
  start_req_p_ = new Bracket_req (pr);
  pr.spantype = Span_req::STOP;
  stop_req_p_ =new Bracket_req (pr);
}

Compressed_music_iterator::~Compressed_music_iterator ()
{
  delete start_req_p_;
  delete stop_req_p_;
}

Compressed_music *
Compressed_music_iterator::compressed_l () const
{
  return (Compressed_music*) music_l_;
}

void
Compressed_music_iterator::construct_children ()
{
  /*
    start_req_p_->plet_i_ = compressed_l ()->den_i_;
    stop_req_p_->plet_i_ = compressed_l ()->den_i_;  
  */
  Music_wrapper_iterator::construct_children ();
}

void
Compressed_music_iterator::do_process_and_next (Moment m)
{
  if (first_b_)
    {
      bool success = report_to_l ()->try_request (start_req_p_);
      if (!success)
	music_l_->warning ( _("No one to print a tuplet start bracket"));
    }

  Music_wrapper_iterator::do_process_and_next (m);
  
  if (!ok ())
    {
      bool success = report_to_l ()->try_request (stop_req_p_);
      if (!success)
          music_l_->warning ( _("No one to print a tuplet stop bracket"));
    }
}
