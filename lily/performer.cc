/*
  performer.cc -- implement Performer

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
               Jan Nieuwenhuizen <jan@digicash.com>
 */


#include "performer-group-performer.hh"
#include "debug.hh"


IMPLEMENT_IS_TYPE_B(Performer);

Performer::Performer()
{
  daddy_perf_l_ = 0;
  init_b_ =false;
}

Performer::~Performer()
{
}

void
Performer::do_print() const
{
}

void 
Performer::play (Audio_element* p) 
{ 
  daddy_perf_l_->play (p); 
}

int
Performer::get_tempo_i()const
{
  return daddy_perf_l_->get_tempo_i();
}

void
Performer::print() const
{
#ifndef NPRINT
  DOUT << "\n" << name() << " {";
  do_print();
  DOUT << "}";
#endif
}

void
Performer::process_requests()
{
}

bool 
Performer::do_try_request (Request* req_l)
{
  return false;
}

bool
Performer::try_request (Request*r)
{
  if (!init_b_) 
    {
	creation_processing();
    }
  return do_try_request (r);
}

void
Performer::creation_processing()
{
  if (!init_b_) 
    {
	do_creation_processing();
	init_b_ = true;
    }
}

void
Performer::do_creation_processing()
{
}

void
Performer::do_removal_processing()
{
}
