/*
  score-performer.cc -- implement Score_performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#if 0

#include "score-performer.hh"

void 
Score_performer::play_event( Midi_item i )
{
    file_p_->output( i );
}

void 
Score_performer::prepare(Moment m)
{
    file_p_->move( mom.prev_ );
}

#endif
