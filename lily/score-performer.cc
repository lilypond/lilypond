/*
  score-performer.cc -- implement Score_performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#include "score-performer.hh"
#include "input-translator.hh"
#include "midi-item.hh"
#include "midi-stream.hh"
#include "string-convert.hh"
#include "debug.hh"

IMPLEMENT_IS_TYPE_B1(Score_performer,Performer_group_performer);
IMPLEMENT_STATIC_NAME(Score_performer);
ADD_THIS_PERFORMER(Score_performer);

Score_performer::Score_performer()
{
    // not so much ugh, as [todo]
    *mlog << "MIDI output to " << "lelie.midi" << " ...\n";    
    midi_stream_p_ = new Midi_stream( "lelie.midi", 
        // don-t forget: extra track 0 for tempo/copyright stuff...
    	//score_l_->staffs_.size() + 1, 
    	1, 
	384 );
}

Score_performer::~Score_performer()
{
    delete midi_stream_p_;
}

Translator* 
Score_performer::ancestor_l( int l ) 
{ 
    return Global_translator::ancestor_l( l );
}

int 
Score_performer::depth_i() const 
{ 
    return Global_translator::depth_i();
}

void 
Score_performer::play_event( Midi_item* l )
{
//    file_p_->output( l );
//    cerr << String_convert::bin2hex_str( l->str() );
    // this should be done at staff level, 
    // cached per staff probably.
    // but it generates output...
    *midi_stream_p_ << *l;
}

void 
Score_performer::prepare( Moment m )
{
    now_mom_ = m;
}

void 
Score_performer::process()
{
    process_requests();
    prev_mom_ = now_mom_;
}

