/*
  score-performer.cc -- implement Score_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997 Jan Nieuwenhuizen <jan@digicash.com>
 */

#include "score-performer.hh"
#include "input-translator.hh"
#include "midi-def.hh"
#include "audio-column.hh"
#include "audio-item.hh"
#include "audio-score.hh"
#include "midi-stream.hh"
#include "string-convert.hh"
#include "debug.hh"
#include "score.hh"
#include "source-file.hh"
#include "source.hh"
#include "audio-staff.hh"

IMPLEMENT_IS_TYPE_B1(Score_performer,Performer_group_performer);

ADD_THIS_PERFORMER(Score_performer);


Score_performer::Score_performer()
{
}


Score_performer::~Score_performer()
{
}


Translator* 
Score_performer::ancestor_l (int l) 
{ 
    return Global_translator::ancestor_l (l);
}


int 
Score_performer::depth_i() const 
{ 
    return Global_translator::depth_i();
}


void
Score_performer::finish()
{
    Performer_group_performer::do_removal_processing();
}


void
Score_performer::play (Audio_element * p)
{
    if  (p->is_type_b (Audio_item::static_name())) {
	audio_column_l_->add ((Audio_item*)p);
    } else if (p->is_type_b (Audio_staff::static_name())) {
	score_l_->audio_score_p_->add_staff ((Audio_staff*)p);
    }
    score_l_->audio_score_p_->add (p);
}


void 
Score_performer::prepare (Moment m)
{
    now_mom_ = m;
    audio_column_l_ = new Audio_column (m);
    score_l_->audio_score_p_->add (audio_column_l_);
}


void 
Score_performer::process()
{
    process_requests();
}


void
Score_performer::set_score (Score* score_l)
{
    Global_translator::set_score (score_l);
}


void
Score_performer::start()
{
}


int
Score_performer::get_tempo_i()const
{
    return score_l_->midi_p_->get_tempo_i (Moment (1, 4));
}
