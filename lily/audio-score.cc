/*
  audio-score.cc -- implement Audio_score

  source file of the GNU LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "debug.hh"
#include "midi-def.hh"
#include "audio-column.hh"
#include "audio-score.hh"

Audio_score::Audio_score( Midi_def* l )
{
    midi_l_ = l;
}

void
Audio_score::play( Audio_item* i, Audio_column* c )
{
    assert( c );
    assert( i );

    c->add( i );
//    typeset_element( i );
}

void
Audio_score::add( Audio_column* p )
{
    p->audio_score_l_ = this;
    audio_column_p_list_.bottom().add(p);
}

#if 0
void
Audio_score::output(Tex_stream &ts)
{
    ts << "\n "<<  midi_l_->lookup_l()->texsetting << "%(Tex id)\n";
    ts<< super_elem_l_->TeX_string();
    ts << "\n\\EndLilyPondOutput";
}
#endif

void
Audio_score::print() const
{    
#ifndef NPRINT
    mtor << "Audio_score { ";
    midi_l_->print();
    mtor << "\ncolumns: ";
    for ( PCursor<Audio_column*> i( audio_column_p_list_ ); i.ok(); i++ )
	i->print();
    mtor << "}\n";
#endif 
}

void
Audio_score::process()
{
#if 0
    clean_cols();
    print();
    *mlog << "Preprocessing elements... " <<flush;
    preprocess();
    *mlog << "\nCalculating column positions ... " <<flush;
    calc_breaking();
    *mlog << "\nPostprocessing elements..." << endl;
    postprocess();
#endif
}

