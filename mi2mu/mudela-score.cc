//
// mudela-score.cc -- implement Mudela_score
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "moment.hh"
#include "duration.hh"
#include "duration-convert.hh"
#include "mi2mu-global.hh"
#include "mudela-column.hh"
#include "mudela-item.hh"
#include "mudela-score.hh"
#include "mudela-staff.hh"
#include "mudela-stream.hh"

Mudela_score::Mudela_score (int format_i, int tracks_i, int tempo_i)
{
    format_i_ = format_i;
    tracks_i_ = tracks_i;
    tempo_i_ = tempo_i;
    mudela_column_p_list_.bottom().add (new Mudela_column (this, Moment (0)));
}

Mudela_score::~Mudela_score()
{
}

void 
Mudela_score::add_item (Mudela_item* mudela_item_p)
{
    mudela_staff_p_list_.bottom()->add_item (mudela_item_p);
}

void
Mudela_score::add_staff (Mudela_staff* mudela_staff_p)
{
    mudela_staff_p_list_.bottom().add (mudela_staff_p);
}

Mudela_column*
Mudela_score::mudela_column_l (Moment mom)
{
    for  (PCursor<Mudela_column*> i (mudela_column_p_list_); i.ok(); i++) {
	if  (i->at_mom() > mom) {
	    Mudela_column* p = new Mudela_column (this, mom);
	    i.insert (p);
	    return p;
	}
	if  (i->at_mom() == mom)
	    return *i;
    }

    Mudela_column* p = new Mudela_column (this, mom);
    mudela_column_p_list_.bottom().add (p);
    return p;
}

void
Mudela_score::output (String filename_str)
{
    LOGOUT(NORMAL_ver) << "Lily output to " << filename_str << " ..." << endl;
    
    // ugh, ugly midi type 1 fix
    if  ( (mudela_staff_p_list_.size() == 1) && !mudela_staff_p_list_.top()->number_i_)
	mudela_staff_p_list_.top()->number_i_ = 1;

    int track_i = 0;
    Mudela_stream mudela_stream (filename_str);
    for  (PCursor<Mudela_staff*> i (mudela_staff_p_list_); i.ok(); i++) {
	LOGOUT(NORMAL_ver) << "track " << track_i++ << ": " << flush;
	i->output (mudela_stream);
	mudela_stream << "\n";
	LOGOUT(NORMAL_ver) << endl;
    }

    mudela_stream << "\\score{\n";
    if  (mudela_staff_p_list_.size() > 1)
	mudela_stream << "<\n\\multi 3;\n";
    for  (PCursor<Mudela_staff*> i (mudela_staff_p_list_); i.ok(); i++) {
	if  ( (mudela_staff_p_list_.size() != 1) 
	    &&  (i == mudela_staff_p_list_.top()))
	    continue;
	mudela_stream << "\\melodic{ ";
	mudela_stream << "\\$" << i->id_str();
	mudela_stream << " }\n";
    }
    if  (mudela_staff_p_list_.size() > 1)
	mudela_stream << ">\n";

    mudela_stream << "\\paper{}\n";

    mudela_stream << "\\midi{ ";
	// let's not use silly 0 track
	mudela_staff_p_list_.bottom()->mudela_tempo_p_->output (mudela_stream);
    mudela_stream << "}\n";

    mudela_stream << "}\n";
}

void
Mudela_score::process()
{
    LOGOUT(NORMAL_ver) << "\nProcessing..." << endl;
	
    LOGOUT(DEBUG_ver) << "columns\n";
    for  (PCursor<Mudela_column*> i (mudela_column_p_list_); i.ok(); i++)
	LOGOUT(DEBUG_ver) << "At: " << i->at_mom() << "\n";

    settle_columns();
    filter_tempo();
    quantify_columns();
    quantify_durations();

    LOGOUT(NORMAL_ver) << "\nCreating voices..." << endl;
    int track_i = 0;
    for  (PCursor<Mudela_staff*> i (mudela_staff_p_list_); i.ok(); i++)  {
	LOGOUT(NORMAL_ver) << "track " << track_i++ << ": " << flush;
	i->process();
	LOGOUT(NORMAL_ver) << endl;
    }
}

void
Mudela_score::filter_tempo()
{
    LOGOUT(NORMAL_ver) << "\nNOT Filtering tempo..." << endl;
}

void
Mudela_score::quantify_columns()
{
    // ugh
    if  (Duration_convert::no_quantify_b_s) {
	LOGOUT(NORMAL_ver) << "\nNOT Quantifying columns..." << endl;
	return;
    }

    LOGOUT(NORMAL_ver) << "\nQuantifying columns..." << endl;

    int n = 32 >? Duration_convert::no_smaller_than_i_s;
    Moment s = Moment (1, n);
    Moment sh = Moment (1, 2 * n);
    for  (int i = 0; i < column_l_array_.size(); i++) {
//	Moment mom = column_l_array_[ i ]->at_mom();
//	column_l_array_[ i ]->at_mom_ = Duration_convert::dur2_mom (dur);
	column_l_array_[ i ]->at_mom_ =
//	    s * (int) ( (sh + column_l_array_[ i ]->at_mom()) / s);
	    s * (int) ( (column_l_array_[ i ]->at_mom()) / s);
	LOGOUT(NORMAL_ver) << '.';
    } 
    LOGOUT(NORMAL_ver) << endl;
}

void
Mudela_score::quantify_durations()
{
//    LOGOUT(NORMAL_ver) << "\nQuantifying durations..." << endl;
}

void
Mudela_score::settle_columns()
{
//    LOGOUT(NORMAL_ver) << "\nNOT Settling columns..." << endl;
//    return;
    LOGOUT(NORMAL_ver) << "\nSettling columns..." << endl;

    assert (!column_l_array_.size());
    int n = mudela_column_p_list_.size();
// huh?
//    column_l_array_.set_size (n);
    for  (PCursor<Mudela_column*> i (mudela_column_p_list_); i.ok(); i++)
	column_l_array_.push (*i);

    int start_i = 0;
    int end_i = 0;
    Moment start_mom = 0;
    Duration smallest_dur;
    smallest_dur.type_i_ =  64;
    Moment const noise_mom = Duration_convert::dur2_mom (smallest_dur)
	/ Moment (2);
    for  (int i = 0; i < n; i++) {
	if  (!start_i) {
	    start_i = end_i = i;
	    start_mom = column_l_array_[ i ]->at_mom();
	    continue;
	}

	// find all columns within noise's distance
	while  ( (i < n)
	    &&  (column_l_array_[ i ]->at_mom() - start_mom < noise_mom))
	    end_i = ++i;

	// bluntly set all to time of first in group
	for  (int j = start_i; j < end_i; j++)
	    column_l_array_[ j ]->at_mom_ = start_mom;

	start_i = end_i = 0;
    }
}

