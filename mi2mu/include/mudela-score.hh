//
// mudela-score.hh -- declare Mudela_score
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef MUDELA_SCORE_HH
#define MUDELA_SCORE_HH

#include "mi2mu-proto.hh"
#include "proto.hh"
#include "plist.hh"
#include "parray.hh"

/// (mudela_score)
class Mudela_score {
public:
    Mudela_score (int format_i, int tracks_i, int tempo_i);
    ~Mudela_score();

    void add_item (Mudela_item* mudela_item_p);
    void add_staff (Mudela_staff* mudela_staff_p);

    Mudela_column* mudela_column_l (Moment mom);

    void output (String filename_str);
    void process();

    // ugh
    Mudela_key* mudela_key_l_;
    Mudela_meter* mudela_meter_l_;
    Mudela_tempo* mudela_tempo_l_;

private:
    void filter_tempo();
    void quantify_columns();
    void quantify_durations();
    void settle_columns();

    Pointer_list<Mudela_column*> mudela_column_p_list_;
    Pointer_list<Mudela_staff*> mudela_staff_p_list_;
//    Link_array<Mudela_column*> column_l_array_;
    // huh?
    Link_array<Mudela_column> column_l_array_;

// ugh, ugh, ugh
public:
    int format_i_;
    int tracks_i_;
    int tempo_i_;
};

#endif // MUDELA_SCORE_HH

