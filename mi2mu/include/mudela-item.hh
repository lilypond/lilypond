//
// mudela-item.hh -- declare mudela_item
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#ifndef MUDELA_ITEM_HH
#define MUDELA_ITEM_HH

#include "mi2mu-proto.hh"
#include "string.hh"
#include "moment.hh"
#include "duration.hh"

// should these:
// * be Mudela_items
// * be Voice_elements/requests
// * get a name-change
// ?

/// (mudela_item)
class Mudela_item 
{
public:
    Mudela_item (Mudela_column* mudela_column_l);
    virtual ~Mudela_item ();
    
    virtual Moment at_mom();
    virtual Moment duration_mom();
    void output (Mudela_stream& mudela_stream_r);
    virtual String str() = 0;

    Mudela_column* mudela_column_l_;
};

class Mudela_key : public Mudela_item 
{
public:
    Mudela_key (int accidentals_i, int minor_i);

    String notename_str (int pitch_i);
    virtual String str();

//private:
    int accidentals_i_;
    int minor_i_;
};

class Mudela_time_signature : public Mudela_item 
{
public:
    Mudela_time_signature (int num_i, int den_i, int division_4_i, int count_32_i);

    Duration i2_dur (int time_i, int division_1_i);
    int clocks_1_i();
    int den_i();
    int num_i();
    virtual String str();
    Moment bar_mom();

private:
    Real sync_f_;
    Duration sync_dur_;
    int clocks_1_i_;
    int num_i_;
    int den_i_;
};

class Mudela_note : public Mudela_item 
{
public:
    Mudela_note (Mudela_column* mudela_column_l, int channel_i, int pitch_i, int dyn_i);

    Duration duration();
    virtual Moment duration_mom();
    virtual String str();
    
//    int const c0_pitch_i_c_ = 60; // huh?
    static int const c0_pitch_i_c_ = 48;

    static bool const simple_plet_b_s = false;
    int channel_i_;
    int pitch_i_;
    Mudela_column* end_column_l_;
};

class Mudela_skip : public Mudela_item 
{
public:
    Mudela_skip (Mudela_column* mudela_column_l, Moment skip_mom);

    Duration duration();
    virtual Moment duration_mom();
    virtual String str();

private:
    Moment mom_;
};


class Mudela_tempo : public Mudela_item 
{
public:
    Mudela_tempo (int useconds_per_4_i);

    int get_tempo_i (Moment moment);
    virtual String str();
    int useconds_per_4_i();

private:
    int useconds_per_4_i_;
    Moment seconds_per_1_mom_;
};

class Mudela_text : public Mudela_item 
{
public:
    enum Type { 
	TEXT = 1, COPYRIGHT, TRACK_NAME, INSTRUMENT_NAME, LYRIC, 
	MARKER, CUE_POINT
    };
    Mudela_text (Mudela_text::Type type,  String str);
    virtual String str();

//private:
    Type type_;
    String text_str_;
};

#endif // MUDELA_ITEM_HH

