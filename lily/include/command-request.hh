/*
  command-request.hh -- declare Non musical requests

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef COMMANDREQUEST_HH
#define COMMANDREQUEST_HH

#include "request.hh"
#include "varray.hh"

/** Request which are  assumed to be "happening" before the
  musical requests. */
class Command_req  : public virtual Request  {
public:
    REQUESTMETHODS(Command_req, command);
    virtual Terminate_voice_req *terminate() {return 0;}
    virtual Group_change_req * groupchange() { return 0;}
    virtual Group_feature_req * groupfeature() { return 0; }
  
    virtual Measure_grouping_req * measuregrouping() { return 0; }
    virtual Clef_change_req * clefchange() { return 0; }
    virtual Key_change_req * keychange() { return 0; }
    virtual Partial_measure_req * partial() { return 0; }
    virtual Meter_change_req * meterchange() { return 0; }
    virtual Bar_req *bar() { return 0; }
    virtual Cadenza_req *cadenza() { return 0; }
    virtual Timing_req*timing() {  return 0; }
};


/** Baseclass for meter/partial req. It has to be handled by
  Staff_{walker,column} baseclass.  */
class Timing_req  : public Command_req  {
public:
    REQUESTMETHODS(Timing_req, timing);
};


class Partial_measure_req  : public Timing_req  {
public:
    Moment duration_;

    Partial_measure_req(Moment);
    REQUESTMETHODS(Partial_measure_req, partial);
};

/**
  todo: allow C meter
 */
class Meter_change_req  : public Timing_req  {
public:
    int beats_i_, one_beat_i_;

    int compare(Meter_change_req const&);
    Meter_change_req();
    void set(int,int);
    REQUESTMETHODS(Meter_change_req, meterchange);
};

/// toggle Cadenza mode
class Cadenza_req  : public Timing_req  {
public:
    /// turn on?
    bool on_b_;
    Cadenza_req(bool);
    REQUESTMETHODS(Cadenza_req,cadenza);
};

/// check if we're at start of a  measure.
class Barcheck_req  : public Timing_req  {
public:

    REQUESTMETHODS(Barcheck_req,barcheck);
};

class Measure_grouping_req : public Timing_req  {
public:
    Array<int> beat_i_arr_;
    Array<Moment> elt_length_arr_;

    REQUESTMETHODS(Measure_grouping_req, measuregrouping);
};

class Group_change_req  : public Command_req  {
public:
    String newgroup_str_;
    REQUESTMETHODS(Group_change_req, groupchange);
};

/** draw a (repeat)-bar. This something different than #Barcheck_req#,
  the latter should only happen at the start of a measure.  */
class Bar_req  : public Command_req  {
public:
    String type_str_;
    Bar_req(String);
    int compare(const Bar_req&)const;
    REQUESTMETHODS(Bar_req,bar);
};
class Terminate_voice_req  : public Command_req  {
public:
    REQUESTMETHODS(Terminate_voice_req,terminate);
};

class Group_feature_req  : public Command_req  {
public:
    String type_str_;
    String value_str_;
    
    REQUESTMETHODS(Group_feature_req, groupfeature);
};


/**
    Handle key changes.
    Routines for sharps and flats are separated, 
    so that caller may identify non-conventional keys.
*/
class Key_change_req  : public Command_req  {
public:
    Array<Melodic_req*> melodic_p_arr_;
    bool minor_b_;

    /// don't ignore the  octaves in #melodic_p_arr_#?
    bool multi_octave_b_;
    Key_change_req();
    Key_change_req(Key_change_req const&);
    ~Key_change_req();
    REQUESTMETHODS(Key_change_req, keychange);

    /// squash the octaves to 1
    void squash_octaves();
    /// return number of flats in key
    int flats_i();

    /// return number of sharps in key
    int sharps_i();

    void transpose(Melodic_req const & d) const;
    /// is minor key?
    int minor_b();
};

class Clef_change_req  : public Command_req  {
public:
    String clef_str_;
    Clef_change_req(String);
    REQUESTMETHODS(Clef_change_req, clefchange);
};

#endif // COMMANDREQUEST_HH
