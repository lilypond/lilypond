/*
  command-request.hh -- declare Non musical requests

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef COMMANDREQUEST_HH
#define COMMANDREQUEST_HH

#include "request.hh"
#include "varray.hh"

/** Request which are  assumed to be "happening" before the
  musical requests. */
struct Command_req : virtual Request {
    REQUESTMETHODS(Command_req, command);

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
struct Timing_req : Command_req {
    REQUESTMETHODS(Timing_req, timing);
};


struct Partial_measure_req : Timing_req {
    Moment duration_;

    Partial_measure_req(Moment);
    REQUESTMETHODS(Partial_measure_req, partial);
};

/**
  todo: allow C meter
 */
struct Meter_change_req : Timing_req {
    int beats_i_, one_beat_i_;

    Meter_change_req();
    void set(int,int);
    REQUESTMETHODS(Meter_change_req, meterchange);
};

/// toggle Cadenza mode
struct Cadenza_req : Timing_req {
    /// turn on?
    bool on_b_;
    Cadenza_req(bool);
    REQUESTMETHODS(Cadenza_req,cadenza);
};

/// check if we're at start of a  measure.
struct Barcheck_req : Timing_req {

    REQUESTMETHODS(Barcheck_req,barcheck);
};

struct Measure_grouping_req: Timing_req {
    Array<int> beat_i_arr_;
    Array<Moment> elt_length_arr_;

    REQUESTMETHODS(Measure_grouping_req, measuregrouping);
};

struct Group_change_req : Command_req {
    String newgroup_str_;
    REQUESTMETHODS(Group_change_req, groupchange);
};

/** draw a (repeat)-bar. This something different than #Barcheck_req#,
  the latter should only happen at the start of a measure.  */
struct Bar_req : Command_req {
    String type_str_;
    Bar_req(String);
    int compare(const Bar_req&)const;
    REQUESTMETHODS(Bar_req,bar);
};
struct Terminate_voice_req : Command_req {
    REQUESTMETHODS(Terminate_voice_req,terminate);
};

struct Group_feature_req : Command_req {
    int stemdir_i_;
    Group_feature_req();
    REQUESTMETHODS(Group_feature_req, groupfeature);
};


struct Key_change_req : Command_req {
    Array<Melodic_req*> melodic_p_arr_;
    virtual void transpose(Melodic_req const &)const;
    Key_change_req();
    Key_change_req(Key_change_req const&);
    ~Key_change_req();
    REQUESTMETHODS(Key_change_req, keychange);
};

struct Clef_change_req : Command_req {
    String clef_str_;
    Clef_change_req(String);
    REQUESTMETHODS(Clef_change_req, clefchange);
};

#endif // COMMANDREQUEST_HH
