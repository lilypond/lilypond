/*
  command-request.hh -- declare non-musical requests

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef COMMANDREQUEST_HH
#define COMMANDREQUEST_HH

#include "request.hh"
#include "array.hh"
#include "duration.hh"
#include "musical-pitch.hh"

/** Request which are  assumed to be "happening" before the
  musical requests.  Not coupled to a note or rest. */
class Command_req  : public virtual Request  {
public:
  REQUESTMETHODS(Command_req);

    DEFAULTACCESSOR(Measure_grouping_req)
    DEFAULTACCESSOR(Clef_change_req)
    DEFAULTACCESSOR(Key_change_req)
    DEFAULTACCESSOR(Partial_measure_req)
    DEFAULTACCESSOR(Time_signature_change_req)
    DEFAULTACCESSOR(Bar_req)
    DEFAULTACCESSOR(Cadenza_req)
    DEFAULTACCESSOR(Timing_req)
    DEFAULTACCESSOR(Command_script_req)
    DEFAULTACCESSOR(Break_req)
    DEFAULTACCESSOR(Mark_req)
  DEFAULTACCESSOR(Bracket_req)
};


class Break_req : public Command_req {
public:
  enum { DISALLOW = -10000, FORCE = 10000 };
  int penalty_i_;
  Break_req ();
  REQUESTMETHODS (Break_req);
};

class Mark_req : public Command_req {
public:
  Mark_req (String);
  String str_;
  REQUESTMETHODS (Mark_req);
};

class Command_script_req : public Command_req,  public Script_req {
public:
  // huh? 
  Command_script_req();
  ~Command_script_req();
  REQUESTMETHODS(Command_script_req);
};

/** Baseclass for time_signature/partial req. It has to be handled by
  Staff_{walker,column} baseclass.  */
class Timing_req  : public Command_req  {
public:
  REQUESTMETHODS(Timing_req);
  DEFAULTACCESSOR(Tempo_req)
};


class Tempo_req : public Timing_req
{
public:
  Duration dur_;
  int metronome_i_;

  Tempo_req();
  REQUESTMETHODS(Tempo_req);
  bool do_equal_b (Request *) const;
};

class Partial_measure_req  : public Timing_req  {
public:
  Moment duration_;

  Partial_measure_req (Moment);
  REQUESTMETHODS(Partial_measure_req);
  bool do_equal_b (Request*) const;
};

/**
  todo: allow C time_signature
 */
class Time_signature_change_req  : public Timing_req  {
public:
  int beats_i_, one_beat_i_;

  Time_signature_change_req();
  bool do_equal_b (Request*) const;
  REQUESTMETHODS(Time_signature_change_req);
};

/// toggle Cadenza mode
class Cadenza_req  : public Timing_req  {
public:
  /// turn on?
  bool on_b_;
  bool do_equal_b (Request*) const;
  Cadenza_req (bool);
  REQUESTMETHODS(Cadenza_req);
};

/// check if we're at start of a  measure.
class Barcheck_req  : public Timing_req  {
public:
  bool do_equal_b (Request *) const;
  REQUESTMETHODS(Barcheck_req);
};

class Measure_grouping_req : public Timing_req  {
public:
  Array<int> beat_i_arr_;
  Array<Moment> elt_length_arr_;
  bool do_equal_b (Request *) const;
  REQUESTMETHODS(Measure_grouping_req);
};

/** draw a (repeat)-bar. This something different than #Barcheck_req#,
  the latter should only happen at the start of a measure.  */
class Bar_req  : public Command_req  {
public:
  String type_str_;
  Bar_req (String);
  bool do_equal_b (Request*) const;

  REQUESTMETHODS(Bar_req);
};


/**
    Handle key changes.
    Routines for sharps and flats are separated, 
    so that caller may identify non-conventional keys.
*/
class Key_change_req  : public Command_req  {
public:
  Array<Musical_pitch> pitch_arr_;
  bool minor_b_;
  bool ordinary_key_b_;

  Key_change_req();
  REQUESTMETHODS(Key_change_req);

  /// squash the octaves to 1
  void squash_octaves();
  /// return number of flats in key
  int flats_i();

  /// return number of sharps in key
  int sharps_i();

  void transpose (Musical_pitch  d);
  /// is minor key?
  bool minor_b() const;
};

class Clef_change_req  : public Command_req  {
public:
  String clef_str_;
  Clef_change_req (String);
  REQUESTMETHODS(Clef_change_req);
};

class Bracket_req :  public Span_req, public Command_req {

public:
  REQUESTMETHODS(Bracket_req);
};

#endif // COMMANDREQUEST_HH
