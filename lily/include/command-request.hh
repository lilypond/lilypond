/*
  command-request.hh -- declare non-musical requests

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef COMMANDREQUEST_HH
#define COMMANDREQUEST_HH

#include "request.hh"
#include "array.hh"
#include "duration.hh"
#include "musical-pitch.hh"

class Break_req : public Request {
public:
  enum { DISALLOW = -10000, FORCE = 10000 };
  int penalty_i_;
  Break_req ();
protected:
  VIRTUAL_COPY_CONS(Music);
};

class Mark_req : public Request {
public:
  Mark_req (String);
  String str_;
protected:
  virtual void do_print () const;  
  VIRTUAL_COPY_CONS(Music);
};


/** Baseclass for time_signature/partial req. It has to be handled by
  Staff_{walker,column} baseclass.  */
class Timing_req  : public Request  {
public:
  VIRTUAL_COPY_CONS(Music);
};


class Tempo_req : public Timing_req
{
public:
  Duration dur_;
  int metronome_i_;

  Tempo_req();
protected:
    virtual void do_print () const;
  VIRTUAL_COPY_CONS(Music);
  bool do_equal_b (Request *) const;
};

class Partial_measure_req  : public Timing_req  {
public:
  Moment length_mom_;

  Partial_measure_req (Moment);
protected:
  VIRTUAL_COPY_CONS(Music);
  virtual void do_print () const;
  bool do_equal_b (Request*) const;
};

/**
  todo: allow C time_signature
 */
class Time_signature_change_req  : public Timing_req  {
public:
  int beats_i_;
  int one_beat_i_;

  Time_signature_change_req();
protected:
  virtual void do_print () const;
  bool do_equal_b (Request*) const;
  VIRTUAL_COPY_CONS(Music);
};

/// toggle Cadenza mode
class Cadenza_req  : public Timing_req  {
public:
  /// turn on?
  bool on_b_;
  Cadenza_req (bool);
protected:
  virtual void do_print () const;
  
  bool do_equal_b (Request*) const;
  VIRTUAL_COPY_CONS(Music);
};

/// check if we're at start of a  measure.
class Barcheck_req  : public Timing_req  {
public:
  bool do_equal_b (Request *) const;
  VIRTUAL_COPY_CONS(Music);
};

class Measure_grouping_req : public Timing_req  {
public:
  Array<int> beat_i_arr_;
  Array<Moment> elt_length_arr_;
protected:
  virtual void do_print () const;
  bool do_equal_b (Request *) const;
  VIRTUAL_COPY_CONS(Music);
};

/** draw a (repeat)-bar. This something different than #Barcheck_req#,
  the latter should only happen at the start of a measure.  */
class Bar_req  : public Request  {
public:
  String type_str_;
  Bar_req (String);
protected:
  virtual void do_print () const;
  bool do_equal_b (Request*) const;

  VIRTUAL_COPY_CONS(Music);
};


/**
    Handle key changes.
    Routines for sharps and flats are separated, 
    so that caller may identify non-conventional keys.
*/
class Key_change_req  : public Request  {
public:
  Array<Musical_pitch> pitch_arr_;
  int modality_i_;
  bool ordinary_key_b_;

  Key_change_req();

  /// squash the octaves to 1
  void squash_octaves();
  /// return number of flats in key
  int flats_i();

  /// return number of sharps in key
  int sharps_i();
  bool minor_b() const;

protected:
  VIRTUAL_COPY_CONS(Music);
  void transpose (Musical_pitch  d);
  virtual void do_print () const;
};

class Clef_change_req  : public Request  {
public:
  String clef_str_;
  Clef_change_req (String);
protected:
  virtual void do_print () const;
  VIRTUAL_COPY_CONS(Music);
};

class Bracket_req :  public Span_req {

public:
  VIRTUAL_COPY_CONS(Music);
};


#endif // COMMANDREQUEST_HH
