/*
  command-request.hh -- declare non-musical requests

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef COMMANDREQUEST_HH
#define COMMANDREQUEST_HH

#include "request.hh"
#include "array.hh"
#include "duration.hh"
#include "musical-pitch.hh"
#include "key-def.hh"
#include "protected-scm.hh"

class Break_req : public Request {
public:
  Real penalty_f_;
  Break_req ();
protected:
  VIRTUAL_COPY_CONS(Music);
};

class Mark_req : public Request {
public:
  Protected_scm mark_label_;
protected:
  virtual bool do_equal_b (Request const*) const;
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
  bool do_equal_b (Request const *) const;
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
  bool do_equal_b (Request const *) const;
  VIRTUAL_COPY_CONS(Music);
};


/// check if we're at start of a  measure.
class Barcheck_req  : public Timing_req  {
public:
  bool do_equal_b (Request const *) const;
  VIRTUAL_COPY_CONS(Music);
};


/** draw a (repeat)-bar. This something different than #Barcheck_req#,
  the latter should only happen at the start of a measure.  */
class Bar_req  : public Request  {
public:
  String type_str_;
  Bar_req (String);
protected:
  virtual bool do_equal_b (Request const*) const;
  virtual void do_print () const;

  VIRTUAL_COPY_CONS(Music);
};

class Breathing_sign_req : public Request {
  VIRTUAL_COPY_CONS(Music);
};

/**
    Handle key changes.
    Routines for sharps and flats are separated, 
    so that caller may identify non-conventional keys.
*/
class Key_change_req  : public Request
{
public:
  Key_change_req ();
  ~Key_change_req();
  Key_change_req(Key_change_req const &);
  Key_def *key_;

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


#endif // COMMANDREQUEST_HH
