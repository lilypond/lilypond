/*
  meter-performer.hh -- declare Meter_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <jan@digicash.com>
*/

#ifndef METER_PERFOMER_HH
#define METER_PERFOMER_HH

#include "lily-proto.hh"
#include "performer.hh"

class Meter_performer : public Performer {
public:
  TRANSLATOR_CLONE(Meter_performer);
  DECLARE_MY_RUNTIME_TYPEINFO;
  Meter_performer();
  ~Meter_performer();

protected:
  void do_print() const;
  virtual bool do_try_request (Request* req_l);
  virtual void do_process_requests();

private:
  Meter_change_req* meter_req_l_;
};

#endif // METER_PERFOMER_HH
