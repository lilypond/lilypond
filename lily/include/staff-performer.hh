/*
  staff-performer.hh -- declare Staff_performer

  (c) 1996,  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef STAFF_PERFORMER_HH
#define STAFF_PERFORMER_HH

#include "performer-group-performer.hh"

/** Perform a staff. Individual notes should have their instrument
  (staff-wide) set, so we override play()

  */
class Staff_performer : public Performer_group_performer 
{
public:
  TRANSLATOR_CLONE(Staff_performer);
  DECLARE_MY_RUNTIME_TYPEINFO;

  Staff_performer ();
  ~Staff_performer ();

  String new_instrument_str ();
  String instrument_str_;

protected:
  virtual void play (Audio_element* p);
  virtual void do_removal_processing ();
  virtual void do_creation_processing ();
  virtual void do_process_requests ();

private:
  Audio_staff* audio_staff_p_;
};

#endif // STAFF_PERFORMER_HH
