/*
  silent-performer.hh -- declare Silent_performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef SILENT_PERFORMER_HH
#define SILENT_PERFORMER_HH

#include "performer.hh"

/**
*/

class Silent_performer : public Performer {
public:
    NAME_MEMBERS();

    Silent_performer();
    ~Silent_performer();

    virtual bool try_request( Request *req_l ) ;
};

#endif // SILENT_PERFORMER_HH
