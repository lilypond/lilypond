/*
  performer-group-performer.hh -- declare Performer_group_performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef PERFORMER_GROUP_PERFORMER_HH
#define PERFORMER_GROUP_PERFORMER_HH

#include "lily-proto.hh"
#include "parray.hh"
#include "plist.hh"
#include "performer.hh"
#include "translator.hh"

/**
  Group a number of performers. Usually delegates everything to its contents.
*/

class Performer_group_performer : public Performer, public virtual Translator {
    Pointer_list<Performer*> perf_p_list_;
    
    Link_array<Performer_group_performer> group_l_arr_;
    Link_array<Performer> nongroup_l_arr_;
    
public:
    Input_performer* iperf_l_;
    
    virtual bool try_request( Request* req_l );
    virtual void add( Performer* perf_p );
};

#endif // PERFORMER_GROUP_PERFORMER_HH

