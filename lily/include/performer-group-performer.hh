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
public:
    NAME_MEMBERS();
    Input_translator* itrans_l_;

    Performer_group_performer();
    ~Performer_group_performer();

    virtual void add( Performer* perf_p );
    virtual void print()const { Performer::print() ; }
    virtual Moment get_mom() const;
    virtual bool do_try_request( Request* req_l );
    virtual bool try_request(Request*r) { return Performer::try_request(r) ; }

protected:

    virtual Translator* find_get_translator_l( String name, String id );
    virtual Translator* get_default_interpreter();

    Translator * ancestor_l( int l = 1 );
    virtual int depth_i() const;

    virtual void process_requests();
    virtual void do_removal_processing();
    virtual void do_creation_processing();

    bool is_bottom_performer_b() const;
    virtual Performer_group_performer* find_performer_l( String name, String id );
    
    virtual void do_print()const;
private:
    Pointer_list<Performer*> perf_p_list_;
    
    Link_array<Performer_group_performer> group_l_arr_;
    Link_array<Performer> nongroup_l_arr_;
};

#endif // PERFORMER_GROUP_PERFORMER_HH

