/*
  music-iterator.hh -- declare {Music,Chord,Voice}_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef MUSIC_ITERATOR_HH
#define MUSIC_ITERATOR_HH

#include "lily-proto.hh"
#include "plist.hh"
#include "moment.hh"

class Music_iterator {
protected:
    bool first_b_;
    virtual void do_print()const;
    
public:
    Music_iterator *daddy_iter_l_;
    NAME_MEMBERS();
    Acceptor *report_to_l_;
    
    static Music_iterator* static_get_iterator_p(Music*,Acceptor*);
    Music_iterator* get_iterator_p(Music*)const;
    void set_acceptor(Acceptor*);
    Music_iterator();
    virtual void next(Moment until);
    virtual Moment next_moment()const;
    virtual bool ok()const;
    virtual ~Music_iterator();
    virtual void construct_children();
    void print()const;
    virtual Acceptor * get_req_acceptor_l();
};

// duh.
class Request_iterator : public Music_iterator {
    Request  *req_l_;
    bool last_b_;
public:
    Request_iterator(Request const *);
    NAME_MEMBERS();
protected:
    virtual bool ok() const;
    virtual Moment next_moment() const;
    
    virtual void do_print()const;
    virtual void next(Moment);
};

class Change_iterator : public Music_iterator {
    Change_reg * change_l_;
public:
     NAME_MEMBERS();
    virtual void next(Moment);
    Change_iterator(Change_reg*);
};

class Chord_iterator : public Music_iterator
{
    const Chord *chord_C_;
    Pointer_list<Music_iterator*> children_p_list_;
public:
    Chord_iterator(Chord const*);
    NAME_MEMBERS();
protected:
    virtual void do_print()const;
    virtual void construct_children();
    virtual void next(Moment);
    virtual Moment next_moment()const;
    virtual bool ok()const;
};

class Voice_element_iterator : public Chord_iterator {

protected:
    virtual void construct_children();
public:
    Voice_element_iterator(Voice_element*);
    NAME_MEMBERS();
};


class Voice_iterator :  private PCursor<Music*>, public Music_iterator
{
    Moment here_mom_;
    const Voice * voice_C_;
    Music_iterator * iter_p_;
    void next_element();
public:
    Voice_iterator(Voice const*);
    NAME_MEMBERS();
protected:
    virtual void do_print()const;
    virtual void construct_children();
    ~Voice_iterator();    
    virtual void next(Moment);
    virtual Moment next_moment()const;
    virtual bool ok()const;
};

#endif // MUSIC_ITERATOR_HH
