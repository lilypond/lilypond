/*
  music-iterator.hh -- declare {Music,Chord,Voice}_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef MUSIC_ITERATOR_HH
#define MUSIC_ITERATOR_HH

#include "lily-proto.hh"
#include "plist.hh"
#include "varray.hh"
#include "moment.hh"

class Music_iterator {
   Array<Translator *>report_to_l_arr_;
   void push_translator (Translator*);
    void pop_translator();
protected:
    bool first_b_;
    virtual void do_print()const;
    
    virtual Translator * get_req_translator_l();
    Music_iterator* get_iterator_p (Music*)const;
    void set_translator (Translator*);
    Music_iterator *daddy_iter_l_;
    
public:
    Translator *report_to_l()const;
    DECLARE_MY_RUNTIME_TYPEINFO;
 
    static Music_iterator* static_get_iterator_p (Music*,Translator*);
    Music_iterator();
    
    virtual void process_and_next (Moment until);
    virtual Moment next_moment()const;
    virtual bool ok()const;
    virtual ~Music_iterator();
    virtual void construct_children();
    void print()const;
};


class Chord_iterator : public Music_iterator
{
    const Chord *chord_C_;
    Pointer_list<Music_iterator*> children_p_list_;
public:
    ~Chord_iterator();
    Chord_iterator (Chord const*);
    DECLARE_MY_RUNTIME_TYPEINFO;
protected:
    virtual void do_print()const;
    virtual void construct_children();
    virtual void process_and_next (Moment);
    virtual Moment next_moment()const;
    virtual bool ok()const;
};

class Request_chord_iterator : public Music_iterator {
    const Request_chord * elt_l_;
    Moment elt_duration_;
    bool last_b_;
public:
    Request_chord_iterator (Request_chord*);
    DECLARE_MY_RUNTIME_TYPEINFO;

protected:
    virtual void process_and_next (Moment);
    virtual Moment next_moment()const;
    virtual void construct_children();
    virtual bool ok()const;
    virtual void do_print()const;
};


class Voice_iterator :  private PCursor<Music*>, public Music_iterator
{
    Moment here_mom_;
    const Voice * voice_C_;
    Music_iterator * iter_p_;
    void start_next_element();
    void leave_element();
    void set_voice_translator();
    
public:
    Voice_iterator (Voice const*);
    DECLARE_MY_RUNTIME_TYPEINFO;
protected:
    virtual void do_print()const;
    virtual void construct_children();
    ~Voice_iterator();    
    virtual void process_and_next (Moment);
    virtual Moment next_moment()const;
    virtual bool ok()const;
};

#endif // MUSIC_ITERATOR_HH
