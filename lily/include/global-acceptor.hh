/*
  global-acceptor.hh -- declare Global_acceptor

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef GLOBAL_ACCEPTOR_HH
#define GLOBAL_ACCEPTOR_HH

#include "acceptor.hh"
#include "pqueue.hh"

class Global_acceptor : public virtual Acceptor {
    PQueue<Moment> extra_mom_pq_;
    Moment last_mom_;
public:
    Score *score_l_;
    Global_acceptor();
    int moments_left_i()const;
    void modify_next(Moment&);
    void add_moment_to_process(Moment);

    virtual void set_score(Score*);
    virtual void prepare(Moment);
    virtual void process() {}
    virtual void finish() {}
    
protected:
    virtual int depth_i() const;
    virtual Acceptor *ancestor_l(int);
};



#endif // GLOBAL_ACCEPTOR_HH
