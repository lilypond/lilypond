/*
  global-translator.hh -- declare Global_translator

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef GLOBAL_TRANSLATOR_HH
#define GLOBAL_TRANSLATOR_HH

#include "translator.hh"
#include "pqueue.hh"

class Global_translator : public virtual Translator {
    PQueue<Moment> extra_mom_pq_;
    Moment last_mom_;
public:
    Score *score_l_;
    Global_translator();
    int moments_left_i()const;
    void modify_next(Moment&);
    void add_moment_to_process(Moment);

    virtual void set_score(Score*);
    virtual void prepare(Moment);
    virtual void process() {}
    virtual void finish() {}
    
protected:
    virtual Global_translator *global_l() { return this; }
    virtual int depth_i() const;
    virtual Translator *ancestor_l(int);
};



#endif // Global_translator_HH
