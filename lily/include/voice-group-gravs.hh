/*
  voice-group-gravs.hh -- declare Voice_group_engravers

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef VOICEGROUPGRAVS_HH
#define VOICEGROUPGRAVS_HH

#include "engraver-group.hh"

/**
  A group of voices which share certain characteristics (such as beams.).
 */
class Voice_group_engravers  : public Engraver_group_engraver {
    Moment termination_mom_;
    Direction dir_;

protected:
    virtual void do_print() const;
    virtual Scalar get_feature (String);
    virtual bool do_try_request (Request*);
public:
    
    
    DECLARE_MY_RUNTIME_TYPEINFO;
    static bool static_acceptable_request_b (Request*);
    Voice_group_engravers();
};
#endif // VOICEGROUPGRAVS_HH
