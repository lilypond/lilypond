/*
  voice-group-performer.hh -- declare Voice_group_performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef VOICE_GROUP_PERFORMER_HH
#define VOICE_GROUP_PERFORMER_HH

#include "performer-group-performer.hh"

#if 0
class Voice_performer_group_performer : public Performer_group_performer {
public:
    Voice_performer_

};

#endif

class Voice_group_performer : public Performer_group_performer {
public:
    NAME_MEMBERS();

    Voice_group_performer();
    ~Voice_group_performer();

private:
};


#if 0
class Voice_performer : 
	public Performer_group_performer, public Interpreter 
{
    
};
#endif

#endif // VOICE_GROUP_PERFORMER_HH
