/*
  voice-group-performer.hh -- declare Voice_group_performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef VOICE_GROUP_PERFORMER_HH
#define VOICE_GROUP_PERFORMER_HH

#include "performer-group-performer.hh"

class Voice_performer_group_performer : public Performer_group_performer {

};

#if 0
class Voice_performer : 
	public Performer_group_performer, public Interpreter 
{
    
};
#endif

#endif // VOICE_GROUP_PERFORMER_HH
