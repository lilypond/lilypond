/*
  voice-group-performer.cc -- implement Voice_group_performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */


#include "voice-group-performer.hh"
#include "translator.hh"
#include "input-translator.hh"
#include "debug.hh"

//IMPLEMENT_IS_TYPE_B2(Voice_group_performer,Performer, Translator);
IMPLEMENT_IS_TYPE_B1(Voice_group_performer,Performer_group_performer);
IMPLEMENT_STATIC_NAME(Voice_group_performer);
ADD_THIS_PERFORMER(Voice_group_performer);

Voice_group_performer::Voice_group_performer()
{
}

Voice_group_performer::~Voice_group_performer()
{
}

