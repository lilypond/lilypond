/*
  performer-group-performer.cc -- implement Performer_group_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
               Jan Nieuwenhuizen <jan@digicash.com>
 */

#include "performer-group-performer.hh"

#include "debug.hh"

IMPLEMENT_IS_TYPE_B2(Performer_group_performer,Performer, Translator_group);
ADD_THIS_TRANSLATOR(Performer_group_performer);

