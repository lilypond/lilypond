/*
  acceptor.cc -- implement Acceptor

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "acceptor.hh"
Acceptor::Acceptor()
{
    iterator_count_  = 0;
}

IMPLEMENT_STATIC_NAME(Acceptor);
IMPLEMENT_IS_TYPE_B(Acceptor);
