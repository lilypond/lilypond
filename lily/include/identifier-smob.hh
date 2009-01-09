/*
  identifier-smob.hh -- declare identifier smob.

  source file of the GNU LilyPond music typesetter

  (c) 2002--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef IDENTIFIER_SMOB_HH
#define IDENTIFIER_SMOB_HH

#include "lily-guile.hh"

SCM package_identifier (SCM);
bool identifier_smob_p (SCM);
SCM unpack_identifier (SCM);

#endif /* IDENTIFIER_SMOB_HH */

