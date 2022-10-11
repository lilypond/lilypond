/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Jan Nieuwenhuizen <janneke@gnu.org>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef MINGW_COMPATIBILITY_HH
#define MINGW_COMPATIBILITY_HH

#if __MINGW32__

/* Mingw uses invalid names for typedefs and defines.  Not yet
   investigated whether this is a mingw bug or a windows bug (ie,
   mingw compatibility feature), also not reported yet.  */

#ifdef CHAR
#define LILY_CHAR CHAR
#undef CHAR
#endif
#define CHAR MINGW_INFRINGES_ON_OUR_NAMESPACE_USING_CHAR

#ifdef CONTEXT
#define LILY_CONTEXT CONTEXT
#undef CONTEXT
#endif
#define CONTEXT MINGW_INFRINGES_ON_OUR_NAMESPACE_USING_CONTEXT

#ifdef DATADIR
#define LILY_DATADIR DATADIR
#undef DATADIR
#endif
#define DATADIR MINGW_INFRINGES_ON_OUR_NAMESPACE_USING_DATADIR

#ifdef RELATIVE
#define LILY_RELATIVE RELATIVE
#undef RELATIVE
#endif
#define RELATIVE MINGW_INFRINGES_ON_OUR_NAMESPACE_USING_RELATIVE

#ifdef THIS
#define LILY_THIS THIS
#undef THIS
#endif
#define THIS MINGW_INFRINGES_ON_OUR_NAMESPACE_USING_THIS

//#include <winsock2.h>

#if defined(__MINGW32__) && !defined(STATIC)
#define SCM_IMPORT 1
#endif

#include <libguile.h>

#undef CHAR
#ifdef LILY_CHAR
#define CHAR LILY_CHAR
#endif

#undef CONTEXT
#ifdef LILY_CONTEXT
#define CONTEXT LILY_CONTEXT
#endif
#undef CONTEXT

#undef DATADIR
#ifdef LILY_DATADIR
#define DATADIR LILY_DATADIR
#endif
#undef DATADIR

#undef RELATIVE
#ifdef LILY_RELATIVE
#define RELATIVE LILY_RELATIVE
#endif

#undef THIS
#ifdef LILY_THIS
#define THIS LILY_THIS
#endif

#endif /* __MINGW__ */

#endif /* MINGW_COMPATIBILITY_HH */
