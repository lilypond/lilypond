/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef VIRTUAL_METHODS_HH
#define VIRTUAL_METHODS_HH

#include <typeinfo>
using namespace std;

#define classname(class_ptr) demangle_classname (typeid (* (class_ptr)))

char const *
demangle_classname (type_info const &);

/*

Virtual copy constructor.  Make up for C++'s lack of a standard
factory or clone () function.  Usage:

class Foo : Baseclass
{
VIRTUAL_COPY_CONSTRUCTOR (Baseclass, Foo);
};

*/

#define DECLARE_CLASSNAME(name) \
  virtual const char *class_name () const {	\
    return #name; \
}

#define VIRTUAL_COPY_CONSTRUCTOR(Base, name)	\
  DECLARE_CLASSNAME(name);\
  virtual Base *clone () const			\
  {						\
    return new name (*this);			\
  }

#endif /* VIRTUAL_METHODS_HH */
