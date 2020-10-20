/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2020 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "std-vector.hh"

#include <iostream>

#define YAFFUT_MAIN
#include "yaffut.hh"

using std::vector;

template<typename T>
void
print (vector<T> v)
{
  for (vsize i = 0; i < v.size (); i++)
    std::cout << "v[" << i << "] = " << v[i] << std::endl;
  std::cout << std::endl;
}

FUNC (vector_erase)
{
  vector<int> v;
  v.push_back (0);
  v.push_back (1);
  EQUAL (v.size (), vsize (2));
  v.erase (v.begin () + 1);
  EQUAL (v.size (), vsize (1));
  EQUAL (v.back (), 0);

  v.push_back (1);
  EQUAL (v.size (), vsize (2));
  v.erase (v.begin () + 0);
  EQUAL (v.size (), vsize (1));
  EQUAL (v.back (), 1);
}

FUNC (vector_sorting)
{
  vector<int> v;
  v.push_back (2);
  v.push_back (1);
  v.push_back (0);
  //sort (v.begin (), v.end ());
  vector_sort (v, std::less<int> ());
  EQUAL (v[0], 0);
  EQUAL (v[1], 1);
  EQUAL (v[2], 2);
}

FUNC (vector_insert)
{
  vector<int> v;
  v.push_back (0);
  v.insert (v.begin (), 1);
  EQUAL (v[0], 1);
  v.insert (v.end (), 2);
  EQUAL (v.back (), 2);
  vector<int> u;
  u.insert (u.begin (), v.begin (), v.end ());
  EQUAL (u.size (), v.size ());
  u.clear ();
  u.insert (u.end (), v.begin (), v.end ());
  EQUAL (u.size (), v.size ());
  u.clear ();
}

FUNC (parray_concat)
{
  vector<int *> u, v;
  int a[5] = { 0, 1, 2, 3, 4 };
  u.push_back (&a[0]);
  u.push_back (&a[1]);
  u.push_back (&a[2]);
  v.push_back (&a[3]);
  v.push_back (&a[4]);
  concat (u, v);
  EQUAL (u[0], &a[0]);
  EQUAL (u[1], &a[1]);
  EQUAL (u[2], &a[2]);
  EQUAL (u[3], &a[3]);
  EQUAL (u[4], &a[4]);
  EQUAL (u.size (), vsize (5));
  concat (u, v);
  EQUAL (u.size (), vsize (7));

  u.clear ();
  v.clear ();
  v.push_back (&a[0]);
  v.push_back (&a[1]);
  v.push_back (&a[2]);
  v.push_back (&a[3]);
  v.push_back (&a[4]);
  concat (u, v);
  EQUAL (u[0], &a[0]);
  EQUAL (u[1], &a[1]);
  EQUAL (u[2], &a[2]);
  EQUAL (u[3], &a[3]);
  EQUAL (u[4], &a[4]);
  EQUAL (u.size (), vsize (5));
}

FUNC (parray_uniq)
{
  vector<int> v;
  v.push_back (0);
  v.push_back (1);
  v.push_back (0);
  vector_sort (v, std::less<int> ());
  uniq (v);
  EQUAL (v.size (), vsize (2));
}

FUNC (vector_search)
{
  vector<int> v;
  v.push_back (0);
  v.push_back (1);
  v.push_back (2);
  vsize i = binary_search (v, 1, std::less<int> ());
  EQUAL (i, vsize (1));
}
