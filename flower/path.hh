#ifndef PATH_HH
#define PATH_HH
#include "string.hh"
#include "varray.hh"


///   searching directory for file.
/**

   Abstraction of PATH variable. An interface for searching input files.
   Search a number of dirs for a file.

   Should use kpathsea?
   
*/

class File_path : private Array<String>
{
public:
    /// locate a file in the search path
    String find(String nm);

    /// construct using prefix. Normally argv[0].
    File_path(String);

    /// add to end of path.
    Array<String>::push;
    void add(String str) { push(str); }
};
/// split path into its components
void split_path(String path, String &drive, String &dirs, String &filebase, String &extension);

#endif
