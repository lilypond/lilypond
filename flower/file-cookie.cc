
#include <cassert>
#include <cstdio>
using namespace std;

#include "memory-stream.hh"

extern "C" {

  bool
  is_memory_stream (void *foo)
  {
    Memory_out_stream *cookie = (Memory_out_stream *) foo;
    return dynamic_cast<Memory_out_stream *> (cookie);
  }

  void *
  lily_fopencookie (void *cookie,
		    char const * /* modes */,
		    lily_cookie_io_functions_t /* io_funcs */)
  {
    assert (is_memory_stream (cookie));
    return (FILE *) cookie;
  }

  int
  lily_cookie_fclose (void *file)
  {
    assert (is_memory_stream (file));
    return Memory_out_stream::cleaner (file);
  }

  int
  lily_cookie_fprintf (void *file, char const *format, ...)
  {
    assert (is_memory_stream (file));
    va_list ap;
    va_start (ap, format);

    static char buf[65536];
    int i = vsnprintf (buf, sizeof (buf), format, ap);
    if (i == -1 || (unsigned) i > sizeof (buf))
      assert (false);
    va_end (ap);
    return Memory_out_stream::writer (file, buf, i);
  }

  int
  lily_cookie_putc (int c, void *file)
  {
    assert (is_memory_stream (file));
    char buf[1];
    buf[0] = (char) c;
    return Memory_out_stream::writer (file, buf, 1);
  }
} /* extern C */
