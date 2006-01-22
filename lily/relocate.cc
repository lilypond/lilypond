/*
  relocate.cc -- implement relocation based on argv0

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "relocate.hh"

#include "config.hh"

#include <sys/stat.h>
#include <unistd.h>

#if HAVE_GETTEXT
#include <libintl.h>
#endif

#include "file-name.hh"
#include "file-path.hh"
#include "lily-guile.hh"
#include "lily-version.hh"
#include "main.hh"
#include "version.hh"
#include "warn.hh"


int
sane_putenv (char const *key, String value, bool overwrite)
{
  if (overwrite || !getenv (key))
    {
      String combine = String (key) + "=" + value;
      char *s = strdup (combine.to_str0 ());
      return putenv (s);
    }
  
  return -1;
}

static int
set_env_file (char const *key, String value, bool overwrite = false)
{
  if (is_file (value))
    return sane_putenv (key, value, overwrite);
  else if (be_verbose_global)
    warning (_f ("no such file: %s for %s", value, key));
  return -1;
}

static int
set_env_dir (char const *key, String value)
{
  if (is_dir (value))
    return sane_putenv (key, value, false);
  else if (be_verbose_global)
    warning (_f ("no such directory: %s for %s", value, key));
  return -1;
}

static int
prepend_env_path (char const *key, String value)
{
  if (is_dir (value))
    {
      if (be_verbose_global)
	progress_indication (_f ("%s=%s\n", key, value.to_str0 ())); 

      if (char const *cur = getenv (key))
	value += to_string (PATHSEP) + cur;

      return sane_putenv (key, value.to_str0 (), true);
    }
  else if (be_verbose_global)
    warning (_f ("no such directory: %s for %s", value, key));
  return -1;
}

String
dir_name (String const file_name)
{
  String s = file_name;
  s.substitute ('\\', '/');
  int n = s.length ();
  if (n && s[n - 1] == '/')
    s[n - 1] = 0;
  s = s.left_string (s.index_last ('/'));
  return s;
}

#ifdef __MINGW32__
#include <winbase.h>
#endif

void
prefix_relocation (String prefix)
{
  if (be_verbose_global)
    warning (_f ("Relocation: compile prefix=%s, new prefix=%s",
		 prefix_directory,
		 prefix.to_str0 ()));
  
  String bindir = prefix + "/bin";
  String datadir = prefix + "/share";
  String localedir = datadir + "/locale";
  String lilypond_datadir = datadir + "/lilypond/" TOPLEVEL_VERSION;

  if (is_dir (lilypond_datadir))
    prefix_directory = lilypond_datadir;

#if HAVE_GETTEXT
  if (is_dir (localedir))
    bindtextdomain ("lilypond", localedir.to_str0 ());
#endif

  prepend_env_path ("PATH", bindir);
}

void
framework_relocation (String prefix)
{
  if (be_verbose_global)
    warning (_f ("Relocation: framework_prefix=%s", prefix));

  String bindir = prefix + "/bin";
  String datadir = prefix + "/share";
  String libdir = prefix + "/lib";
  String sysconfdir = prefix + "/etc";

  /* need otherwise dynamic .so's aren't found.   */
  prepend_env_path ("DYLD_LIBRARY_PATH", libdir);
  
  set_env_file ("FONTCONFIG_FILE", sysconfdir + "/fonts/fonts.conf", true);
  set_env_dir ("FONTCONFIG_PATH", sysconfdir + "/fonts");

#ifdef __MINGW32__
  char font_dir[PATH_MAX];
  ExpandEnvironmentStrings ("%windir%/fonts", font_dir, sizeof (font_dir));
  prepend_env_path ("GS_FONTPATH", font_dir);
#endif

  /* FIXME: *cough* 8.15 *cough* */
  prepend_env_path ("GS_FONTPATH", datadir + "/ghostscript/8.15/fonts");
  prepend_env_path ("GS_LIB", datadir + "/ghostscript/8.15/Resource");
  prepend_env_path ("GS_LIB", datadir + "/ghostscript/8.15/lib");

  prepend_env_path ("GS_FONTPATH", datadir + "/gs/fonts");
  prepend_env_path ("GS_LIB", datadir + "/gs/Resource");
  prepend_env_path ("GS_LIB", datadir + "/gs/lib");

  prepend_env_path ("GUILE_LOAD_PATH", datadir
		    + to_string ("/guile/%d.%d",
				 SCM_MAJOR_VERSION, SCM_MINOR_VERSION));

  set_env_file ("PANGO_RC_FILE", sysconfdir + "/pango/pangorc");
  set_env_dir ("PANGO_PREFIX", prefix);
  
  prepend_env_path ("PATH", bindir);
}

String
get_working_directory ()
{
  char cwd[PATH_MAX];
  getcwd (cwd, PATH_MAX);

  return String (cwd);
}

void
setup_paths (char const *argv0_ptr)
{
  File_name argv0_filename (argv0_ptr);
  
  prefix_directory = LILYPOND_DATADIR;
  if (relocate_binary
      && getenv ("LILYPOND_RELOCATE_PREFIX"))
    {
      String prefix = getenv ("LILYPOND_RELOCATE_PREFIX");
#ifdef __MINGW32__
      /* Normalize file name.  */
      prefix = File_name (prefix).to_string ().get_copy_str0 ();
#endif /* __MINGW32__ */
      prefix_relocation (prefix);
      String bindir = prefix + "/bin";
      framework_relocation (bindir + "/" FRAMEWORKDIR);
    }
  else if (relocate_binary)
    {
      String argv0_abs;
      if (argv0_filename.is_absolute ())
	{
	  argv0_abs = argv0_filename.to_string ();
	  if (be_verbose_global)
	    warning (_f ("Relocation: is absolute: argv0=%s", argv0_ptr));
	}
      else if (argv0_filename.dir_.length ())
	{
	  argv0_abs = get_working_directory ()
	    + "/" + String (argv0_filename.to_string ());
	  if (be_verbose_global)
	    warning (_f ("Relocation: from cwd: argv0=%s", argv0_ptr));
	}
      else
	{
	  /* Find absolute ARGV0 name, using PATH.  */
	  File_path path;
	  path.parse_path (getenv ("PATH"));

	  if (be_verbose_global)
	    warning (_f ("Relocation: from PATH=%s\nargv0=%s",
			 path.to_string ().c_str (), argv0_ptr));

#ifndef __MINGW32__
	  argv0_abs = path.find (argv0_filename.to_string ());
#else /* __MINGW32__ */
	  char const *ext[] = {"exe", "", 0 };
	  argv0_abs = path.find (argv0_filename.to_string (), ext);
#endif /* __MINGW32__ */

	  if (argv0_abs.is_empty ())
	    programming_error ("can't find absolute argv0.");
	}

      String bindir = dir_name (argv0_abs);
      String argv0_prefix = dir_name (bindir);
      String compile_prefix = dir_name (dir_name (dir_name (prefix_directory)));
      if (argv0_prefix != compile_prefix)
	prefix_relocation (argv0_prefix);
      if (argv0_prefix != compile_prefix || String (FRAMEWORKDIR) != "..")
	framework_relocation (bindir + "/" FRAMEWORKDIR);
    }

  /* FIXME: use LILYPOND_DATADIR.  */
  if (char const *env = getenv ("LILYPONDPREFIX"))
    {
#ifdef __MINGW32__
      /* Normalize file name.  */
      env = File_name (env).to_string ().get_copy_str0 ();
#endif
      prefix_directory = env;
    }

  global_path.append ("");


  /*
    When running from build dir, a full LILYPOND_PREFIX is set-up at

        $(OUTBASE)/share/lilypond/TOPLEVEL_VERSION

     This historical hack will allow the shorthand

        LILYPONDPREFIX=out lily/out/lilypond ...

  */
  
  struct stat statbuf;
  String build_prefix = prefix_directory + "/share/lilypond/" TOPLEVEL_VERSION;
  if (stat (build_prefix.to_str0 (), &statbuf) == 0)
    prefix_directory = build_prefix;

  
  /* Adding mf/out make lilypond unchanged source directory, when setting
     LILYPONDPREFIX to lilypond-x.y.z */
  char *suffixes[] = {"ly", "ps", "scm", 0 };

  
  Array<String> dirs;
  for (char **s = suffixes; *s; s++)
    {
      String path = prefix_directory + to_string ('/') + String (*s);
      dirs.push (path);
    }


  dirs.push (prefix_directory + "/fonts/otf/");
  dirs.push (prefix_directory + "/fonts/type1/");
  dirs.push (prefix_directory + "/fonts/svg/");
  
  for (int i = 0; i < dirs.size (); i++)
    global_path.prepend (dirs[i]);
}
