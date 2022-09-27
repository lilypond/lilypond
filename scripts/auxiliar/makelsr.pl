#!/usr/bin/env perl

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2019--2022 Werner Lemberg <wl@gnu.org>
#
# LilyPond is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


# This script needs a few modules that are not core modules (tested
# with Perl version 5.36):
#
#   File::Which
#   IPC::Run3
#   MySQL::Dump::Parser::XS
#   Pandoc
#   Parallel::ForkManager
#
# Either install missing modules using your package manager (if
# available) or use `cpanm` to download, compile, and install them.


use 5.14.0; # We use `s///r`.
use strict;
use warnings;

use Encode qw(decode);

use File::Spec::Functions qw(:ALL); # For file name manipulation.
use File::Which; # For checking the existence of `convert-ly`.
use Getopt::Long qw(:config no_ignore_case
                            bundling_values); # For option handling.
use HTTP::Tiny; # For downloading.
use IO::Zlib; # For uncompressing `.gz` files.
use IPC::Run3; # For communication with `convert-ly`.
use MySQL::Dump::Parser::XS 0.04; # Accessing MySQL database dumps.
use Pandoc; # Accessing `pandoc`.
use Parallel::ForkManager; # For parallel processing.
use Time::Piece; # This extends `localtime`.
use Time::Seconds; # For `ONE_DAY` constant.


# Don't buffer 'stdout'.
$| = 1;


# Function:
#   Show help text.
#
# Arguments:
#   $ret    Exit code.
#
sub show_help {
  my ($ret) = @_;

  my $help = <<'EOT';
Usage: makelsr.pl [OPTION]...
Use the LSR database to regenerate LilyPond's 'snippets' directory.

  -c, --no-convert       don't call 'convert-ly'
  -d, --dump=WHAT        if WHAT is a file name, use it as a local
                           dump file instead of downloading the LSR
                           database
                         if WHAT is 'no', don't use a dump file (this
                           implies '--no-lsr')
  -f, --no-snippet-list  don't update snippet list files
  -h, --help             display this help and exit
  -j, --jobs=N           use N jobs for creating snippet files in
                           parallel (default: 1)
  -l, --no-lsr           don't update snippets from the LSR database
  -n, --no-new           don't handle snippets from the 'new'
                           subdirectory
  -N, --new              shorthand for '--dump=no --no-lsr' to quickly
                           handle new snippets only; this also
                           suppresses 'missing tag' warnings
  -p, --path=DIR         directory where to look for 'convert-ly'
                           script
  -s, --top-source=DIR   directory where to look for subdirectory
                           'Documentation/snippets' (default: '.')
  -t, --textdump=FILE    create a plain text dump file of all snippets
                           (omitting binary fields) in the LSR
                           database dump; ignored if '--dump=no'
  -T, --textdump-only=FILE
                         shorthand for '--no-convert --no-lsr
                         --no-new --no-snippet-list --textdump=FILE'
  -V, --verbose          be verbose

Environment variables:
  LILYPOND_BUILD_DIR     top of the LilyPond build directory tree
  TMPDIR, TEMP (only Windows), TMP (only Windows)
                         directory for temporary files

This script performs the following actions; use the above options to
modify them.

* Download the daily LSR database dump from
  'https://lsr.di.unimi.it/download/' into a directory for temporary
  files.

* Delete all snippet and snippet list files in directory
  'Documentation/snippets' (but not in 'Documentation/snippets/new').

* Extract all snippets from the LSR database that have the 'docs' tag
  set, convert their documentation parts from HTML to Texinfo with the
  'pandoc' program, run the script 'convert-ly' to update their
  LilyPond code parts to current syntax, and store them in
  'Documentation/snippets'.

  The snippet file names are derived from the snippet titles.
  Additionally, various files named 'winds.snippet-list' or
  'connecting-notes.snippet-list' are created that list the snippets
  grouped by tags assigned in the database.

* Convert all snippet files in 'Documentation/snippets/new' with
  'convert-ly' and output them to 'Documentation/snippets', possibly
  overwriting existing files.

Note that 'approved documentation snippets' is a subset of the
available snippets in the LSR.

The `convert-ly` script is searched in the following order; the first
successful hit wins.

* In the directory given with option '--path'.
* In directory '$LILYPOND_BUILD_DIR/out/bin'.
* In directory './build/out/bin'.
* In $PATH.
EOT

  print $help;
  exit $ret;
}


# Option handling.
my $dump = "";
my $help = 0;
my $jobs = 1;
my $new = 0;
my $no_convert = 0;
my $no_lsr = 0;
my $no_new = 0;
my $no_snippet_list = 0;
my $path = "";
my $textdump = "";
my $textdump_only = "";
my $top_source = ".";
my $verbose = 0;

GetOptions(
  "dump|d=s" => \$dump,
  "help|h" => \$help,
  "jobs|j=i" => \$jobs,
  "new|N" => \$new,
  "no-convert|c"  => \$no_convert,
  "no-lsr|l" => \$no_lsr,
  "no-new|n"  => \$no_new,
  "no-snippet-list|f" => \$no_snippet_list,
  "path|p=s" => \$path,
  "textdump|t=s" => \$textdump,
  "textdump-only|T=s" => \$textdump_only,
  "top-source|s=s" => \$top_source,
  "verbose|V" => \$verbose,
  ) || show_help(2);
show_help(1) if $help;

if ($new) {
  $dump = "no" if !$dump;
  $no_lsr = 1;
}

if ($textdump_only) {
  $no_convert = 1;
  $no_lsr = 1;
  $no_new = 1;
  $no_snippet_list = 1;
  $textdump = $textdump_only;
}

# Check availability of external programs.
unless ($no_lsr) {
  pandoc or die "error: 'pandoc' executable not found\n";
}

my $CONVERT_LY = "";
unless ($no_convert) {
  if ($path) {
    $CONVERT_LY = catfile($path, "convert-ly");
    if (!-x $CONVERT_LY) {
      warn "warning: Option '--path' set"
           . " but can't use '$CONVERT_LY'\n";
      $CONVERT_LY = "";
    }
  }
  if (!$CONVERT_LY) {
    if (defined $ENV{LILYPOND_BUILD_DIR}) {
      $CONVERT_LY = catfile($ENV{LILYPOND_BUILD_DIR},
                            "out", "bin", "convert-ly");
      if (!-x $CONVERT_LY) {
        warn   "warning: Environment variable"
               . " \$LILYPOND_BUILD_DIR is set\n"
             . "  but can't use '$CONVERT_LY'\n";
        $CONVERT_LY = "";
      }
    }
    else {
      print "'LILYPOND_BUILD_DIR' environment variable not set\n"
        if $verbose;
    }
  }
  if (!$CONVERT_LY) {
    $CONVERT_LY = catfile("build", "out", "bin", "convert-ly");
    if (!-x $CONVERT_LY) {
      print "Can't use '$CONVERT_LY'\n" if $verbose;
      $CONVERT_LY = "";
    }
  }
  if (!$CONVERT_LY) {
    $CONVERT_LY = which "convert-ly";
    if (!defined $CONVERT_LY) {
      print "Can't find 'convert-ly' in path\n" if $verbose;
      die "error: 'convert-ly' script not found\n";
    }
  }

  print "Using '$CONVERT_LY'\n" if $verbose;
}


# Check the actual snippet directories.
my $snippet_dir = catdir("Documentation", "snippets");
my $new_snippet_dir = catdir($snippet_dir, "new");

unless ($no_lsr && $no_snippet_list && $no_new) {
  my @dirs;

  push @dirs, catdir($top_source, $snippet_dir);
  push @dirs, catdir($top_source, $new_snippet_dir) unless $no_new;

  for my $dir (@dirs) {
    die "error: No directory '$dir'\n" unless -d $dir;
    die "error: Directory '$dir' is not readable\n" unless -r $dir;
    die "error: Directory '$dir' is not writable\n" unless -w $dir;
  }
}


# Change to the top source directory for simplicity.
my $curr_dir = rel2abs(curdir);
chdir $top_source
  or die "error: Can't change to directory '$top_source'\n";


# Clean up.
unless ($no_lsr) {
  print "Removing old snippet files\n";
  while (my $f = glob(catfile($snippet_dir, "*.ly"))) {
    unlink $f or warn "warning: Can't remove file '$f': $!";
  }
}

unless ($no_snippet_list || $no_lsr) {
  print "Removing old snippet list files\n";
  while (my $f = glob(catfile($snippet_dir, "*.snippet-list"))) {
    unlink $f or warn "warning: Can't remove file '$f': $!";
  }
}


# Open database.
print "Getting LSR dump file...";
print "\n" if $verbose;

my $lsr_dump_gz = "";

if ($dump eq "no") {
  $no_lsr = 1;
  $textdump = "";
  print $verbose ? "  Dump file disabled\n"
                 : " no\n";
}
elsif ($dump) {
  $lsr_dump_gz = file_name_is_absolute($dump)
                   ? $dump
                   : catfile($curr_dir, $dump);
  print $verbose ? "  Using LSR dump file '$lsr_dump_gz'\n"
                 : " local\n";
}
else {
  # Download the LSR database, which can be found at
  #
  #   https://lsr.di.unimi.it/download/lsr-YYYY-MM-DD.mysqldump.gz
  #
  # We also try the dates of yesterday and tomorrow to accommodate for
  # different time zones.
  my $lsr_prefix = "https://lsr.di.unimi.it/download/";
  my $success = 0;

  my $today = localtime;

  for my $date ($today, $today + ONE_DAY, $today - ONE_DAY) {
    my $lsr_filename = sprintf "lsr-%s.mysqldump.gz", $date->ymd;

    $lsr_dump_gz = catfile(tmpdir, $lsr_filename);

    print   "  Downloading '$lsr_prefix$lsr_filename'\n"
          . "    to '$lsr_dump_gz' ..."
      if $verbose;

    my $response = HTTP::Tiny->new->mirror(
                     $lsr_prefix . $lsr_filename, $lsr_dump_gz);
    # Status 304 is 'Not Modified'.
    if ($response->{success} || $response->{status} == 304) {
      print " OK\n";
      $success = 1;
      last;
    }

    print   " failed:\n"
          . "    $response->{status} $response->{reason}\n"
      if $verbose;
  }

  if (!$success) {
    print " failed\n" unless $verbose;
    die "error: Can't download LSR database\n";
  }
}


my $parser;
my %tables;

if ($no_lsr && !$textdump) {
  # Manually set up the minimum of necessary infrastructure.
  push @{$tables{"tag"}} => ();
  push @{$tables{"snippet"}} => ();
}
else {
  my $lsr_dump = IO::Zlib->new($lsr_dump_gz, "rb")
    or die "error: Can't open file '$lsr_dump_gz': $!\n";

  # Parse input and store all table entries in the `tables` hash as
  # arrays.
  $parser = MySQL::Dump::Parser::XS->new;

  print "Parsing LSR database\n";
  while (my $line = <$lsr_dump>) {
    my @entries = $parser->parse($line);
    my $table_name = $parser->current_target_table();

    push @{$tables{$table_name}} => @entries if $table_name;
  }
}


# Function:
#   Convert tags to file names, similar to the code in the original
#   Java implementation used to extract LSR snippets.  We additionally
#   remove (simple) HTML tags.
#
# Arguments:
#   $s              String to be manipulated.
#   $is_doctitle    If set, handle `$s` as a LilyPond `doctitle`
#                   string.
#
sub convert_name {
  my ($s, $is_doctitle) = @_;

  # Remove HTML start and end tags.
  $s =~ s| < /? [a-zA-Z0-9]+ .*? > ||gx;

  # Double backslashes must be reduced to single ones in `doctitle`
  # strings.
  $s =~ s/\\\\/\\/g if $is_doctitle;

  # Translate some characters not generally allowed in file names.
  $s =~ tr[* /:<>?|_;\\]
          [+\-\-\-\-\-\-\-\-\-\-];

  # Remove some problematic characters entirely.
  $s =~ s/[()"']+//g;

  # Convert to lowercase.
  $s = lc($s);

  return $s;
}


# Access entries of the `tag` table and build a hash `ids` to map tag
# ID numbers onto the corresponding tag names.  Also build a hash set
# `tags` that collects the names of all tags.
my $tag_table = $tables{"tag"};
my %ids;
my %tags;

for my $entry (@{$tag_table}) {
  my $id = $entry->{"id"};
  # Tag names are also used for snippet list file names, thus the call
  # of `convert_name`.
  my $t = convert_name($entry->{"name"}, 0);

  $ids{$id} = $t;
  $tags{$t} = ();
}


# Function:
#   Subroutine for `sort`, ignoring a `.ly` file extension while
#   comparing file names.  This makes 'foo-bar-baz.ly' be sorted after
#   'foo-bar.ly'.
#
sub ly_sort {
  my ($aa, $bb) = map { s/\.ly$//r } ($a, $b);

  return $aa cmp $bb;
}


# Access entries of the snippet table.
my $snippet_table = $tables{"snippet"};

# A hash to access entries of the snippet table by file name.
my %snippets;

# The maximum of tags per entry in the LSR database.
use constant MAX_TAGS => 7;


# The next loop over all snippet entries does the following actions.
#
# * Add a new field `filename` to the snippet table, containing file
#   names derived from snippet titles.
# * Replace tag IDs with tag names.
# * Fill the `snippets` hash.
for my $entry (@{$snippet_table}) {
  if (!defined $entry->{"title"}) {
    my $id = $entry->{"id"};
    my $filename = "snippet-$id.ly";

    warn "warning: Snippet $id has no title;"
         . "using '$filename' as file name\n";

    $entry->{"filename"} = $filename;
  }
  else {
    $entry->{"filename"} = convert_name($entry->{"title"}, 0) . ".ly";
  }

  for my $idx (0 .. MAX_TAGS - 1) {
    my $tag_field = "id_tag${idx}_tag";

    if (defined $entry->{$tag_field}) {
      my $id = $entry->{$tag_field};

      $entry->{$tag_field} = $ids{$id};
    }
  }

  $snippets{$entry->{"filename"}} = $entry;
}


# Function:
#   Clean up data entries.
#
# Arguments:
#   $data         Data to be manipulated.
#   $is_string    If set, escape backslashes for usage as a LilyPond
#                 string.
#
# Return:
#   The manipulated data.
#
sub normalize_text {
  my ($data, $is_string) = @_;

  # Fix encoding; we always need UTF-8.
  $data = decode("UTF-8", $data);

  # Make line endings uniform.
  $data =~ s/(\015\012?|\012)/\n/g;

  # Backslashes must be doubled in LilyPond strings.
  $data =~ s/\\/\\\\/g if $is_string;

  # Remove trailing (horizontal) whitespace from every line.
  $data =~ s/\h+$//gm;

  # Remove leading and trailing empty lines.
  $data =~ s/^\n+//;
  $data =~ s/\n+$//;

  return $data;
}


# Write text dump of LSR database.
if ($textdump) {
  # We want to see constructed file names in the LSR text dump also, so
  # add a proper entry to the array of column names.
  my @column_names = $parser->columns("snippet");
  push @column_names => "filename";

  my $textdump_file = file_name_is_absolute($textdump)
                        ? $textdump
                        : catfile($curr_dir, $textdump);

  print "Writing LSR text dump";
  print " to file '$textdump_file'" if $verbose;
  print "\n";

  # Emit a text dump of all snippets (sorted by snippet ID).
  open(my $fh, ">:encoding(UTF-8)", $textdump_file)
    or die "error: Can't open file '$textdump_file': $!\n";

  for my $entry (sort { $a->{"id"} <=> $b->{"id"} }
                 @{$snippet_table}) {
    for my $name (@column_names) {
      # Ignore binary data.
      next if $name eq "image";
      next if $name eq "largeimage";

      # Ignore unset fields.
      next unless defined $entry->{$name};

      my $tag = "$name: ";
      print $fh $tag;

      my $data = normalize_text($entry->{$name}, 0);

      # Insert a prefix to indicate continuation lines for nicer
      # reading.
      my $prefix = " " x (length($tag) - 2) . "| ";
      my $n = 0;
      $data =~ s/^/$n++ ? "$prefix" : $&/gme; # Skip the first match.

      print $fh "$data\n";
    }

    print $fh "\n";
  }

  print $fh "END OF DUMP\n";

  close($fh) or warn "warning: Can't close file '$textdump_file': $!";
}


# Handle new snippets.  We expect them to have a structure similar to
# snippets derived from the LSR database.
unless ($no_new) {
  print "Examining snippets in subdirectory 'new'\n";
  opendir my $dh, $new_snippet_dir
    or die "error: Can't open directory '$new_snippet_dir': $!\n";

  for my $f (sort ly_sort readdir $dh) {
    next unless $f =~ /\.ly$/;
    my $filename = catfile($new_snippet_dir, $f);
    next unless -f $filename;

    # Read snippet (in raw mode; conversion to UTF-8 is done later
    # while writing the snippets).
    print "  $f\n" if $verbose;
    open(my $fh, "<:raw", $filename)
      or die "error: Can't open file '$filename': $!\n";
    read $fh, my $content, -s $fh;
    close($fh) or warn "warning: Can't close file '$filename': $!";

    # Split content into fields similar to entries in the LSR
    # database.
    my $version;
    my @lsrtags;
    my $texidoc;
    my $doctitle;
    my $code;

    # We use named groups `(?<name> ...)` in the regular expression,
    # also removing leading and trailing whitespace.  `(?: ...)` is
    # for clustering, `*?` is the non-greedy version of `*`, and
    # `(?s)` makes `.` match newlines, too.  Finally, `\A` and `\z`
    # match the start and the end of the string, respectively, while
    # `$` matches EOL because of the 'm' modifier of this regex.
    #
    # The hash called `+` holds all matches of named groups.
    if ($content =~
        /
          \A \s*

          \\version \s* " (?<version> [0-9.]+ ) " \s*

          \\header \s* { \s*

            lsrtags \s* = \s* " \s*
              (?<lsrtags> (?: \\" | [^"] )*? )
            \s* " \s*

            texidoc \s* = \s* "
              (?<texidoc> (?: \\" | [^"] )*? )
            \s* " \s*

            doctitle \s* = \s* "
              (?<doctitle> (?: \\" | [^"] )*? )
            \s* " \s*

          } \h* (?<invalid> .*? ) \h* $ \s*

          (?<code> (?s) .*? )

          \s* \z
        /xm) {
      $version = $+{version};
      @lsrtags = split /\s*,\s*/, $+{lsrtags};
      $texidoc = $+{texidoc};
      $doctitle = $+{doctitle};
      $code = $+{code};

      # Some data checks.

      # This is to catch '% begin verbatim', which gets automatically
      # inserted by this script.
      die <<"EOT"
error: In snippet '$filename',
  the closing bracket of the '\\header' block must be
  the last character on this line (found '$+{invalid}')
EOT
        if $+{invalid};

      # While the number of allowed tags per entry in the LSR database
      # is actually `MAX_TAGS`, one of them is 'docs', which is always
      # set for snippets imported into the LilyPond repository (and
      # thus not emitted).
      my $max = MAX_TAGS - 1;
      die   "error: More than $max LSR tags in snippet\n"
          . "  '$filename'\n"
        if scalar(@lsrtags) > MAX_TAGS - 1;

      my $t = $texidoc;
      $t =~ s/\\\\//g;
      $t =~ s/\\"//g;
      die   "Unescaped backslash found in 'texidoc' field of file\n"
          . "  '$filename'\n"
        if $t =~ /\\/;

      # No newlines in title.
      $doctitle =~ s/\n/ /g;

      my $d = convert_name($doctitle, 1) . ".ly";
      die <<"EOT"
error: 'doctitle' field in snippet '$filename'
  doesn't map to its file name:
    expected '$f'
    but got  '$d'
EOT
        if $d ne $f;
    }
    else {
      die <<"EOT"
error: Snippet '$filename'
  doesn't follow the expected structure:

    \\version "..."

    \\header {
      lsrtags = "..."
      texidoc = "..."
      doctitle = "..."
    }

    <lilypond code>
EOT
    }

    # Create (or overwrite) database entry.
    my $entry;
    if (defined $snippets{$f}) {
      # Get existing entry.
      $entry = $snippets{$f};
    }
    else {
      # Create new reference to anonymous hash and add it.
      $entry = {};
      $snippets{$f} = $entry;
      push @{$snippet_table}, $entry;
    }

    $entry->{"approved"} = 1;
    $entry->{"filename"} = $f;

    for my $idx (0 .. MAX_TAGS - 1 - 1) {
      # Without deleting the hash value first there are mysterious
      #
      #   Modification of non-creatable hash value attempted
      #
      # errors.  The value is set to `undef` if not used, which might
      # be the reason.
      delete $entry->{"id_tag${idx}_tag"};

      if (defined $lsrtags[$idx]) {
        my $tag = convert_name($lsrtags[$idx], 0);
        if (!exists $tags{$tag}) {
          warn   "warning: Snippet '$filename'\n"
               . "  contains new tag '$tag'\n" if !$new;
          $tags{$tag} = ();
          # No need to take care of the `ids` hash, which is no longer
          # needed.
        }

        $entry->{"id_tag${idx}_tag"} = $tag;
      }
      else {
        $entry->{"id_tag${idx}_tag"} = undef;
      }
    }

    # Also add the 'docs' tag.
    my $max = MAX_TAGS - 1;
    delete $entry->{"id_tag${max}_tag"};
    $entry->{"id_tag${max}_tag"} = "docs";

    $entry->{"snippet"} = $code;
    $entry->{"text"} = $texidoc;
    $entry->{"title"} = $doctitle;

    # An additional field to identify a new snippet.
    $entry->{"version"} = $version;
  }

  closedir $dh;
}


my %snippet_lists;

# Function:
#   Store a snippet file name in the `snippet_lists` hash (as a
#   sub-hash entry so that we can easily check later on whether it is
#   approved).
#
# Arguments:
#   $idx         Tag index.
#   $filename    File name.
#   $entry       Reference to snippet table hash.
#
use constant APPROVED => 1;

sub add_to_snippet_list {
  my ($idx, $filename, $entry) = @_;

  if (defined $entry->{"id_tag${idx}_tag"}) {
    $snippet_lists{$entry->{"id_tag${idx}_tag"}}->{$filename}
      = $entry->{"approved"} == APPROVED;
  }

  return;
}


# Fill the `snippet_lists` hash.
if ($dump eq "no") {
  # Use contents of existing snippet list files instead of database
  # entries.
  print "Reading snippet list files\n";
  opendir my $dh, $snippet_dir
    or die "error: Can't open directory '$snippet_dir': $!\n";

  for my $f (sort ly_sort readdir $dh) {
    next unless $f =~ /\.snippet-list$/;
    my $filename = catfile($snippet_dir, $f);
    next unless -f $filename;

    my $tag = $f =~ s/\.snippet-list$//r;

    print "  $f\n" if $verbose;
    open(my $fh, "<:encoding(UTF-8)", $filename)
      or die "error: Can't open file '$filename': $!\n";

    for my $snippet_filename (<$fh>) {
      chomp $snippet_filename;
      # Add snippet as being approved to the categories 'docs' and
      # `$tag`.
      $snippet_lists{"docs"}->{$snippet_filename} = 1;
      $snippet_lists{$tag}->{$snippet_filename} = 1;
    }

    close($fh) or warn "warning: Can't close file '$filename': $!";
  }

  closedir $dh;
}

# If the database is not used, `snippet_table` only holds 'new'
# entries.
for my $entry (@{$snippet_table}) {
  next if not defined $entry->{"version"} and $no_lsr;
  for my $idx (0 .. MAX_TAGS - 1) {
    add_to_snippet_list($idx, $entry->{"filename"}, $entry);
  }
}


unless ($no_snippet_list) {
  # Write snippet lists.  No need to parallelize this since it is very
  # quick.
  print "Creating snippet list files\n";
  for my $list (sort keys %snippet_lists) {
    # Skip unassigned tags (i.e., lists without entries).
    next unless keys %{$snippet_lists{$list}};

    # Don't create a snippet list for the 'docs' tag since it would
    # contain all snippets.
    next if $list eq "docs";

    my @snippet_list_contents;

    for my $snippet (sort ly_sort keys %{$snippet_lists{$list}}) {
      # Only take approved snippets from the 'docs' category.
      if (defined $snippet_lists{"docs"}->{$snippet}
          && $snippet_lists{"docs"}->{$snippet} == APPROVED) {
        push @snippet_list_contents => "$snippet\n";
      }
    }

    # Don't write empty snippet lists.
    next if not @snippet_list_contents;

    my $f = "$list.snippet-list";
    my $filename = catfile($snippet_dir, $f);
    print "  $f\n" if $verbose;
    open(my $fh, ">:encoding(UTF-8)", $filename)
      or die "error: Can't open file '$filename': $!\n";
    print $fh @snippet_list_contents;
    close($fh) or warn "warning: Can't close file '$filename': $!";
  }
}


exit 0 if ($no_lsr && $no_new);


# Function:
#   Convert HTML data into the Texinfo format.
#
#   We pre- and post-process the HTML data sent to and received from
#   the `pandoc` converter, respectively.
#
#   Pre:
#     * Convert `<samp>...</samp>` to `#samp#...#/samp#` since
#       `pandoc` would swallow these tags unprocessed otherwise
#       (tested with version 2.6; note that `pandoc`'s `raw_html`
#       extension has no effect since the Texinfo writer ignores it).
#     * Ditto for `<var>...</var>`.
#     * Convert a full stop followed by two spaces to `#:#` since
#       `pandoc` would swallow the second space otherwise.  [This
#       wouldn't be a problem in the final PDF or HTML output,
#       however, it improves the snippet source code: Editors like
#       Emacs use a double space after a full stop to indicate the end
#       of a sentence in contrast to a single space after an
#       abbreviation full stop.]
#     * Convert `&nbsp;` and U+00A0 to `##::##`.  Pandoc transforms a
#       non-breakable space to `@ `; however, for our purposes
#       `@tie{}` is better.  To avoid overlong output lines we do this
#       temporary replacement before calling pandoc.
#
#   Post:
#     * Remove unusable Texinfo node references to 'Top'.
#     * Escape double quotation marks with a backslash since
#       everything has to be emitted as LilyPond strings.  This can't
#       be done easily before calling pandoc (namely, to avoid too
#       long Texinfo source code lines) because `"` can occur within
#       HTML tags.
#     * Convert `#samp#...#/samp#` to `@samp{...}`.
#     * Convert `#var#...#/var#` to `@var{...}`.
#     * Convert `#:#` back to a full stop followed by two spaces.
#     * Convert `##::##` to `@tie{}`.
#     * Replace ``` ``...'' ``` with `@qq{...}`.
#
#   Note that we don't check whether there is a paragraph boundary
#   between an opening and a closing HTML tag (ditto for
#   ``` ``...'' ```).
#
# Arguments:
#   $s    String to be manipulated.
#
# Return:
#   The manipulated data.
#
sub html_to_texinfo {
  my ($s) = @_;

  $s =~ s|<samp\h*>(.*?)</samp\h*>|#samp#$1#/samp#|sg;
  $s =~ s|<var\h*>(.*?)</var\h*>|#var#$1#/var#|sg;
  $s =~ s/\.  /#:#/g;
  $s =~ s/(?:&nbsp;|\N{U+00A0})/##::##/g;

  $s = pandoc->convert("html" => "texinfo", $s, "--columns=71");

  $s =~ s/\@node Top\n\@top Top\n\n//;
  $s =~ s|#samp#(.*?)#/samp#|\@samp{$1}|sg;
  $s =~ s|#var#(.*?)#/var#|\@var{$1}|sg;
  $s =~ s/#:#/.  /g;
  $s =~ s/"/\\"/g;
  $s =~ s/##::##/\@tie{}/g;
  $s =~ s/``(.*?)''/\@qq{$1}/sg;

  return $s;
}


# Function:
#   Call `convert-ly` to update LilyPond code to current syntax.
#
# Arguments:
#   $in    Input sent to `convert-ly`.
#
# Return:
#   Output of `convert-ly` (stdout and stderr).
#
my $loglevel = $verbose ? "PROGRESS" : "ERROR";

sub convert_ly {
  my ($in) = @_;
  my $out;
  my @err;

  eval {
    run3 [$CONVERT_LY, "--loglevel=$loglevel", "-"],
      \$in, \$out, \@err,
      {binmode_stdin => ":encoding(UTF-8)",
       binmode_stdout => ":encoding(UTF-8)"};
  };

  die "error: $@" if $@;
  die "error: '$CONVERT_LY' killed by signal "
      . ($? & 0x7F) . "\n"
    if ($? & 0x7F);
  die "error: '$CONVERT_LY' exited with error code "
      . ($? >> 8) . "\n"
    if ($? >> 8);

  return ($out, @err);
}


# Start and end of the documentation section of a snippet.
use constant DOC_PREAMBLE => <<~'EOT';
  %% DO NOT EDIT this file manually; it was automatically
  %% generated from the LilyPond Snippet Repository
  %% (http://lsr.di.unimi.it).
  %%
  %% Make any changes in the LSR itself, or in
  %% `Documentation/snippets/new/`, then run
  %% `scripts/auxiliar/makelsr.pl`.
  %%
  %% This file is in the public domain.

  \version "2.22.2"

  \header {
  EOT

# This gets evaluated to eventually expand the variables.
use constant DOC_PREAMBLE_NEW => <<~'EOT';
  %% DO NOT EDIT this file manually; it was automatically
  %% generated from `Documentation/snippets/new/`.
  %%
  %% Make any changes in `Documentation/snippets/new/`,
  %% then run `scripts/auxiliar/makelsr.pl --new`.
  %%
  %% This file is in the public domain.
  %%
  %% Note: this file works from version $version.

  \version "$version"

  \header {
  EOT

use constant DOC_POSTAMBLE => <<~'EOT';
  } % begin verbatim


  EOT


# Emit all approved snippets from the 'docs' category as files.  This
# gets processed in parallel.
my $pm = Parallel::ForkManager->new($jobs);
$pm->set_waitpid_blocking_sleep(0);

print "Creating snippet files\n";
FILES:
for my $entry (sort { $a->{"filename"} cmp $b->{"filename"} }
               @{$snippet_table}) {
  $pm->start and next FILES;

  my @lsrtags = ();
  my $is_docs = 0;

  # Collect tags in array `lsrtags` (except tag 'docs', which all of
  # our snippets have set).
  for my $idx (0 .. MAX_TAGS - 1) {
    my $tag_field = "id_tag${idx}_tag";

    if (defined $entry->{$tag_field}) {
      if ($entry->{$tag_field} eq "docs") {
        $is_docs = 1;
      }
      else {
        push @lsrtags => $entry->{$tag_field};
      }
    }
  }

  # Only use approved documentation snippets.
  if ($is_docs && $entry->{"approved"} == APPROVED) {
    my $f = $entry->{"filename"};
    my $filename = catfile($snippet_dir, $f);
    my $version = $entry->{"version"} // "";

    if (($no_lsr && $version) || !$no_lsr) {
      my $data;

      if ($version) {
        my $s = DOC_PREAMBLE_NEW;
        # Replace variables with itself, but interpolated.
        $s =~ s/(\$\w+)/$1/gee;
        $data .= $s;
      }
      else {
        $data .= DOC_PREAMBLE;
      }

      $data .=   '  lsrtags = "'
               . join(", ", sort @lsrtags) . '"' . "\n"
               . "\n";

      my $texidoc = normalize_text($entry->{"text"}, !$version);
      $texidoc = html_to_texinfo($texidoc) if not $version;

      $data .=   '  texidoc = "' . "\n"
               . $texidoc . "\n"
               . '"' . "\n"
               . "\n";

      # The title gets stored as a LilyPond string, too.
      my $doctitle = normalize_text($entry->{"title"}, !$version);
      $doctitle = html_to_texinfo($doctitle) if not $version;

      # No newlines in title.
      $doctitle =~ s/\n+$//;
      $doctitle =~ s/\n/ /g;

      $data .= '  doctitle = "' . $doctitle . '"' . "\n";
      $data .= DOC_POSTAMBLE;

      # Finally, emit the LilyPond snippet code itself.
      my $snippet = normalize_text($entry->{"snippet"}, 0);

      # We have to remove '%LSR' comments, which are not intended to
      # be exported from the LSR.
      $snippet .= "\n";
      $snippet =~ s/^ \h* %+ \h* LSR .* \n//gmx; # `%LSR` on a line of its own.
      $snippet =~ s/\h* %+ \h* LSR .* $//gmx; # Trailing `%LSR`.

      $data .= $snippet;

      my @diag = ();
      ($data, @diag) = convert_ly($data) unless $no_convert;

      if (@diag) {
        # Emitting the file name and the diagnose messages (with some
        # indentation) in one call ensures correct log output even if
        # processing in parallel.
        my $d =   "  $filename:\n"
                . "    " . join("    ", @diag);
        $d =~ s/^ \h+ $//gxm;
        print $d;
      }
      else {
        print "  $f\n" if $verbose;
      }

      open(my $fh, ">:encoding(UTF-8)", $filename)
        or die "error: Can't open file '$filename': $!\n";
      print $fh $data;
      close($fh) or warn "warning: Can't close file '$filename': $!";
    }
  }

  $pm->finish;
}

$pm->wait_all_children;

exit 0;


# Local variables:
# perl-indent-level: 2
# indent-tabs-mode: nil
# End:
