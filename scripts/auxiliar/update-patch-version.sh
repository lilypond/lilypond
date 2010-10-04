#! /bin/bash
#
#  Update the version number in lilypond snippets without running convert-ly
#   on the snippets
#  Note: this does not change anything but the version number, and it will only
#   change the specified version number
#
#  This is useful when a patch containing a number of snippets (due to a change
#   in syntax) has not completed review before a new development release
#   is made.
#
#  Usage: update-patch-version old-version new-version

if [ $# -ne 2 ] # need exactly 2 arguments
then
  echo "Usage:  update-patch-version old-version new-version"
  exit 1
fi

echo "Warning -- if this script is run after a branch is rebased,"
echo "  unintended changes will occur. It would be best to revert"
echo "  the rebase commit before running."

git grep --name-only $1 | xargs sed -i -e s/$1/$2/g
