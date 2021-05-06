#!/bin/sh

set -eu

GITLAB_API="https://gitlab.com/api/v4"
GITLAB_API_PROJECT="$GITLAB_API/projects/lilypond%2Flilypond"

REF_NAME="master"
JOB_NAME="test-baseline"

# FIXME: Always downloads the latest test-baseline; likely want the one for the
# base commit of the current merge request.
ARTIFACT_URL="$GITLAB_API_PROJECT/jobs/artifacts/$REF_NAME/raw/test-baseline.tar.gz?job=$JOB_NAME"
echo "Downloading $ARTIFACT_URL ..."
wget --quiet -O test-baseline.tar.gz "$ARTIFACT_URL"

echo "Extracting test-baseline.tar.gz ..."
tar xf test-baseline.tar.gz

if [ -z "${GITLAB_CI-}" ]; then
  echo "WARNING: It appears you are running this script outside of GitLab's" >&2
  echo "CI environment! If you want to use the downloaded test-baseline" >&2
  echo "locally, you need to adapt the stored paths to match your setup:" >&2
  echo " - The source directory was \`/builds/lilypond/lilypond/'." >&2
  echo " - The build directory was \`/builds/lilypond/lilypond/build/'." >&2
  echo " - Fonts were installed as packaged by Ubuntu 18.04, for example" >&2
  echo "   DejaVu Sans in \`/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf'." >&2
  exit 1
fi
