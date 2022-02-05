#!/usr/bin/env python3

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2022--2022 Jonas Hahnfeld <hahnjo@hahnjo.de>
#
# LilyPond is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

import argparse
import os
import sys
import urllib.parse
from typing import List

import requests

# Also used by create-release.py
FILE_PATTERNS = [
    # sources
    "lilypond-%s.tar.gz",
    # binaries
    "lilypond-%s-darwin-x86_64.tar.gz",
    "lilypond-%s-linux-x86_64.tar.gz",
    "lilypond-%s-mingw-x86_64.zip",
    # documentation
    "lilypond-%s-documentation.tar.xz",
]


def get_files(version: str) -> List[str]:
    """Return the list of files for this version."""
    return [file_pattern % version for file_pattern in FILE_PATTERNS]


def api_url(repo: str) -> str:
    quoted_repo = urllib.parse.quote_plus(repo)
    return f"https://gitlab.com/api/v4/projects/{quoted_repo}"


def package_url(repo, version, file: str) -> str:
    """Return the URL for a file in a versioned package."""
    base_api_url = api_url(repo)
    return f"{base_api_url}/packages/generic/lilypond/{version}/{file}"


# Main functionality if invoked as a script.
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Upload a release to GitLab")
    parser.add_argument(
        "--repo", help="repository at gitlab.com", default="lilypond/lilypond"
    )
    parser.add_argument("--token", help="GitLab Access Token", required=True)
    parser.add_argument("version", help="of the release")
    args = parser.parse_args()

    # Create a session to speed up multiple requests.
    api = requests.Session()
    api.headers = {"Private-Token": args.token}

    files = get_files(args.version)
    missing = []
    for file in files:
        if not os.path.exists(file):
            missing.append(file)

    if len(missing) > 0:
        print("The following files are missing:")
        for file in missing:
            print(f"  - {file}")
        print("Please put them into one directory and start this script.")
        sys.exit(1)

    print(f"Uploading files for Lilypond {args.version}:")
    # In reverse order because the most recently uploaded files are shown first.
    for file in reversed(files):
        print(f" * Uploading {file} ...")
        with open(file, "rb") as file_to_upload:
            # Make sure a previously deleted package is made visible again.
            url = package_url(args.repo, args.version, file) + "?status=default"
            api.put(url, data=file_to_upload)
