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
import sys
from typing import Dict, List

import requests

from upload import api_url, get_files, package_url

parser = argparse.ArgumentParser(description="Create a release on GitLab")
parser.add_argument(
    "--repo", help="repository at gitlab.com", default="lilypond/lilypond"
)
parser.add_argument("--token", help="GitLab Access Token", required=True)
parser.add_argument("--description", help="file with description for release")
parser.add_argument("version", help="of the release")
args = parser.parse_args()

links: List[Dict[str, str]] = []
# In reverse order because the most recently uploaded files are shown first.
for file in reversed(get_files(args.version)):
    links.append(
        {
            "name": file,
            "url": package_url(args.repo, args.version, file),
            "filepath": f"/{file}",
            "link_type": "package",
        }
    )

release = {
    "name": f"LilyPond {args.version}",
    "tag_name": f"v{args.version}",
    "milestones": [args.version],
    "assets": {"links": links},
}

# If specified, read the description and add it to the data.
if args.description:
    with open(args.description, "r", encoding="utf-8") as file:
        release["description"] = file.read()

base_api_url = api_url(args.repo)
url = f"{base_api_url}/releases"
headers = {"Private-Token": args.token}
r = requests.post(url, headers=headers, json=release)
# Accept any 2xx status code.
if r.status_code // 100 == 2:
    print(f"Successfully created release for version '{args.version}'!")
else:
    print(f"Failure during creating of release for version '{args.version}':")
    print(r.json())
    sys.exit(1)
