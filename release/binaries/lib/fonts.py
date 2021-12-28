# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2021--2022 Jonas Hahnfeld <hahnjo@hahnjo.de>
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

"""This module defines all fonts packaged with LilyPond."""

# Each class in this file represents a package, ignore missing docstrings.
# pylint: disable=missing-class-docstring

import glob
import os
import shutil
from typing import List
import urllib.request

from .build import Package
from .config import Config


class FontPackage(Package):
    """A package representing a font of .otf files."""

    # This class is abstract, but pylint cannot deduce this because it does not
    # define new abstract methods.
    # pylint: disable=abstract-method

    def build(self, c: Config):
        src_directory = self.src_directory(c)
        install_directory = self.install_directory(c)

        os.makedirs(install_directory, exist_ok=True)

        # Collect and copy over all .otf files from the downloaded archive.
        otf_glob = os.path.join(src_directory, "**", "*.otf")
        for otf_file in glob.glob(otf_glob, recursive=True):
            shutil.copy(otf_file, install_directory)

        return True


class TeXGyre(FontPackage):
    @property
    def version(self) -> str:
        return "2.501"

    @property
    def directory(self) -> str:
        directory_version = self.version.replace(".", "_")
        return f"tg{directory_version}otf"

    @property
    def archive(self) -> str:
        return f"{self.directory}.zip"

    @property
    def download_url(self) -> str:
        return (
            f"http://www.gust.org.pl/projects/e-foundry/tex-gyre/whole/{self.archive}"
        )

    @property
    def license_file(self) -> str:
        """The license file that will be downloaded in apply_patches."""
        return "GUST-FONT-LICENSE.txt"

    def apply_patches(self, c: Config):
        # Not really a patch, but the archive comes without a license that we
        # need to download separately.
        license_url = (
            f"http://www.gust.org.pl/projects/e-foundry/licenses/{self.license_file}"
        )
        license_path = os.path.join(self.src_directory(c), self.license_file)
        if not os.path.exists(license_path):
            urllib.request.urlretrieve(license_url, license_path)

    @property
    def license_files(self) -> List[str]:
        return [self.license_file]

    def __str__(self) -> str:
        return f"TeX Gyre {self.version}"


texgyre = TeXGyre()


class UrwBase35(FontPackage):
    @property
    def version(self) -> str:
        return "20200910"

    @property
    def directory(self) -> str:
        return f"urw-base35-fonts-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.version}.zip"

    @property
    def download_url(self) -> str:
        # pylint: disable=line-too-long
        return f"https://github.com/ArtifexSoftware/urw-base35-fonts/archive/refs/tags/{self.archive}"

    @property
    def license_files(self) -> List[str]:
        return ["LICENSE", "COPYING"]

    def __str__(self) -> str:
        return f"(URW)++ base 35 fonts {self.version}"


urwbase35 = UrwBase35()

all_fonts: List[Package] = [
    texgyre,
    urwbase35,
]
