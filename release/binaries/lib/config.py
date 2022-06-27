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

"""This module contains the class Config, which knows about the
platform (the current one by default) and a set of directories,
for example where to store downloaded files.
"""

import enum
import os
import platform as py_platform
import sysconfig


@enum.unique
class Platform(enum.Enum):
    """An enum of all supported platforms"""

    # The values correspond to platform.system(), converted to lower case.
    FREEBSD = "freebsd"
    LINUX = "linux"
    MACOS = "darwin"

    MINGW = "mingw"

    @classmethod
    def get_platform(cls, platform_system: str) -> "Platform":
        """Find the platform for the given value of platform.system()"""
        platform_system = platform_system.lower()
        for member in cls.__members__.values():
            if member.value == platform_system:
                return member
        raise KeyError(f"Platform '{platform_system}' not found")


class Config:
    """A class to store the configuration for a given platform."""

    # pylint: disable=too-many-instance-attributes

    base_dir: str
    downloads_dir: str
    jobs: int
    platform: Platform
    architecture: str
    triple: str
    native_config: "Config"
    program_suffix: str

    def __init__(
        self,
        base_dir: str,
        downloads_dir: str = None,
        jobs: int = 1,
        platform: Platform = None,
        architecture: str = None,
        triple: str = None,
        native_config: "Config" = None,
        program_suffix: str = "",
    ):
        """Create a new Config instance, optionally for a given platform."""
        # pylint: disable=too-many-arguments

        self.base_dir = os.path.realpath(base_dir)
        if downloads_dir is None:
            downloads_dir = os.path.join(base_dir, "downloads")
        self.downloads_dir = downloads_dir

        self.jobs = jobs

        self.platform = platform or Platform.get_platform(py_platform.system())
        self.architecture = architecture or py_platform.machine()
        if triple is None:
            triple = sysconfig.get_config_var("HOST_GNU_TYPE")
            assert triple is not None
        self.triple = triple

        self.native_config = native_config or self
        self.program_suffix = program_suffix

    @property
    def dependencies_dir(self) -> str:
        """Return the path to the dependencies"""
        return os.path.join(self.base_dir, "dependencies")

    @property
    def dependencies_src_dir(self) -> str:
        """Return the path to the dependencies' sources"""
        return os.path.join(self.dependencies_dir, "src")

    @property
    def dependencies_build_dir(self) -> str:
        """Return the path to the dependencies' build directories"""
        return os.path.join(self.dependencies_dir, "build")

    @property
    def dependencies_install_dir(self) -> str:
        """Return the path to the dependencies' temporary install directories"""
        return os.path.join(self.dependencies_dir, "install")

    @property
    def dependencies_log_dir(self) -> str:
        """Return the path to the directory with the dependencies' logs"""
        return os.path.join(self.dependencies_dir, "log")

    def is_freebsd(self) -> bool:
        """Return True if this config is for platform FreeBSD"""
        return self.platform == Platform.FREEBSD

    def is_linux(self) -> bool:
        """Return True if this config is for platform Linux"""
        return self.platform == Platform.LINUX

    def is_macos(self) -> bool:
        """Return True if this config is for platform macOS"""
        return self.platform == Platform.MACOS

    def is_mingw(self) -> bool:
        """Return True if this config is for platform mingw"""
        return self.platform == Platform.MINGW

    @property
    def make_command(self) -> str:
        """Return the command for the make build system"""
        if self.is_freebsd():
            # Use GNU make instead of BSD make.
            return "gmake"
        return "make"

    def create_directories(self):
        """Create all necessary directories for this configuration."""
        os.makedirs(self.downloads_dir, exist_ok=True)
        os.makedirs(self.dependencies_dir, exist_ok=True)
        os.makedirs(self.dependencies_src_dir, exist_ok=True)
        os.makedirs(self.dependencies_build_dir, exist_ok=True)
        os.makedirs(self.dependencies_install_dir, exist_ok=True)
        os.makedirs(self.dependencies_log_dir, exist_ok=True)

    def __repr__(self) -> str:
        return f"<Config for {self.platform}>"


class MingwConfig(Config):
    """A class for cross-compilation to mingw."""

    def __init__(
        self,
        base_dir: str,
        native_config: Config,
    ):
        super().__init__(
            base_dir=base_dir,
            downloads_dir=native_config.downloads_dir,
            jobs=native_config.jobs,
            platform=Platform.MINGW,
            architecture="x86_64",
            triple="x86_64-w64-mingw32",
            native_config=native_config,
            program_suffix=".exe",
        )
