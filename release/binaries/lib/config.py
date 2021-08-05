"""This module contains the class Config, which knows about the
platform (the current one by default) and a set of directories,
for example where to store downloaded files.
"""

import enum
import os
import sys


@enum.unique
class Platform(enum.Enum):
    """An enum of all supported platforms"""

    # The values correspond to sys.platform
    LINUX = "linux"
    MACOS = "darwin"

    @classmethod
    def get_platform(cls, platform: str) -> "Platform":
        """Find the platform for the given value of sys.platform"""
        for member in cls.__members__.values():
            if member.value == platform:
                return member
        raise KeyError(f"Platform '{platform}' not found")


class Config:
    """A class to store the configuration for a given platform."""

    base_dir: str
    downloads_dir: str
    jobs: int
    platform: Platform

    def __init__(
        self,
        base_dir: str,
        downloads_dir: str = None,
        jobs: int = 1,
        platform: Platform = None,
    ):
        self.base_dir = os.path.realpath(base_dir)
        if downloads_dir is None:
            downloads_dir = os.path.join(base_dir, "downloads")
        self.downloads_dir = downloads_dir

        self.jobs = jobs

        if platform is None:
            self.platform = Platform.get_platform(sys.platform)
        else:
            self.platform = platform

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

    def is_linux(self) -> bool:
        """Return True if this config is for platform Linux"""
        return self.platform == Platform.LINUX

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
