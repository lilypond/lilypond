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

"""This module contains the base-class Package, and the derived classes
ConfigurePackage and MesonPackage, which implement the build commands
for these two systems.
"""

# All classes in this file are abstract, but pylint cannot deduce this for the
# derived ones because they do not define new abstract methods.
# pylint: disable=abstract-method

import logging
import os
import shutil
import subprocess
from typing import Callable, Dict, List, Optional, TextIO
import urllib.request

from .config import Config

lib_path = os.path.dirname(os.path.realpath(__file__))


class Package:
    """Base class for all packages."""

    # pylint: disable=too-many-public-methods

    def enabled(self, c: Config) -> bool:
        """Return whether this package is enabled for this configuration."""
        # pylint: disable=unused-argument
        return True

    @property
    def version(self) -> str:
        """Return the version of this package"""
        raise NotImplementedError

    @property
    def directory(self) -> str:
        """Return the directory name for this package

        This usually includes the version of the package.
        """
        raise NotImplementedError

    def src_directory(self, c: Config) -> str:
        """Return the source directory for this package"""
        return os.path.join(c.dependencies_src_dir, self.directory)

    def build_directory(self, c: Config) -> str:
        """Return the build directory for this package"""
        return os.path.join(c.dependencies_build_dir, self.directory)

    def install_directory(self, c: Config) -> str:
        """Return the temporary install directory for this package"""
        return os.path.join(c.dependencies_install_dir, self.directory)

    def log_path(self, c: Config) -> str:
        """Return the path to the log file for this package"""
        return os.path.join(c.dependencies_log_dir, f"{self.directory}.log")

    @property
    def archive(self) -> str:
        """Return the archive file name of this package

        This is usually the directory name plus an archive extension.
        """
        raise NotImplementedError

    def archive_path(self, c: Config) -> str:
        """Return the full path to the archive file

        By default, the file is stored in the downloads directory.
        """
        return os.path.join(c.downloads_dir, self.archive)

    @property
    def download_url(self) -> str:
        """Return the download URL for this package

        The build process expects an extractable archive.
        """
        raise NotImplementedError

    def dependencies(self, c: Config) -> List["Package"]:
        """Return the dependencies for this package"""
        # pylint: disable=unused-argument
        return []

    def download(self, c: Config):
        """Download the package's archive."""
        dest = self.archive_path(c)
        if os.path.exists(dest):
            logging.debug("'%s' already downloaded", self.archive)
        else:
            logging.info("Downloading '%s'...", self.download_url)
            urllib.request.urlretrieve(self.download_url, dest)

    def patch_file(self, c: Config, filename: str, patch: Callable[[str], str]):
        """Patch the content of a file with the given lambda."""
        filename = os.path.join(self.src_directory(c), filename)
        with open(filename, "r", encoding="utf-8") as old:
            content = old.read()
        content = patch(content)
        with open(filename, "w", encoding="utf-8") as new:
            new.write(content)

    def apply_patches(self, c: Config):
        """Apply additional patches (if needed)"""

    def prepare_sources(self, c: Config) -> bool:
        """Extract the archive and apply additional patches (if needed)."""
        src_directory = self.src_directory(c)
        if os.path.exists(src_directory):
            logging.debug("'%s' already extracted", self.archive)
            return True

        archive = self.archive_path(c)
        if not os.path.exists(archive):
            logging.error("'%s' does not exist!", self.archive)
            return False

        # Do not use c.dependencies_src_dir directly, sub-classes may override.
        extract_dir = os.path.dirname(self.src_directory(c))
        shutil.unpack_archive(archive, extract_dir)

        self.apply_patches(c)

        return True

    def pkgconfig_path(self, c: Config) -> Optional[str]:
        """Return the pkgconfig path for this path"""
        # Try the default location.
        lib_pkgconfig = os.path.join(self.install_directory(c), "lib", "pkgconfig")
        if os.path.isdir(lib_pkgconfig):
            return lib_pkgconfig

        # On FreeBSD, meson puts pkgconfig files into libdata/.
        libdata_pkgconfig = os.path.join(
            self.install_directory(c), "libdata", "pkgconfig"
        )
        if os.path.isdir(libdata_pkgconfig):
            return libdata_pkgconfig

        return None

    def collect_pkgconfig_paths(self, c: Config) -> List[str]:
        """Collect the pkgconfig paths for all dependencies."""
        all_paths = []
        for dependency in self.dependencies(c):
            all_paths += dependency.collect_pkgconfig_paths(c)

        path = self.pkgconfig_path(c)
        if path is not None:
            all_paths.append(path)

        return all_paths

    def build_env_extra(self, c: Config) -> Dict[str, str]:
        """Return additional environment mappings to build this package."""
        env: Dict[str, str] = {}
        env["PKG_CONFIG"] = os.path.join(lib_path, "pkg-config-static.sh")
        env["PKG_CONFIG_LIBDIR"] = os.pathsep.join(self.collect_pkgconfig_paths(c))
        return env

    def run_command(self, c: Config, args: List[str], log: TextIO, stage: str) -> bool:
        """Run command in the build directory and log invocation."""
        # Separating command and args with single quotes to prevent
        # the need for escaping spaces in the paths or arguments when
        # copying the invocations to the shell.
        formatted_args = "' '".join(args)
        formatted_args = f"'{formatted_args}'"
        formatted_env = "\n  ".join(
            [f"{key}={val}" for key, val in self.build_env_extra(c).items()]
        )
        log_entry = (
            "Running command\n"
            f"  {formatted_args}\n"
            "in directory\n"
            f"  '{self.build_directory(c)}'\n"
            "with additional environmental settings\n"
            f"  {formatted_env}"
        )
        logging.debug(log_entry)
        log.write(log_entry + "\n*** Begin of command output ***\n")
        log.flush()

        build_env = os.environ.copy()
        build_env.update(self.build_env_extra(c))

        result = subprocess.run(
            args,
            stdout=log,
            stderr=log,
            cwd=self.build_directory(c),
            env=build_env,
            check=False,
        )

        log.write("*** End of command output ***\n\n")

        if result.returncode != 0:
            logging.error("%s exited with code %d", stage, result.returncode)
            return False

        return True

    def build(self, c: Config) -> bool:
        """Build the package and install it to a temporary location."""
        raise NotImplementedError

    @property
    def license_files(self) -> List[str]:
        """Return the list of license files for this package"""
        return []

    def copy_license_files(self, destination: str, c: Config):
        """Copy the license files for this package to destination."""
        for license_file in self.license_files:
            src = os.path.join(self.src_directory(c), license_file)
            basename = os.path.basename(license_file)
            dst = os.path.join(destination, f"{self.directory}.{basename}")
            shutil.copy(src, dst)


class ConfigurePackage(Package):
    """A package that has a configure script and is built with make."""

    @property
    def configure_script(self) -> str:
        """Return the relative path to the configure script"""
        return "configure"

    def configure_args_triples(self, c: Config) -> List[str]:
        """Return the parameters to configure the package for the platform"""
        return [
            # "--build" is the current system.
            f"--build={c.native_config.triple}",
            # "--host" is the system we are compiling for.
            f"--host={c.triple}",
        ]

    def configure_args_static(self, c: Config) -> List[str]:
        """Return the parameters to configure the package for a static build (if any)"""
        # pylint: disable=unused-argument
        return ["--disable-shared", "--enable-static"]

    def configure_args(self, c: Config) -> List[str]:
        """Return additional parameters to pass to the configure script

        The build process automatically adds options for the build and host
        triples (see configure_args_triples), a static library build (see
        configure_args_static), as well as the install prefix to the temporary
        location."""
        # pylint: disable=unused-argument
        return []

    def make_args(self, c: Config) -> List[str]:
        """Return additional parameters to pass to the make invocation"""
        # pylint: disable=unused-argument
        return []

    def make_install_args(self, c: Config) -> List[str]:
        """Return additional parameters to pass to make install"""
        # pylint: disable=unused-argument
        return []

    def build(self, c: Config) -> bool:
        src_directory = self.src_directory(c)
        build_directory = self.build_directory(c)
        install_directory = self.install_directory(c)

        os.makedirs(build_directory, exist_ok=True)

        # Set up the log file.
        with open(self.log_path(c), "w", encoding="utf-8") as log:

            # Run the configure script.
            args = [f"{src_directory}/{self.configure_script}"]

            # Add the target triples.
            args += self.configure_args_triples(c)

            # Disable shared libraries, force static library build.
            args += self.configure_args_static(c)

            # Install the package to a temporary location.
            args += [f"--prefix={install_directory}"]
            args += self.configure_args(c)

            if not self.run_command(c, args, log, "configure"):
                return False

            # Build the package.
            args = [c.make_command, "-j", str(c.jobs)] + self.make_args(c)
            if not self.run_command(c, args, log, "make"):
                return False

            # Install the package.
            args = [c.make_command, "install"] + self.make_install_args(c)
            if not self.run_command(c, args, log, "install"):
                return False

        return True


class MesonPackage(Package):
    """A package that is configured with meson and built with ninja."""

    def meson_args_static(self, c: Config) -> List[str]:
        """Return the parameters to configure the package for a static build (if any)"""
        # pylint: disable=unused-argument
        return ["--default-library=static"]

    def meson_args(self, c: Config) -> List[str]:
        """Return additional parameters to pass to 'meson setup'

        The build process automatically adds options for optimizations, a static
        library build (see meson_args_static) and the install prefix to the
        temporary location."""
        # pylint: disable=unused-argument
        return []

    def build(self, c: Config):
        src_directory = self.src_directory(c)
        build_directory = self.build_directory(c)
        install_directory = self.install_directory(c)

        os.makedirs(build_directory, exist_ok=True)

        # Set up the log file.
        with open(self.log_path(c), "w", encoding="utf-8") as log:

            # Run 'meson setup' to configure the package.
            args = ["meson", "setup", "--buildtype=release"]

            if c.is_mingw():
                mingw_cross = os.path.join(lib_path, "mingw_cross.txt")
                args += [f"--cross-file={mingw_cross}"]

            # Disable shared libraries, force static library build.
            args += self.meson_args_static(c)

            # Install the package to a temporary location.
            args += ["--libdir=lib", f"--prefix={install_directory}"]
            # Disable automatic feature detection, packages should add explicit
            # options.
            args += ["--auto-features=disabled"]
            args += self.meson_args(c)
            args += [src_directory, build_directory]

            if not self.run_command(c, args, log, "configure"):
                return False

            # Build the package.
            args = ["ninja", "-j", str(c.jobs)]
            if not self.run_command(c, args, log, "build"):
                return False

            # Install the package.
            args = ["ninja", "install"]
            if not self.run_command(c, args, log, "install"):
                return False

        return True
