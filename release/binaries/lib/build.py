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
from typing import Callable, Dict, List, Optional
import urllib.request

from .config import Config

lib_path = os.path.dirname(os.path.realpath(__file__))


class Package:
    """Base class for all packages."""

    # pylint: disable=too-many-public-methods

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

    @property
    def dependencies(self) -> List["Package"]:
        """Return the dependencies for this package"""
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
        with open(filename, "r") as old:
            content = old.read()
        content = patch(content)
        with open(filename, "w") as new:
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

        return None

    def collect_pkgconfig_paths(self, c: Config) -> List[str]:
        """Collect the pkgconfig paths for all dependencies."""
        all_paths = []
        for dependency in self.dependencies:
            all_paths += dependency.collect_pkgconfig_paths(c)

        path = self.pkgconfig_path(c)
        if path is not None:
            all_paths.append(path)

        return all_paths

    @property
    def configure_default_static(self) -> bool:
        """Return True if the package should be configured for a static build
        with the default arguments."""
        return True

    def build_env(self, c: Config) -> Dict[str, str]:
        """Return the environment mapping to build this package."""
        env = os.environ
        env["PKG_CONFIG"] = os.path.join(lib_path, "pkg-config-static.sh")
        env["PKG_CONFIG_LIBDIR"] = os.pathsep.join(self.collect_pkgconfig_paths(c))
        return env

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

    def configure_args(self, c: Config) -> List[str]:
        """Return additional parameters to pass to the configure script

        The build process automatically adds options for a static library build
        (unless !self.configure_default_static) and the install prefix to the
        temporary location."""
        # pylint: disable=no-self-use,unused-argument
        return []

    @property
    def make_args(self) -> List[str]:
        """Return additional parameters to pass to the make invocation"""
        return []

    @property
    def make_install_args(self) -> List[str]:
        """Return additional parameters to pass to make install"""
        return []

    def build(self, c: Config) -> bool:
        src_directory = self.src_directory(c)
        build_directory = self.build_directory(c)
        install_directory = self.install_directory(c)

        os.makedirs(build_directory, exist_ok=True)

        build_env = self.build_env(c)

        # Set up the log file.
        with open(self.log_path(c), "w") as log:

            def run(args: List[str]) -> subprocess.CompletedProcess:
                formatted_args = "' '".join(args)
                formatted_args = f"'{formatted_args}'"
                logging.debug("Running [ %s ] in '%s'", formatted_args, build_directory)
                log.write(f" $ {formatted_args}\n")
                log.flush()

                return subprocess.run(
                    args,
                    stdout=log,
                    stderr=log,
                    cwd=build_directory,
                    env=build_env,
                    check=False,
                )

            # Run the configure script.
            args = [f"{src_directory}/configure"]

            if self.configure_default_static:
                # Disable shared libraries, force static library build.
                args += ["--disable-shared", "--enable-static"]

            # Install the package to a temporary location.
            args += [f"--prefix={install_directory}"]
            args += self.configure_args(c)

            result = run(args)
            if result.returncode != 0:
                logging.error("configure exited with code %d", result.returncode)
                return False

            # Build the package.
            args = ["make", "-j", str(c.jobs)] + self.make_args
            result = run(args)
            if result.returncode != 0:
                logging.error("make exited with code %d", result.returncode)
                return False

            # Install the package.
            args = ["make", "install"] + self.make_install_args
            result = run(args)
            if result.returncode != 0:
                logging.error("install exited with code %d", result.returncode)
                return False

        return True


class MesonPackage(Package):
    """A package that is configured with meson and built with ninja."""

    def meson_args(self, c: Config) -> List[str]:
        """Return additional parameters to pass to 'meson setup'

        The build process automatically adds options for optimizations, a static
        library build (unless !self.configure_default_static) and the install
        prefix to the temporary location."""
        # pylint: disable=no-self-use,unused-argument
        return []

    def build(self, c: Config):
        src_directory = self.src_directory(c)
        build_directory = self.build_directory(c)
        install_directory = self.install_directory(c)

        os.makedirs(build_directory, exist_ok=True)

        build_env = self.build_env(c)

        # Set up the log file.
        with open(self.log_path(c), "w") as log:

            def run(args: List[str]) -> subprocess.CompletedProcess:
                formatted_args = "' '".join(args)
                formatted_args = f"'{formatted_args}'"
                logging.debug("Running [ %s ] in '%s'", formatted_args, build_directory)
                log.write(f" $ {formatted_args}\n")
                log.flush()

                return subprocess.run(
                    args,
                    stdout=log,
                    stderr=log,
                    cwd=build_directory,
                    env=build_env,
                    check=False,
                )

            # Run 'meson setup' to configure the package.
            args = ["meson", "setup", "--buildtype=release"]

            if self.configure_default_static:
                # Disable shared libraries, force static library build.
                args += ["--default-library=static"]

            # Install the package to a temporary location.
            args += ["--libdir=lib", f"--prefix={install_directory}"]
            # Disable automatic feature detection, packages should add explicit
            # options.
            args += ["--auto-features=disabled"]
            args += self.meson_args(c)
            args += [src_directory, build_directory]

            result = run(args)
            if result.returncode != 0:
                logging.error("configure exited with code %d", result.returncode)
                return False

            # Build the package.
            args = ["ninja", "-j", str(c.jobs)]
            result = run(args)
            if result.returncode != 0:
                logging.error("build exited with code %d", result.returncode)
                return False

            # Install the package.
            args = ["ninja", "install"]
            result = run(args)
            if result.returncode != 0:
                logging.error("install exited with code %d", result.returncode)
                return False

        return True
