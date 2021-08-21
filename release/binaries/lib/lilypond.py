"""This module defines a package for LilyPond."""

import glob
import logging
import os
import shutil
import stat
import subprocess
import tarfile
from typing import Dict, List

from .build import Package, ConfigurePackage
from .config import Config
from .dependencies import all_dependencies
from .dependencies import freetype, fontconfig, ghostscript, glib, guile, pango, python


class LilyPond(ConfigurePackage):
    """A package definition to build static binaries of LilyPond."""

    _archive_path: str

    def __init__(self, archive: str):
        archive = os.path.realpath(archive)
        self._archive_path = archive

        directory_prefix = "lilypond-"
        with tarfile.open(archive, "r") as tar:
            # Assume that the first member of the archive is the directory
            # that contains all source files. Use it to determine the version
            # and do not rely on the archive name, which the user may rename.
            directory = tar.next().name
            if not directory.startswith(directory_prefix):
                raise ValueError("Specified archive has invalid structure")

        # Remember some values to return in the property methods below.
        self._version = directory[len(directory_prefix) :]
        self._directory = directory

    @property
    def version(self) -> str:
        return self._version

    @property
    def directory(self) -> str:
        return self._directory

    def src_directory(self, c: Config) -> str:
        return os.path.join(c.base_dir, "lilypond", self.directory)

    def build_directory(self, c: Config) -> str:
        return os.path.join(c.base_dir, "lilypond", "build")

    def install_directory(self, c: Config) -> str:
        return os.path.join(c.base_dir, "lilypond", "install")

    def log_path(self, c: Config) -> str:
        return os.path.join(c.base_dir, "lilypond", "lilypond.log")

    @property
    def archive(self) -> str:
        return os.path.basename(self._archive_path)

    def archive_path(self, c: Config) -> str:
        return self._archive_path

    @property
    def download_url(self) -> str:
        raise NotImplementedError

    @property
    def dependencies(self) -> List[Package]:
        return [freetype, fontconfig, ghostscript, glib, guile, pango, python]

    @property
    def configure_default_static(self) -> bool:
        # LilyPond itself isn't a library!
        return False

    def build_env(self, c: Config) -> Dict[str, str]:
        env = super().build_env(c)
        env["GHOSTSCRIPT"] = ghostscript.exe_path(c)
        env["GUILE"] = guile.exe_path(c)
        env["PYTHON"] = python.exe_path(c)
        return env

    def configure_args(self, c: Config) -> List[str]:
        return [
            # Include the static version of libstdc++.
            "--enable-static-gxx",
            # Disable the documentation.
            "--disable-documentation",
        ]

    @property
    def python_scripts(self) -> List[str]:
        """Return a list of all Python scripts installed by default."""
        return [
            "abc2ly",
            "convert-ly",
            "etf2ly",
            "lilymidi",
            "lilypond-book",
            "lilysong",
            "midi2ly",
            "musicxml2ly",
        ]

    @property
    def guile_scripts(self) -> List[str]:
        """Return a list of all Guile scripts installed by default."""
        return ["lilypond-invoke-editor"]

    @property
    def license_files(self) -> List[str]:
        return ["COPYING"]

    def __str__(self) -> str:
        return f"LilyPond {self.version}"


def strip(binary: str):
    """Strip debugging symbols from an executable or shared library."""
    args = ["strip", binary]
    subprocess.run(args, check=True)


def chmod_x(path: str):
    """Make a file executable."""
    mode = os.stat(path).st_mode
    mode |= stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH
    os.chmod(path, mode)


class LilyPondPackager:
    """A class to package the built binaries of LilyPond and other needed files."""

    lilypond: LilyPond
    c: Config

    def __init__(self, lilypond: LilyPond, c: Config):
        self.lilypond = lilypond
        self.c = c

    @property
    def package_dir(self) -> str:
        """Return the temporary directory to stage the files to package."""
        return os.path.join(self.c.base_dir, "package")

    @property
    def libexec_dir(self) -> str:
        """Return the path to libexec in the temporary package directory.

        Auxiliary binaries are put here instead of bin/ so they do not end up
        replacing the system-provided versions if bin/ is added to $PATH.
        """
        return os.path.join(self.package_dir, "libexec")

    def _copy_recursive(self, src: str, *paths):
        src = os.path.join(src, *paths)
        dst = os.path.join(self.package_dir, *paths)
        shutil.copytree(src, dst)

    def _copy_to_libexec_and_strip(self, src: str):
        dst = os.path.join(self.libexec_dir, os.path.basename(src))
        shutil.copy(src, dst)
        strip(dst)

    def _copy_guile_files(self):
        # Copy needed files for Guile. Source files in share/ should go before
        # ccache in lib/ to avoid warnings.
        guile_install = guile.install_directory(self.c)
        self._copy_recursive(guile_install, "share", "guile")
        self._copy_recursive(
            guile_install, "lib", "guile", guile.major_version, "ccache"
        )

    def _copy_fontconfig_files(self):
        fontconfig_install = fontconfig.install_directory(self.c)
        self._copy_recursive(fontconfig_install, "etc", "fonts")

    def _copy_relocation_files(self):
        # Copy files for relocation.
        root_path = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
        relocate_src = os.path.join(root_path, "relocate")
        relocate_dst = os.path.join(self.package_dir, "etc", "relocate")
        shutil.copytree(relocate_src, relocate_dst)

    def _copy_python_files(self):
        # Copy packages for Python ...
        python_install = python.install_directory(self.c)
        python_libdir = python.python_with_major_version
        self._copy_recursive(python_install, "lib", python_libdir)

        # ... but delete a number of directories we don't need:
        python_libdir = os.path.join(self.package_dir, "lib", python_libdir)
        # First, delete all tests.
        tests_path = os.path.join(python_libdir, "**", "test")
        for directory in glob.glob(tests_path, recursive=True):
            shutil.rmtree(directory)
        # This directory contains the libpython*.a library.
        shutil.rmtree(glob.glob(os.path.join(python_libdir, "config-*"))[0])
        for directory in [
            # "Distributing Python Modules"
            "distutils",
            # "Integrated Development and Learning Environment"
            "idlelib",
            # 2to3
            "lib2to3",
        ]:
            shutil.rmtree(os.path.join(python_libdir, directory))

    def _create_wrapper(self, script: str, shebang: str, wrapper_template: str):
        bin_path = os.path.join(self.package_dir, "bin", script)
        libexec_path = os.path.join(self.libexec_dir, script)

        with open(bin_path) as orig:
            orig_lines = orig.readlines()
        new_lines = [shebang + "\n"] + orig_lines[1:]
        with open(libexec_path, "w") as new:
            new.writelines(new_lines)

        with open(bin_path, "w") as wrapper:
            wrapper.write(wrapper_template % script)

        chmod_x(bin_path)
        chmod_x(libexec_path)

    def _create_python_wrappers(self):
        python_interpreter = python.python_with_major_version
        python_shebang = "#!/usr/bin/env python3"
        python_wrapper_template = f"""#!/bin/sh
root="$(dirname $0)/.."
exec "$root/libexec/{python_interpreter}" "$root/libexec/%s" "$@"
"""

        for script in self.lilypond.python_scripts:
            self._create_wrapper(script, python_shebang, python_wrapper_template)

    def _create_guile_wrappers(self):
        guile_shebang = "#!/usr/bin/env -S guile --no-auto-compile"
        guile_version_major = guile.major_version
        guile_wrapper_template = f"""#!/bin/sh
root="$(dirname $0)/.."
export GUILE_AUTO_COMPILE=0
export GUILE_LOAD_PATH="$root/share/guile/{guile_version_major}"
export GUILE_LOAD_COMPILED_PATH="$root/lib/guile/{guile_version_major}/ccache"
exec "$root/libexec/guile" "$root/libexec/%s" "$@"
"""

        for script in self.lilypond.guile_scripts:
            self._create_wrapper(script, guile_shebang, guile_wrapper_template)

    def _copy_license_files(self):
        destination = os.path.join(self.package_dir, "licenses")
        os.makedirs(destination)

        for package in all_dependencies + [self.lilypond]:
            package.copy_license_files(destination, self.c)

    def prepare_package(self):
        """Prepare the package by copying all needed files into a temporary location."""
        # Start from scratch.
        if os.path.exists(self.package_dir):
            shutil.rmtree(self.package_dir)

        logging.debug("Copying files into '%s'...", self.package_dir)
        # Copy all of LilyPond, creating the base of the archive.
        lilypond_install = self.lilypond.install_directory(self.c)
        shutil.copytree(lilypond_install, self.package_dir)
        lilypond_exe = os.path.join(self.package_dir, "bin", "lilypond")
        strip(lilypond_exe)

        # Files needed to run core LilyPond.
        self._copy_guile_files()
        self._copy_fontconfig_files()
        self._copy_relocation_files()

        os.makedirs(self.libexec_dir)
        self._copy_to_libexec_and_strip(ghostscript.exe_path(self.c))

        # Files needed to run the scripts.
        self._copy_to_libexec_and_strip(guile.exe_path(self.c))
        self._copy_to_libexec_and_strip(python.exe_path(self.c))

        self._copy_python_files()

        # Move scripts to libexec, adapt their shebangs, and create wrappers.
        self._create_python_wrappers()
        self._create_guile_wrappers()

        self._copy_license_files()

    def package_tar(self):
        """Create a .tar.gz archive of the LilyPond binaries."""
        self.prepare_package()

        # Put the entire tree into a .tar.gz archive.
        archive = f"{self.lilypond.directory}-{self.c.platform.value}.tar.gz"
        archive_path = os.path.join(self.c.base_dir, archive)
        if os.path.exists(archive_path):
            os.remove(archive_path)

        logging.debug("Creating archive '%s'...", archive_path)

        def reset(tarinfo: tarfile.TarInfo):
            tarinfo.uid = tarinfo.gid = 0
            tarinfo.uname = tarinfo.gname = "root"
            return tarinfo

        with tarfile.open(archive_path, "x:gz") as tar:
            tar.add(
                self.package_dir,
                arcname=self.lilypond.directory,
                recursive=True,
                filter=reset,
            )
