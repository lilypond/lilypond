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

"""This module defines a package for LilyPond."""

import glob
import logging
import os
import shutil
import stat
import subprocess
import tarfile
from typing import Dict, List
import zipfile

from .build import Package, ConfigurePackage
from .config import Config
from .dependencies import all_dependencies
from .dependencies import (
    freetype,
    fontconfig,
    ghostscript,
    gettext,
    glib,
    guile,
    pango,
    libpng,
    cairo,
    python,
    embeddable_python,
)
from .fonts import all_fonts
from .fonts import texgyre, urwbase35


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
            info = tar.next()
            if info is None:
                raise ValueError("Specified archive is empty")
            directory = info.name
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

    def flexlexer_directory(self, c: Config) -> str:
        """Return the directory where FlexLexer.h is stored (for mingw)."""
        return os.path.join(self.build_directory(c), "FlexLexer")

    @property
    def archive(self) -> str:
        return os.path.basename(self._archive_path)

    def archive_path(self, c: Config) -> str:
        return self._archive_path

    @property
    def download_url(self) -> str:
        raise NotImplementedError

    def dependencies(self, c: Config) -> List[Package]:
        gettext_dep: List[Package] = []
        if c.is_freebsd() or c.is_macos() or c.is_mingw():
            gettext_dep = [gettext]
        python_dep: List[Package] = [python]
        if c.is_mingw():
            python_dep = [embeddable_python]
        return (
            gettext_dep
            + [
                freetype,
                fontconfig,
                ghostscript,
                glib,
                guile,
                pango,
                libpng,
                cairo,
            ]
            + python_dep
        )

    def configure_args_static(self, c: Config) -> List[str]:
        # LilyPond itself isn't a library!
        return []

    def build_env_extra(self, c: Config) -> Dict[str, str]:
        env = super().build_env_extra(c)
        env["GHOSTSCRIPT"] = ghostscript.exe_path(c.native_config)
        env["GUILE"] = guile.exe_path(c.native_config)
        env["PYTHON"] = python.exe_path(c.native_config)
        if c.is_freebsd() or c.is_macos() or c.is_mingw():
            env.update(gettext.get_env_variables(c))
        return env

    def configure_args(self, c: Config) -> List[str]:
        static = []
        if not c.is_macos():
            static = [
                # Include the static version of libstdc++.
                "--enable-static-gxx",
            ]

        flexlexer = []
        if c.is_mingw():
            flexlexer_dir = self.flexlexer_directory(c)
            flexlexer = [f"--with-flexlexer-dir={flexlexer_dir}"]

        return (
            static
            + [
                # Disable the documentation.
                "--disable-documentation",
                "--enable-cairo-backend",
            ]
            + flexlexer
        )

    def make_args(self, c: Config) -> List[str]:
        args = []
        if not c.is_mingw():
            args += ["all", "bytecode"]
        return args

    def make_install_args(self, c: Config) -> List[str]:
        args = []
        if not c.is_mingw():
            args += ["install-bytecode"]
        return args

    def build(self, c: Config) -> bool:
        # If mingw, copy FlexLexer.h from /usr/include and pass it to configure
        # because the cross-compiler would not find it.
        if c.is_mingw():
            flexlexer_dir = self.flexlexer_directory(c)
            os.makedirs(flexlexer_dir, exist_ok=True)
            shutil.copy("/usr/include/FlexLexer.h", flexlexer_dir)

        return super().build(c)

    @property
    def python_scripts(self) -> List[str]:
        """Return a list of all Python scripts installed by default."""
        return [
            "abc2ly",
            "convert-ly",
            "etf2ly",
            "lilymidi",
            "lilypond-book",
            "lilypond-invoke-editor",
            "lilysong",
            "midi2ly",
            "musicxml2ly",
        ]

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
    def bin_dir(self) -> str:
        """Return the path to bin in the temporary package directory."""
        return os.path.join(self.package_dir, "bin")

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

    def _copy_to_dir_and_strip(self, src: str, dst_dir: str):
        dst = os.path.join(dst_dir, os.path.basename(src))
        shutil.copy(src, dst)
        strip(dst)

    def _copy_to_libexec_and_strip(self, src: str):
        self._copy_to_dir_and_strip(src, self.libexec_dir)

    def _copy_to_bin_and_strip(self, src: str):
        self._copy_to_dir_and_strip(src, self.bin_dir)

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

    def _copy_font_files(self):
        share_lilypond = os.path.join(self.package_dir, "share", "lilypond")
        share_lilypond = os.path.join(share_lilypond, self.lilypond.version)
        fonts_otf = os.path.join(share_lilypond, "fonts", "otf")

        texgyre_install = texgyre.install_directory(self.c)
        for family in ["cursor", "heros", "schola"]:
            for style in ["regular", "bold", "bolditalic", "italic"]:
                otf = f"texgyre{family}-{style}.otf"
                src = os.path.join(texgyre_install, otf)
                dst = os.path.join(fonts_otf, otf)
                shutil.copy(src, dst)

        urwbase35_install = urwbase35.install_directory(self.c)
        for style in ["Roman", "BdIta", "Bold", "Italic"]:
            otf = f"C059-{style}.otf"
            src = os.path.join(urwbase35_install, otf)
            dst = os.path.join(fonts_otf, otf)
            shutil.copy(src, dst)

        for family in ["NimbusMonoPS", "NimbusSans"]:
            for style in ["Regular", "Bold", "BoldItalic", "Italic"]:
                otf = f"{family}-{style}.otf"
                src = os.path.join(urwbase35_install, otf)
                dst = os.path.join(fonts_otf, otf)
                shutil.copy(src, dst)

    def _copy_relocation_files(self):
        # Copy files for relocation.
        root_path = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
        relocate_src = os.path.join(root_path, "relocate")
        relocate_dst = os.path.join(self.package_dir, "etc", "relocate")
        shutil.copytree(relocate_src, relocate_dst)

    def _copy_native_bytecode(self):
        # Copy compiled Guile bytecode from the native LilyPond build.
        lilypond_install = self.lilypond.install_directory(self.c.native_config)
        src = os.path.join(lilypond_install, "lib", "lilypond")
        dst = os.path.join(self.package_dir, "lib", "lilypond")
        shutil.copytree(src, dst)

        # Touch the files so that compiled bytecode of generated files (such as
        # font-encodings.scm) works correctly.
        for root, _, files in os.walk(dst):
            for name in files:
                os.utime(os.path.join(root, name), None)

    def _copy_mingw_files(self):
        self._copy_native_bytecode()

        # Copy shared Dlls for mingw.
        gettext_install = gettext.install_directory(self.c)
        libintl_dll = os.path.join(gettext_install, "bin", "libintl-8.dll")
        self._copy_to_bin_and_strip(libintl_dll)

        glib_install = glib.install_directory(self.c)
        for lib in [
            "libgio-2.0-0.dll",
            "libglib-2.0-0.dll",
            "libgmodule-2.0-0.dll",
            "libgobject-2.0-0.dll",
        ]:
            self._copy_to_bin_and_strip(os.path.join(glib_install, "bin", lib))

        # Copy helper executable for spawning from glib.
        gspawn = os.path.join(glib_install, "bin", "gspawn-win64-helper-console.exe")
        self._copy_to_bin_and_strip(gspawn)

    def _copy_mingw_python(self):
        python_install = embeddable_python.install_directory(self.c)
        for python_file in os.listdir(python_install):
            shutil.copy(os.path.join(python_install, python_file), self.bin_dir)

    def _move_scripts(self):
        for script in self.lilypond.python_scripts:
            src = os.path.join(self.bin_dir, script)
            dst = os.path.join(self.bin_dir, f"{script}.py")
            os.rename(src, dst)

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
        bin_path = os.path.join(self.bin_dir, script)
        libexec_path = os.path.join(self.libexec_dir, script)

        with open(bin_path, "r", encoding="utf-8") as orig:
            orig_lines = orig.readlines()
        new_lines = [shebang + "\n"] + orig_lines[1:]
        with open(libexec_path, "w", encoding="utf-8") as new:
            new.writelines(new_lines)

        with open(bin_path, "w", encoding="utf-8") as wrapper:
            wrapper.write(wrapper_template % script)

        chmod_x(bin_path)
        chmod_x(libexec_path)

    def _create_python_wrappers(self):
        python_interpreter = python.python_with_major_version
        python_shebang = "#!/usr/bin/env python3"
        python_wrapper_template = f"""#!/bin/sh
script="$0"
if test -L "$script"; then
    # readlink is not POSIX, but present on all platforms we care about.
    # On macOS, it does not support the recursive following of symlinks,
    # so we only resolve one link and hope for the best.
    script="$(readlink -f "$script" 2>/dev/null || readlink "$script")"
fi
root="$(dirname "$script")/.."
exec "$root/libexec/{python_interpreter}" "$root/libexec/%s" "$@"
"""

        for script in self.lilypond.python_scripts:
            self._create_wrapper(script, python_shebang, python_wrapper_template)

    def _copy_license_files(self):
        destination = os.path.join(self.package_dir, "licenses")
        os.makedirs(destination)

        for package in all_dependencies + all_fonts + [self.lilypond]:
            if not package.enabled(self.c):
                continue
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
        lilypond_exe = f"lilypond{self.c.program_suffix}"
        lilypond_exe = os.path.join(self.bin_dir, lilypond_exe)
        strip(lilypond_exe)

        # Files needed to run core LilyPond.
        self._copy_guile_files()
        self._copy_fontconfig_files()
        self._copy_font_files()
        self._copy_relocation_files()
        if self.c.is_mingw():
            self._copy_mingw_files()

        os.makedirs(self.libexec_dir)
        self._copy_to_libexec_and_strip(ghostscript.exe_path(self.c))

        if self.c.is_mingw():
            self._copy_mingw_python()
            # Move scripts to have proper extensions.
            self._move_scripts()
        else:
            # Files needed to run the scripts.
            self._copy_to_libexec_and_strip(python.exe_path(self.c))
            self._copy_python_files()

            # Move scripts to libexec, adapt their shebangs, and create wrappers.
            self._create_python_wrappers()

        self._copy_license_files()

    def package_tar(self):
        """Create a .tar.gz archive of the LilyPond binaries."""
        self.prepare_package()

        # Put the entire tree into a .tar.gz archive.
        platform = self.c.platform.value
        architecture = self.c.architecture
        archive = f"{self.lilypond.directory}-{platform}-{architecture}.tar.gz"
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

    def package_zip(self):
        """Create a .zip archive of the LilyPond binaries (for mingw)."""
        self.prepare_package()

        # Put the entire tree into a .zip archive.
        platform = self.c.platform.value
        architecture = self.c.architecture
        archive = f"{self.lilypond.directory}-{platform}-{architecture}.zip"
        archive_path = os.path.join(self.c.base_dir, archive)
        if os.path.exists(archive_path):
            os.remove(archive_path)

        logging.debug("Creating archive '%s'...", archive_path)

        # Adapted from zipfile.py.
        def add_to_zip(zip_archive, path, zippath):
            if os.path.isfile(path):
                zip_archive.write(path, zippath, zipfile.ZIP_DEFLATED)
            elif os.path.isdir(path):
                if zippath:
                    zip_archive.write(path, zippath)
                for name in sorted(os.listdir(path)):
                    add_to_zip(
                        zip_archive,
                        os.path.join(path, name),
                        os.path.join(zippath, name),
                    )
            # else: ignore

        with zipfile.ZipFile(archive_path, "w") as zip_archive:
            package_dir = self.package_dir
            lilypond_dir = self.lilypond.directory
            # Create an entry for the root directory.
            zip_archive.write(package_dir, lilypond_dir)

            dir_names = sorted(os.listdir(package_dir))
            # Windows remembers if an archive was downloaded from the internet,
            # and doesn't apply the timestamps if extracted via the Windows
            # Explorer. This breaks Guile bytecode because the .scm files in
            # share/ are extracted after the .go files in lib/. Move lib/ last
            # to work around this issue.
            dir_names = [name for name in dir_names if name != "lib"] + ["lib"]
            for name in dir_names:
                add_to_zip(
                    zip_archive,
                    os.path.join(package_dir, name),
                    os.path.join(lilypond_dir, name),
                )
