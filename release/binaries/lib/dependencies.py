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

"""This module defines all run-time dependencies of LilyPond."""

# Each class in this file represents a package, ignore missing docstrings.
# pylint: disable=missing-class-docstring

import logging
import os
import re
import shutil
from typing import Dict, List

from .build import Package, ConfigurePackage, MesonPackage
from .config import Config


def copy_slice(src: str, dst: str, lines: slice):
    """Copy a slice of lines from src to dst."""
    with open(src, "r", encoding="utf-8") as src_file:
        content = src_file.readlines()
    content = content[lines]
    with open(dst, "w", encoding="utf-8") as dst_file:
        dst_file.writelines(content)


class Expat(ConfigurePackage):
    @property
    def version(self) -> str:
        return "2.4.9"

    @property
    def directory(self) -> str:
        return f"expat-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        version = self.version.replace(".", "_")
        return f"https://github.com/libexpat/libexpat/releases/download/R_{version}/{self.archive}"

    def configure_args(self, c: Config) -> List[str]:
        return [
            # Disable unneeded components.
            "--without-xmlwf",
            "--without-examples",
            "--without-tests",
            "--without-docbook",
        ]

    @property
    def license_files(self) -> List[str]:
        return ["COPYING"]

    def __str__(self) -> str:
        return f"Expat {self.version}"


expat = Expat()


class Zlib(ConfigurePackage):
    @property
    def version(self) -> str:
        return "1.2.13"

    @property
    def directory(self) -> str:
        return f"zlib-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        return f"https://www.zlib.net/{self.archive}"

    def apply_patches(self, c: Config):
        def patch_configure(content: str) -> str:
            return content.replace("leave 1", "")

        self.patch_file(c, "configure", patch_configure)

    def build_env_extra(self, c: Config) -> Dict[str, str]:
        env = super().build_env_extra(c)
        if c.is_mingw():
            env["CHOST"] = c.triple
        return env

    def configure_args_triples(self, c: Config) -> List[str]:
        # Cross-compilation is enabled via the CHOST environment variable.
        return []

    def configure_args_static(self, c: Config) -> List[str]:
        return ["--static"]

    def get_env_variables(self, c: Config) -> Dict[str, str]:
        """Return environment variables to make zlib available."""
        zlib_install = self.install_directory(c)
        return {
            "CPATH": os.path.join(zlib_install, "include"),
            # Cannot use LIBRARY_PATH because it is only used if GCC is built
            # as a native compiler, so it doesn't work for mingw.
            "LDFLAGS": "-L" + os.path.join(zlib_install, "lib"),
        }

    def copy_license_files(self, destination: str, c: Config):
        readme_src = os.path.join(self.src_directory(c), "README")
        readme_dst = os.path.join(destination, f"{self.directory}.README")
        copy_slice(readme_src, readme_dst, slice(-38, None))

    def __str__(self) -> str:
        return f"zlib {self.version}"


zlib = Zlib()


class FreeType(ConfigurePackage):
    @property
    def version(self) -> str:
        return "2.12.1"

    @property
    def directory(self) -> str:
        return f"freetype-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        return f"https://download.savannah.gnu.org/releases/freetype/{self.archive}"

    def dependencies(self, c: Config) -> List[Package]:
        return [zlib]

    def configure_args(self, c: Config) -> List[str]:
        return [
            "--with-zlib=yes",
            "--with-bzip2=no",
            "--with-png=no",
            "--with-brotli=no",
            # Disable unused harfbuzz.
            "--with-harfbuzz=no",
        ]

    @property
    def license_files(self) -> List[str]:
        return ["LICENSE.TXT", os.path.join("docs", "GPLv2.TXT")]

    def __str__(self) -> str:
        return f"FreeType {self.version}"


freetype = FreeType()


class Fontconfig(ConfigurePackage):
    @property
    def version(self) -> str:
        return "2.14.0"

    @property
    def directory(self) -> str:
        return f"fontconfig-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        return f"https://www.freedesktop.org/software/fontconfig/release/{self.archive}"

    def dependencies(self, c: Config) -> List[Package]:
        return [expat, freetype]

    def configure_args(self, c: Config) -> List[str]:
        return ["--disable-docs"]

    @property
    def license_files(self) -> List[str]:
        return ["COPYING"]

    def __str__(self) -> str:
        return f"Fontconfig {self.version}"


fontconfig = Fontconfig()


class Ghostscript(ConfigurePackage):
    @property
    def version(self) -> str:
        return "9.56.1"

    @property
    def directory(self) -> str:
        return f"ghostscript-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.gz"

    @property
    def download_url(self) -> str:
        # pylint: disable=line-too-long
        url_version = self.version.replace(".", "")
        return f"https://github.com/ArtifexSoftware/ghostpdl-downloads/releases/download/gs{url_version}/{self.archive}"

    def apply_patches(self, c: Config):
        # Remove unused bundled sources to disable their build.
        dirs = ["tesseract", "leptonica"]

        for unused in dirs:
            shutil.rmtree(os.path.join(self.src_directory(c), unused))

    def dependencies(self, c: Config) -> List[Package]:
        return [freetype]

    def configure_args_static(self, c: Config) -> List[str]:
        # Ghostscript doesn't have --disable-shared nor --enable-static.
        return []

    def configure_args(self, c: Config) -> List[str]:
        return [
            # Only enable drivers needed for LilyPond.
            "--disable-contrib",
            "--disable-dynamic",
            "--with-drivers=PNG,PS",
            # Disable unused dependencies and features.
            "--disable-cups",
            "--disable-dbus",
            "--disable-fontconfig",
            "--disable-gtk",
            "--without-cal",
            "--without-ijs",
            "--without-libidn",
            "--without-libpaper",
            "--without-libtiff",
            "--without-pdftoraster",
            "--without-urf",
            "--without-x",
        ]

    def exe_path(self, c: Config) -> str:
        """Return path to gs executable."""
        gs_exe = f"gs{c.program_suffix}"
        return os.path.join(self.install_directory(c), "bin", gs_exe)

    @property
    def license_files(self) -> List[str]:
        return ["LICENSE", os.path.join("doc", "COPYING")]

    def __str__(self) -> str:
        return f"Ghostscript {self.version}"


ghostscript = Ghostscript()


class Gettext(ConfigurePackage):
    def enabled(self, c: Config) -> bool:
        return c.is_freebsd() or c.is_macos() or c.is_mingw()

    @property
    def version(self) -> str:
        return "0.21.1"

    @property
    def directory(self) -> str:
        return f"gettext-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        return f"https://ftpmirror.gnu.org/gnu/gettext/{self.archive}"

    def apply_patches(self, c: Config):
        # localcharset.c defines locale_charset, which is also provided by
        # Guile. However, Guile has a modification to this file so we really
        # need to build that version.
        def patch_makefile(content: str) -> str:
            return content.replace("localcharset.lo", "")

        makefile = os.path.join("gettext-runtime", "intl", "Makefile.in")
        self.patch_file(c, makefile, patch_makefile)

        def patch_dcigettext(content: str) -> str:
            return content.replace("locale_charset ()", "NULL")

        dcigettext = os.path.join("gettext-runtime", "intl", "dcigettext.c")
        self.patch_file(c, dcigettext, patch_dcigettext)

    @property
    def configure_script(self) -> str:
        return os.path.join("gettext-runtime", "configure")

    def configure_args_static(self, c: Config) -> List[str]:
        if c.is_mingw():
            # On mingw, we need to build glib as shared libraries for DllMain to
            # work. This also requires a shared libintl.dll to ensure there is
            # exactly one copy of the variables and code.
            return ["--enable-shared", "--disable-static"]
        return super().configure_args_static(c)

    def configure_args(self, c: Config) -> List[str]:
        return [
            # Disable building with libiconv in case the library is installed
            # on the system (as is the case for FreeBSD and macOS).
            "am_cv_func_iconv=no",
            # Disable unused features.
            "--disable-java",
            "--disable-threads",
        ]

    @property
    def macos_ldflags(self):
        """Return additional linker flags for macOS."""
        return "-Wl,-framework -Wl,CoreFoundation"

    def get_env_variables(self, c: Config) -> Dict[str, str]:
        """Return environment variables to make libintl available."""
        gettext_install = self.install_directory(c)

        # Cannot use LIBRARY_PATH because it is only used if GCC is built
        # as a native compiler, so it doesn't work for mingw.
        ldflags = "-L" + os.path.join(gettext_install, "lib")
        if c.is_macos():
            ldflags += " " + self.macos_ldflags

        return {
            "CPATH": os.path.join(gettext_install, "include"),
            "LDFLAGS": ldflags,
        }

    @property
    def license_files(self) -> List[str]:
        return [
            os.path.join("gettext-runtime", "COPYING"),
            os.path.join("gettext-runtime", "intl", "COPYING.LIB"),
        ]

    def __str__(self) -> str:
        return f"gettext {self.version}"


gettext = Gettext()


class Libffi(ConfigurePackage):
    @property
    def version(self) -> str:
        return "3.4.3"

    @property
    def directory(self) -> str:
        return f"libffi-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.gz"

    @property
    def download_url(self) -> str:
        return f"https://github.com/libffi/libffi/releases/download/v{self.version}/{self.archive}"

    @property
    def license_files(self) -> List[str]:
        return ["LICENSE"]

    def __str__(self) -> str:
        return f"libffi {self.version}"


libffi = Libffi()


class PCRE(ConfigurePackage):
    @property
    def version(self) -> str:
        return "8.45"

    @property
    def directory(self) -> str:
        return f"pcre-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.bz2"

    @property
    def download_url(self) -> str:
        return f"https://sourceforge.net/projects/pcre/files/pcre/{self.version}/{self.archive}"

    @property
    def license_files(self) -> List[str]:
        return ["LICENCE"]

    def __str__(self) -> str:
        return f"PCRE {self.version}"


pcre = PCRE()


class GLib(MesonPackage):
    @property
    def version(self) -> str:
        return "2.72.4"

    @property
    def directory(self) -> str:
        return f"glib-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        major_version = ".".join(self.version.split(".")[0:2])
        return f"https://download.gnome.org/sources/glib/{major_version}/{self.archive}"

    def dependencies(self, c: Config) -> List[Package]:
        gettext_dep: List[Package] = []
        if c.is_freebsd() or c.is_macos() or c.is_mingw():
            gettext_dep = [gettext]
        return gettext_dep + [libffi, pcre, zlib]

    def build_env_extra(self, c: Config) -> Dict[str, str]:
        env = super().build_env_extra(c)
        if c.is_freebsd() or c.is_macos() or c.is_mingw():
            # Make meson find libintl.
            env.update(gettext.get_env_variables(c))
        return env

    def meson_args_static(self, c: Config) -> List[str]:
        if c.is_mingw():
            # The libraries rely on DllMain which doesn't work with static.
            return ["--default-library=shared"]
        return super().meson_args_static(c)

    def meson_args(self, c: Config) -> List[str]:
        return [
            # Disable unused features and tests.
            "-Dlibmount=disabled",
            "-Dtests=false",
            "-Dxattr=false",
        ]

    @property
    def license_files(self) -> List[str]:
        return ["COPYING"]

    def __str__(self) -> str:
        return f"GLib {self.version}"


glib = GLib()


class Bdwgc(ConfigurePackage):
    @property
    def version(self) -> str:
        return "8.2.2"

    @property
    def directory(self) -> str:
        return f"gc-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.gz"

    @property
    def download_url(self) -> str:
        return f"https://www.hboehm.info/gc/gc_source/{self.archive}"

    def apply_patches(self, c: Config):
        # For lack of a better method for finding static data sections in shared
        # libraries, bdwgc on Windows scans all memory pages and (temporarily)
        # adds them to the root set. This leads to problems and sporadic crashes
        # if a memory page disappears while marking. Until a proper solution is
        # found, disable this entire mechanism because by design of statically
        # linking both bdwgc and libguile, we can guarantee that all root sets
        # relevant for garbage collection are in the data section of the main
        # executable, which can be conveniently located by looking at a single
        # static variable and determining the surrounding pages.
        # Upstream issue: https://github.com/ivmai/bdwgc/issues/454
        def disable_win32_dlls(content: str) -> str:
            return content.replace("GC_no_win32_dlls = FALSE", "GC_no_win32_dlls = TRUE")

        self.patch_file(c, "os_dep.c", disable_win32_dlls)

    def configure_args(self, c: Config) -> List[str]:
        return [
            "--disable-docs",
            # Enable large config, optimizing for heap sizes larger than a few
            # 100 MB and allowing more heap sections needed on Windows for huge
            # scores.
            "--enable-large-config",
            # Fix cross-compilation for mingw.
            "--with-libatomic-ops=none",
        ]

    def copy_license_files(self, destination: str, c: Config):
        readme_src = os.path.join(self.src_directory(c), "README.md")
        readme_dst = os.path.join(destination, f"{self.directory}.README")
        copy_slice(readme_src, readme_dst, slice(-48, None))

    def __str__(self) -> str:
        return f"bdwgc {self.version}"


bdwgc = Bdwgc()


class GMP(ConfigurePackage):
    @property
    def version(self) -> str:
        return "6.2.1"

    @property
    def directory(self) -> str:
        return f"gmp-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        return f"https://gmplib.org/download/gmp/{self.archive}"

    @property
    def license_files(self) -> List[str]:
        return ["COPYING.LESSERv3"]

    def copy_license_files(self, destination: str, c: Config):
        super().copy_license_files(destination, c)

        readme_src = os.path.join(self.src_directory(c), "README")
        readme_dst = os.path.join(destination, f"{self.directory}.README")
        copy_slice(readme_src, readme_dst, slice(0, 27))

    def __str__(self) -> str:
        return f"GMP {self.version}"


gmp = GMP()


class Libiconv(ConfigurePackage):
    def enabled(self, c: Config) -> bool:
        return c.is_mingw()

    @property
    def version(self) -> str:
        return "1.17"

    @property
    def directory(self) -> str:
        return f"libiconv-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.gz"

    @property
    def download_url(self) -> str:
        return f"https://ftpmirror.gnu.org/gnu/libiconv/{self.archive}"

    @property
    def license_files(self) -> List[str]:
        return ["COPYING"]

    def __str__(self) -> str:
        return f"libiconv {self.version}"


libiconv = Libiconv()


class Libtool(ConfigurePackage):
    @property
    def version(self) -> str:
        return "2.4.7"

    @property
    def directory(self) -> str:
        return f"libtool-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        return f"https://ftpmirror.gnu.org/gnu/libtool/{self.archive}"

    @property
    def license_files(self) -> List[str]:
        return ["COPYING"]

    def __str__(self) -> str:
        return f"Libtool {self.version}"


libtool = Libtool()


class Libunistring(ConfigurePackage):
    @property
    def version(self) -> str:
        return "1.0"

    @property
    def directory(self) -> str:
        return f"libunistring-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        return f"https://ftpmirror.gnu.org/gnu/libunistring/{self.archive}"

    def dependencies(self, c: Config) -> List[Package]:
        libiconv_dep: List[Package] = []
        if c.is_mingw():
            libiconv_dep = [libiconv]
        return libiconv_dep

    def configure_args(self, c: Config) -> List[str]:
        mingw_args = []
        if c.is_mingw():
            libiconv_install_dir = libiconv.install_directory(c)
            mingw_args = [
                f"--with-libiconv-prefix={libiconv_install_dir}",
            ]

        return ["--disable-threads"] + mingw_args

    @property
    def license_files(self) -> List[str]:
        return ["COPYING.LIB"]

    def copy_license_files(self, destination: str, c: Config):
        super().copy_license_files(destination, c)

        readme_src = os.path.join(self.src_directory(c), "README")
        readme_dst = os.path.join(destination, f"{self.directory}.README")
        copy_slice(readme_src, readme_dst, slice(47, 63))

    def __str__(self) -> str:
        return f"libunistring {self.version}"


libunistring = Libunistring()


class Guile(ConfigurePackage):
    @property
    def version(self) -> str:
        return "2.2.7"

    @property
    def major_version(self) -> str:
        """Return Guile's major version, used in the directory structure."""
        return ".".join(self.version.split(".")[0:2])

    @property
    def directory(self) -> str:
        return f"guile-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        return f"https://ftpmirror.gnu.org/gnu/guile/{self.archive}"

    def _apply_patches_mingw(self, c: Config):
        # Fix the build.
        def patch_start_child(content: str) -> str:
            return content.replace("int start_child", "pid_t start_child")

        posix_w32_h = os.path.join("libguile", "posix-w32.h")
        self.patch_file(c, posix_w32_h, patch_start_child)

        # TODO: Find proper solution...
        def patch_gethostname(content: str) -> str:
            return "\n".join(
                [
                    line
                    for line in content.split("\n")
                    if "gethostname_used_without_requesting" not in line
                ]
            )

        unistd_in_h = os.path.join("lib", "unistd.in.h")
        self.patch_file(c, unistd_in_h, patch_gethostname)

        # Fix conversion of large long values.
        def patch_conversion(content: str) -> str:
            return content.replace(
                "SIZEOF_TYPE < SIZEOF_SCM_T_BITS", "SIZEOF_TYPE < SIZEOF_LONG"
            )

        for conv in ["conv-integer.i.c", "conv-uinteger.i.c"]:
            self.patch_file(c, os.path.join("libguile", conv), patch_conversion)

        # Fix headers so compilation of LilyPond works.
        def patch_iselect(content: str) -> str:
            return content.replace("sys/select.h", "winsock2.h")

        iselect_h = os.path.join("libguile", "iselect.h")
        self.patch_file(c, iselect_h, patch_iselect)

        def patch_null_threads(content: str) -> str:
            content = content.replace(" sigset_t", " _sigset_t")
            content = re.sub("return sigprocmask.*", "return 0;", content)
            return content

        null_threads_h = os.path.join("libguile", "null-threads.h")
        self.patch_file(c, null_threads_h, patch_null_threads)

        def patch_numbers(content: str) -> str:
            return "\n".join(
                [line for line in content.split("\n") if "copysign" not in line]
            )

        numbers_h = os.path.join("libguile", "numbers.h")
        self.patch_file(c, numbers_h, patch_numbers)

    def apply_patches(self, c: Config):
        # Fix configure on CentOS7 to not look in lib64.
        def patch_configure(content: str) -> str:
            return content.replace("=lib64", "=lib")

        self.patch_file(c, "configure", patch_configure)

        # Explicitly list static archive to prevent pkgconfig on CentOS7 from
        # reordering the library items.
        def patch_pkgconfig(content: str) -> str:
            return content.replace(
                "-lguile-@GUILE_EFFECTIVE_VERSION@",
                "${libdir}/libguile-@GUILE_EFFECTIVE_VERSION@.a",
            )

        pkgconfig = os.path.join("meta", f"guile-{self.major_version}.pc.in")
        self.patch_file(c, pkgconfig, patch_pkgconfig)

        # Fix non-portable invocation of inplace sed.
        def patch_inplace_sed(content: str) -> str:
            return content.replace("$(SED) -i", "$(SED)")

        libguile_makefile_in = os.path.join("libguile", "Makefile.in")
        self.patch_file(c, libguile_makefile_in, patch_inplace_sed)

        if c.is_mingw():
            self._apply_patches_mingw(c)

    def dependencies(self, c: Config) -> List[Package]:
        gettext_dep: List[Package] = []
        if c.is_freebsd() or c.is_macos() or c.is_mingw():
            gettext_dep = [gettext]
        libiconv_dep: List[Package] = []
        if c.is_mingw():
            libiconv_dep = [libiconv]
        return gettext_dep + libiconv_dep + [bdwgc, libffi, libtool, libunistring, gmp]

    def build_env_extra(self, c: Config) -> Dict[str, str]:
        env = super().build_env_extra(c)
        if c.is_macos():
            # We don't need the full get_env_variables because we can pass
            # --with-libintl-prefix= via the arguments to configure.
            env["LDFLAGS"] = gettext.macos_ldflags
        return env

    def configure_args(self, c: Config) -> List[str]:
        gmp_install_dir = gmp.install_directory(c)
        libunistring_install_dir = libunistring.install_directory(c)
        libtool_install_dir = libtool.install_directory(c)

        libintl_args = []
        if c.is_freebsd() or c.is_macos() or c.is_mingw():
            gettext_install_dir = gettext.install_directory(c)
            libintl_args = [
                f"--with-libintl-prefix={gettext_install_dir}",
            ]

        mingw_args = []
        if c.is_mingw():
            guile_for_build = self.exe_path(c.native_config)
            libiconv_install_dir = libiconv.install_directory(c)
            mingw_args = [
                f"GUILE_FOR_BUILD={guile_for_build}",
                f"--with-libiconv-prefix={libiconv_install_dir}",
            ]

        return (
            [
                # Disable unused parts of Guile.
                "--without-threads",
                "--disable-networking",
                # Disable -Werror to enable builds with newer compilers.
                "--disable-error-on-warning",
                # Make configure find the statically built dependencies.
                f"--with-libgmp-prefix={gmp_install_dir}",
                f"--with-libltdl-prefix={libtool_install_dir}",
                f"--with-libunistring-prefix={libunistring_install_dir}",
                # Prevent that configure searches for libcrypt.
                "ac_cv_search_crypt=no",
            ]
            + libintl_args
            + mingw_args
        )

    def exe_path(self, c: Config) -> str:
        """Return path to the guile interpreter."""
        guile_exe = f"guile{c.program_suffix}"
        return os.path.join(self.install_directory(c), "bin", guile_exe)

    @property
    def license_files(self) -> List[str]:
        return ["LICENSE", "COPYING.LESSER"]

    def __str__(self) -> str:
        return f"Guile {self.version}"


guile = Guile()


class HarfBuzz(MesonPackage):
    @property
    def version(self) -> str:
        return "4.4.1"

    @property
    def directory(self) -> str:
        return f"harfbuzz-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        # pylint: disable=line-too-long
        return f"https://github.com/harfbuzz/harfbuzz/releases/download/{self.version}/{self.archive}"

    def dependencies(self, c: Config) -> List[Package]:
        return [freetype]

    def meson_args(self, c: Config) -> List[str]:
        return [
            # Enable FreeType, but disable tests.
            "-Dfreetype=enabled",
            "-Dtests=disabled",
        ]

    @property
    def license_files(self) -> List[str]:
        return ["COPYING"]

    def __str__(self) -> str:
        return f"HarfBuzz {self.version}"


harfbuzz = HarfBuzz()


class FriBidi(ConfigurePackage):
    @property
    def version(self) -> str:
        return "1.0.12"

    @property
    def directory(self) -> str:
        return f"fribidi-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        # pylint: disable=line-too-long
        return f"https://github.com/fribidi/fribidi/releases/download/v{self.version}/{self.archive}"

    @property
    def license_files(self) -> List[str]:
        return ["COPYING"]

    def __str__(self) -> str:
        return f"FriBiDi {self.version}"


fribidi = FriBidi()


class Pango(MesonPackage):
    @property
    def version(self) -> str:
        return "1.50.11"

    @property
    def directory(self) -> str:
        return f"pango-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        major_version = ".".join(self.version.split(".")[0:2])
        # fmt: off
        return f"https://download.gnome.org/sources/pango/{major_version}/{self.archive}"
        # fmt: on

    def apply_patches(self, c: Config):
        # Disable tests, fail to build on FreeBSD.
        def patch_meson_build(content: str) -> str:
            # Disable unused parts (tests fail to build on FreeBSD, utils and tools on macOS)
            for subdir in ["tests", "tools", "utils"]:
                content = content.replace(f"subdir('{subdir}')", "")
            return content

        self.patch_file(c, "meson.build", patch_meson_build)

    def dependencies(self, c: Config) -> List[Package]:
        return [fontconfig, freetype, fribidi, glib, harfbuzz]

    def build_env_extra(self, c: Config) -> Dict[str, str]:
        env = super().build_env_extra(c)
        glib_bin = os.path.join(glib.install_directory(c), "bin")
        env["PATH"] = f"{glib_bin}{os.pathsep}{os.environ['PATH']}"
        return env

    def meson_args(self, c: Config) -> List[str]:
        return [
            # Disable Cairo, which is enabled by default since 1.50.3.
            "-Dcairo=disabled",
            # Enable Fontconfig and FreeType.
            "-Dfontconfig=enabled",
            "-Dfreetype=enabled",
        ]

    @property
    def license_files(self) -> List[str]:
        return ["COPYING"]

    def __str__(self) -> str:
        return f"Pango {self.version}"


pango = Pango()


class Libpng(ConfigurePackage):
    @property
    def version(self) -> str:
        return "1.6.38"

    @property
    def directory(self) -> str:
        return f"libpng-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        return f"https://downloads.sourceforge.net/libpng/{self.archive}"

    def dependencies(self, c: Config) -> List[Package]:
        return [zlib]

    def build_env_extra(self, c: Config) -> Dict[str, str]:
        env = super().build_env_extra(c)
        env.update(zlib.get_env_variables(c))
        return env

    @property
    def license_files(self) -> List[str]:
        return ["LICENSE"]

    def __str__(self) -> str:
        return f"libpng {self.version}"


libpng = Libpng()


class Pixman(MesonPackage):
    @property
    def version(self) -> str:
        return "0.40.0"

    @property
    def directory(self) -> str:
        return f"pixman-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.gz"

    @property
    def download_url(self) -> str:
        return f"https://www.cairographics.org/releases/{self.archive}"

    def apply_patches(self, c: Config):
        # Disable tests, they fail to build on macOS.
        def patch_meson_build(content: str) -> str:
            return content.replace("subdir('test')", "")

        self.patch_file(c, "meson.build", patch_meson_build)

    @property
    def license_files(self) -> List[str]:
        return ["COPYING"]

    def __str__(self) -> str:
        return f"pixman {self.version}"


pixman = Pixman()


class Cairo(ConfigurePackage):
    @property
    def version(self) -> str:
        return "1.16.0"

    @property
    def directory(self) -> str:
        return f"cairo-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        return f"https://www.cairographics.org/releases/{self.archive}"

    def dependencies(self, c: Config) -> List[Package]:
        return [zlib, freetype, fontconfig, libpng, pixman]

    def build_env_extra(self, c: Config) -> Dict[str, str]:
        env = super().build_env_extra(c)
        env.update(zlib.get_env_variables(c))
        # Cairo sets this by default, but doesn't work on Mingw.
        env["CFLAGS"] = "-Wp,-U_FORTIFY_SOURCE"
        return env

    def configure_args(self, c: Config) -> List[str]:
        return [
            # ordering follows Cairo's configure --help output
            "--enable-xlib=no",
            "--enable-xlib-xrender=no",
            "--enable-xcb=no",
            "--enable-xcb-shm=no",
            "--enable-quartz=no",
            "--enable-quartz-font=no",
            "--enable-win32=no",
            "--enable-win32-font=no",
            "--enable-egl=no",
            "--enable-glx=no",
            "--enable-wgl=no",
            "--enable-script=no",
            "--enable-ft=yes",
            "--enable-fc=yes",
            "--enable-ps=yes",
            "--enable-pdf=yes",
            "--enable-svg=yes",
        ]

    @property
    def license_files(self) -> List[str]:
        return ["COPYING"]

    def __str__(self) -> str:
        return f"cairo {self.version}"


cairo = Cairo()


PYTHON_VERSION = "3.10.8"


class Python(ConfigurePackage):
    def enabled(self, c: Config) -> bool:
        # For Windows, we are using the embeddable package because it's
        # impossible to cross-compile Python without tons of patches...
        return not c.is_mingw()

    @property
    def version(self) -> str:
        return PYTHON_VERSION

    @property
    def major_version(self) -> str:
        """Return Python's major version, used in the executable name."""
        return ".".join(self.version.split(".")[0:2])

    @property
    def python_with_major_version(self) -> str:
        """Return the string 'python' with the major version."""
        return f"python{self.major_version}"

    @property
    def directory(self) -> str:
        return f"Python-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        return f"https://www.python.org/ftp/python/{self.version}/{self.archive}"

    def apply_patches(self, c: Config):
        # setup.py tries to build extension based on software installed in the
        # global system directories, and there is no option to build some of
        # them selectively. Instead we empty the script and enable what we need
        # in Modules/Setup below. This has the additional advantage that the
        # modules are built statically into libpython and not dynamically loaded
        # from lib-dynload/.
        setup_py = os.path.join(self.src_directory(c), "setup.py")
        with open(setup_py, "w", encoding="utf-8"):
            pass

        def patch_setup(content: str) -> str:
            for module in [
                "array",
                "fcntl",
                "math",
                # Needed for fractions
                "_contextvars",
                # Needed for hashlib
                "_md5",
                "_sha1",
                "_sha256",
                "_sha512",
                "_sha3",
                "_blake2",
                # Needed for subprocess
                "_posixsubprocess",
                "select",
                # Needed for tempfile
                "_random",
                # Needed for xml
                "pyexpat",
                # Needed for zipfile
                "binascii",
                "_struct",
                "zlib",
            ]:
                content = content.replace("#" + module, module)
            return content

        self.patch_file(c, os.path.join("Modules", "Setup"), patch_setup)

    def dependencies(self, c: Config) -> List[Package]:
        return [zlib]

    def build_env_extra(self, c: Config) -> Dict[str, str]:
        env = super().build_env_extra(c)
        env.update(zlib.get_env_variables(c))
        return env

    def configure_args(self, c: Config) -> List[str]:
        return [
            "--with-ensurepip=no",
            # Prevent that configure searches for libcrypt.
            "ac_cv_search_crypt=no",
            "ac_cv_search_crypt_r=no",
        ]

    def exe_path(self, c: Config) -> str:
        """Return path to the python3 interpreter."""
        exe = self.python_with_major_version
        return os.path.join(self.install_directory(c), "bin", exe)

    @property
    def license_files(self) -> List[str]:
        return ["LICENSE"]

    def __str__(self) -> str:
        return f"Python {self.version}"


python = Python()


class EmbeddablePython(Package):
    def enabled(self, c: Config) -> bool:
        return c.is_mingw()

    @property
    def version(self) -> str:
        return PYTHON_VERSION

    @property
    def directory(self) -> str:
        return f"python-{self.version}-embed-amd64"

    @property
    def archive(self) -> str:
        return f"{self.directory}.zip"

    @property
    def download_url(self) -> str:
        return f"https://www.python.org/ftp/python/{self.version}/{self.archive}"

    def prepare_sources(self, c: Config) -> bool:
        src_directory = self.src_directory(c)
        if os.path.exists(src_directory):
            logging.debug("'%s' already extracted", self.archive)
            return True

        archive = self.archive_path(c)
        if not os.path.exists(archive):
            logging.error("'%s' does not exist!", self.archive)
            return False

        # The archive has no directory, directly extract into src_directory.
        shutil.unpack_archive(archive, self.src_directory(c))

        return True

    def build(self, c: Config):
        src_directory = self.src_directory(c)
        install_directory = self.install_directory(c)

        os.makedirs(install_directory, exist_ok=True)

        # NB: Without a dot between the two numbers!
        major_version = "".join(self.version.split(".")[0:2])
        python_with_major_version = f"python{major_version}"

        # Copy over the needed files from the downloaded archive.
        for filename in [
            "python.exe",
            f"{python_with_major_version}.dll",
            f"{python_with_major_version}.zip",
            # For parsing (Music)XML
            "pyexpat.pyd",
        ]:
            shutil.copy(os.path.join(src_directory, filename), install_directory)

        return True

    @property
    def license_files(self) -> List[str]:
        return ["LICENSE.txt"]

    def __str__(self) -> str:
        return f"Python {self.version} (embeddable package)"


embeddable_python = EmbeddablePython()

all_dependencies: List[Package] = [
    expat,
    zlib,
    freetype,
    fontconfig,
    ghostscript,
    gettext,
    libffi,
    pcre,
    glib,
    bdwgc,
    gmp,
    libiconv,
    libtool,
    libunistring,
    guile,
    harfbuzz,
    fribidi,
    pango,
    libpng,
    pixman,
    cairo,
    python,
    embeddable_python,
]
