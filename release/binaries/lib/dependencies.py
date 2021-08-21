"""This module defines all run-time dependencies of LilyPond."""

# Each class in this file represents a package, ignore missing docstrings.
# pylint: disable=missing-class-docstring

import os
import re
import shutil
from typing import Dict, List

from .build import Package, ConfigurePackage, MesonPackage
from .config import Config


class Expat(ConfigurePackage):
    @property
    def version(self) -> str:
        return "2.4.1"

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


class FreeType(ConfigurePackage):
    @property
    def version(self) -> str:
        return "2.11.0"

    @property
    def directory(self) -> str:
        return f"freetype-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        return f"https://download.savannah.gnu.org/releases/freetype/{self.archive}"

    def configure_args(self, c: Config) -> List[str]:
        return [
            # Disable compression options.
            "--with-zlib=no",
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


class UtilLinux(ConfigurePackage):
    @property
    def version(self) -> str:
        return "2.37.1"

    @property
    def directory(self) -> str:
        return f"util-linux-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        major_version = ".".join(self.version.split(".")[0:2])
        return f"https://www.kernel.org/pub/linux/utils/util-linux/v{major_version}/{self.archive}"

    def configure_args(self, c: Config) -> List[str]:
        return [
            # Enable only libuuid.
            "--disable-all-programs",
            "--enable-libuuid",
            # Fix build with GCC 4.8.5 on CentOS 7.
            "CFLAGS=-std=c99 -O2",
        ]

    @property
    def license_files(self) -> List[str]:
        return [os.path.join("Documentation", "licenses", "COPYING.BSD-3-Clause")]

    def copy_license_files(self, destination: str, c: Config):
        super().copy_license_files(destination, c)

        # Copy manually to prepend "libuuid".
        src = os.path.join(self.src_directory(c), "libuuid", "COPYING")
        dst = os.path.join(destination, f"{self.directory}.libuuid.COPYING")
        shutil.copy(src, dst)

    def __str__(self) -> str:
        return f"util-linux {self.version}"


util_linux = UtilLinux()


class Fontconfig(ConfigurePackage):
    @property
    def version(self) -> str:
        return "2.13.1"

    @property
    def directory(self) -> str:
        return f"fontconfig-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.bz2"

    @property
    def download_url(self) -> str:
        return f"https://www.freedesktop.org/software/fontconfig/release/{self.archive}"

    def apply_patches(self, c: Config):
        def patch_makefile(content: str) -> str:
            return content.replace("po-conf test", "po-conf")

        self.patch_file(c, "Makefile.in", patch_makefile)

        def patch_uuid_header(content: str) -> str:
            return content.replace("uuid/uuid.h", "uuid.h")

        self.patch_file(c, "configure", patch_uuid_header)
        self.patch_file(c, os.path.join("src", "fccache.c"), patch_uuid_header)
        self.patch_file(c, os.path.join("src", "fchash.c"), patch_uuid_header)

    @property
    def dependencies(self) -> List[Package]:
        return [expat, freetype, util_linux]

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
        return "9.54.0"

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
        # Also remove some Resource directories to reduce the size.
        for resource_dir in ["Font", "CIDFont", "CIDFSubst", "CMap"]:
            dirs.append(os.path.join("Resource", resource_dir))

        for unused in dirs:
            shutil.rmtree(os.path.join(self.src_directory(c), unused))

    @property
    def dependencies(self) -> List[Package]:
        return [freetype]

    @property
    def configure_default_static(self) -> bool:
        # Ghostscript doesn't have --disable-shared nor --enable-static.
        return False

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
            "--disable-openjpeg",
            "--without-cal",
            "--without-ijs",
            "--without-jbig2dec",
            "--without-libidn",
            "--without-libpaper",
            "--without-libtiff",
            "--without-pdftoraster",
            "--without-urf",
            "--without-x",
        ]

    @property
    def make_args(self) -> List[str]:
        return ["PS_FONT_DEPS=", "GS_LIB_DEFAULT="]

    @property
    def make_install_args(self) -> List[str]:
        return ["PS_FONT_DEPS="]

    def exe_path(self, c: Config) -> str:
        """Return path to gs executable."""
        return os.path.join(self.install_directory(c), "bin", "gs")

    @property
    def license_files(self) -> List[str]:
        return ["LICENSE", os.path.join("doc", "COPYING")]

    def __str__(self) -> str:
        return f"Ghostscript {self.version}"


ghostscript = Ghostscript()


class Libffi(ConfigurePackage):
    @property
    def version(self) -> str:
        return "3.4.2"

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


class Zlib(ConfigurePackage):
    @property
    def version(self) -> str:
        return "1.2.11"

    @property
    def directory(self) -> str:
        return f"zlib-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        return f"https://www.zlib.net/{self.archive}"

    @property
    def configure_default_static(self) -> bool:
        # zlib uses the non-default argument --static, see below.
        return False

    def configure_args(self, c: Config) -> List[str]:
        return ["--static"]

    def copy_license_files(self, destination: str, c: Config):
        readme_path = os.path.join(self.src_directory(c), "README")
        with open(readme_path) as readme:
            notice = readme.readlines()[-38:]
        readme_path = os.path.join(destination, f"{self.directory}.README")
        with open(readme_path, "w") as readme:
            readme.writelines(notice)

    def __str__(self) -> str:
        return f"zlib {self.version}"


zlib = Zlib()


class GLib(MesonPackage):
    @property
    def version(self) -> str:
        return "2.68.3"

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

    @property
    def dependencies(self) -> List[Package]:
        return [libffi, zlib]

    def meson_args(self, c: Config) -> List[str]:
        return [
            # Use the internal PCRE library to avoid an external dependency.
            "-Dinternal_pcre=true",
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
        return "8.0.4"

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
        # Already fixed for future releases:
        # https://github.com/ivmai/bdwgc/commit/d2c4444fea498114b892bc65887221e36aa1e3a1
        def patch_pkgconfig(content: str) -> str:
            return re.sub("Cflags:.*", "\\g<0> -pthread", content)

        self.patch_file(c, "bdw-gc.pc.in", patch_pkgconfig)

    def configure_args(self, c: Config) -> List[str]:
        return ["--disable-docs"]

    def copy_license_files(self, destination: str, c: Config):
        readme_path = os.path.join(self.src_directory(c), "README.md")
        with open(readme_path) as readme:
            notice = readme.readlines()[-48:]
        readme_path = os.path.join(destination, f"{self.directory}.README")
        with open(readme_path, "w") as readme:
            readme.writelines(notice)

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

        readme_path = os.path.join(self.src_directory(c), "README")
        with open(readme_path) as readme:
            notice = readme.readlines()[0:27]
        readme_path = os.path.join(destination, f"{self.directory}.README")
        with open(readme_path, "w") as readme:
            readme.writelines(notice)

    def __str__(self) -> str:
        return f"GMP {self.version}"


gmp = GMP()


class Libtool(ConfigurePackage):
    @property
    def version(self) -> str:
        return "2.4.6"

    @property
    def directory(self) -> str:
        return f"libtool-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        return f"https://ftp.gnu.org/gnu/libtool/{self.archive}"

    @property
    def license_files(self) -> List[str]:
        return ["COPYING"]

    def __str__(self) -> str:
        return f"Libtool {self.version}"


libtool = Libtool()


class Libunistring(ConfigurePackage):
    @property
    def version(self) -> str:
        return "0.9.10"

    @property
    def directory(self) -> str:
        return f"libunistring-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        return f"https://ftp.gnu.org/gnu/libunistring/{self.archive}"

    @property
    def license_files(self) -> List[str]:
        return ["COPYING.LIB"]

    def copy_license_files(self, destination: str, c: Config):
        super().copy_license_files(destination, c)

        readme_path = os.path.join(self.src_directory(c), "README")
        with open(readme_path) as readme:
            notice = readme.readlines()[47:63]
        readme_path = os.path.join(destination, f"{self.directory}.README")
        with open(readme_path, "w") as readme:
            readme.writelines(notice)

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
        return f"https://ftp.gnu.org/gnu/guile/{self.archive}"

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

    @property
    def dependencies(self) -> List[Package]:
        return [bdwgc, libffi, libtool, libunistring, gmp]

    def configure_args(self, c: Config) -> str:
        gmp_install_dir = gmp.install_directory(c)
        libunistring_install_dir = libunistring.install_directory(c)
        libtool_install_dir = libtool.install_directory(c)
        return [
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

    def exe_path(self, c: Config) -> str:
        """Return path to the guile interpreter."""
        return os.path.join(self.install_directory(c), "bin", "guile")

    @property
    def license_files(self) -> List[str]:
        return ["LICENSE", "COPYING.LESSER"]

    def __str__(self) -> str:
        return f"Guile {self.version}"


guile = Guile()


class HarfBuzz(MesonPackage):
    @property
    def version(self) -> str:
        return "2.8.2"

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

    @property
    def dependencies(self) -> List[Package]:
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
        return "1.0.10"

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
        return "1.48.7"

    @property
    def directory(self) -> str:
        return f"pango-{self.version}"

    @property
    def archive(self) -> str:
        return f"{self.directory}.tar.xz"

    @property
    def download_url(self) -> str:
        major_version = ".".join(self.version.split(".")[0:2])
        return f"http://ftp.gnome.org/pub/GNOME/sources/pango/{major_version}/{self.archive}"

    @property
    def dependencies(self) -> List[Package]:
        return [fontconfig, freetype, fribidi, glib, harfbuzz]

    def build_env(self, c: Config) -> Dict[str, str]:
        env = super().build_env(c)
        glib_bin = os.path.join(glib.install_directory(c), "bin")
        env["PATH"] = f"{glib_bin}{os.pathsep}{env['PATH']}"
        return env

    def meson_args(self, c: Config) -> List[str]:
        return [
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


class Python(ConfigurePackage):
    @property
    def version(self) -> str:
        return "3.9.6"

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
        with open(setup_py, "w"):
            pass

        def patch_setup(content: str) -> str:
            for module in [
                "array",
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
                # Needed for zipfile
                "binascii",
                "_struct",
            ]:
                content = content.replace("#" + module, module)
            return content

        self.patch_file(c, os.path.join("Modules", "Setup"), patch_setup)

    def configure_args(self, c: Config) -> str:
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

all_dependencies: List[Package] = [
    expat,
    freetype,
    util_linux,
    fontconfig,
    ghostscript,
    libffi,
    zlib,
    glib,
    bdwgc,
    gmp,
    libtool,
    libunistring,
    guile,
    harfbuzz,
    fribidi,
    pango,
    python,
]
