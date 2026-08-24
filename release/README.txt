cclsh Linux x86-64 release

Run bin/cclsh from this directory. Keep bin, lib and libexec together: the
launchers use the bundled dynamic loader and matched CCL image.

The archive is self-contained and does not require Nix. It requires Linux on
x86-64 with a kernel version supported by the bundled glibc runtime.

bin/cclsh-fast is the optional experimental prewarmed launcher. Start with
bin/cclsh until you have read its documentation.
