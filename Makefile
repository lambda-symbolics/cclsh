PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin
DESTDIR ?=
SHELLS_FILE ?= /etc/shells
LOGIN_USER ?=
PROBE_USER ?=
CCL ?= ccl
CCL_IMAGE ?=
CCL_SOURCE ?= ../ccl

.PHONY: build fast system-shell-build login-build ccl-kernel check sbcl-check integration-check install install-fast install-system-shell install-login-shell

build:
	CCLSH_CCL="$(CCL)" CCLSH_CCL_IMAGE="$(CCL_IMAGE)" scripts/build

fast:
	scripts/build-fast

system-shell-build: CCL = $(CCL_SOURCE)/lx86cl64
system-shell-build:
	CCLSH_CCL_IMAGE="$(CCL_IMAGE)" scripts/login-build \
		"$(CCL_SOURCE)" "$(CCL)" cclsh.attestation

login-build: system-shell-build

ccl-kernel:
	scripts/ccl-kernel "$(CCL_SOURCE)"

check:
	CCLSH_CCL="$(CCL)" CCLSH_CCL_IMAGE="$(CCL_IMAGE)" scripts/check

sbcl-check:
	CCLSH_SBCL="$(SBCL)" scripts/sbcl-check

integration-check:
	@set -e; \
	verify_current() { \
		kernel=$$(realpath -e cclsh); \
		scripts/verify-attestation \
			"$$kernel" "$$kernel.image" cclsh.attestation; \
	}; \
	if test -f cclsh.attestation; then \
		attested=1; \
		verify_current; \
	else \
		attested=0; \
		$(MAKE) build; \
	fi; \
	CCLSH_CCL="$(CCL)" CCLSH_CCL_IMAGE="$(CCL_IMAGE)" \
		scripts/integration-check; \
	if test "$$attested" -eq 1; then verify_current; fi

install:
	@if test -z "$(DESTDIR)" && test "$$(id -u)" -eq 0; then \
		echo "owner-only root install refused; use install-system-shell" >&2; \
		exit 2; \
	fi
	CCLSH_SKIP_BUILD=1 CCLSH_INSTALL_DIRECTORY="$(DESTDIR)$(BINDIR)" \
		scripts/install

install-fast: fast
	@test -x "$(DESTDIR)$(BINDIR)/cclsh" || { \
		echo "install-fast requires cclsh in $(DESTDIR)$(BINDIR)" >&2; \
		exit 1; \
	}
	install -m 755 cclsh-fast "$(DESTDIR)$(BINDIR)/cclsh-fast"

install-system-shell:
	@if test -n "$(DESTDIR)"; then \
		echo "install-system-shell does not support DESTDIR" >&2; \
		exit 2; \
	fi
	CCLSH_SKIP_BUILD=1 \
	CCLSH_INSTALL_DIRECTORY="$(BINDIR)" \
	CCLSH_SYSTEM_SHELL=1 \
	CCLSH_PROBE_USER="$(PROBE_USER)" \
	CCLSH_SHELLS_FILE="$(SHELLS_FILE)" \
	CCLSH_BUILD_ATTESTATION="$(CURDIR)/cclsh.attestation" \
		scripts/install

install-login-shell:
	@echo "install-login-shell is deprecated; installing one shared system shell" >&2
	@$(MAKE) --no-print-directory install-system-shell \
		BINDIR="$(BINDIR)" SHELLS_FILE="$(SHELLS_FILE)" \
		PROBE_USER="$(if $(PROBE_USER),$(PROBE_USER),$(LOGIN_USER))"
