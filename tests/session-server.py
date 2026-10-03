"""Exercise multiple real terminals attached to one SBCL shell image."""

import os
from pathlib import Path
import pty
import select
import signal
import socket
import subprocess
import sys
import tempfile
import termios
import time

ROOT = Path(__file__).resolve().parent.parent
LAUNCHER = str(ROOT / "scripts/cclshd")
PROMPT = b"\x1b]133;B"


class Terminal:
    def __init__(self, environment):
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            for number in signal.valid_signals():
                if number not in (signal.SIGKILL, signal.SIGSTOP):
                    signal.signal(number, signal.SIG_DFL)
            os.execve(LAUNCHER, [LAUNCHER, "attach"], environment)
        os.set_inheritable(self.fd, False)
        self.until(PROMPT)

    def until(self, marker, timeout=15):
        output = bytearray()
        deadline = time.monotonic() + timeout
        while marker not in output:
            remaining = deadline - time.monotonic()
            if remaining <= 0 or not select.select([self.fd], [], [], remaining)[0]:
                raise AssertionError(f"missing {marker!r}: {output!r}")
            output.extend(os.read(self.fd, 65536))
        return bytes(output)

    def command(self, text):
        os.write(self.fd, text.encode() + b"\n")
        return self.until(PROMPT)

    def close(self):
        if self.fd is not None:
            os.close(self.fd)
            self.fd = None
            os.waitpid(self.pid, 0)

    def foreground_job(self):
        """Wait for terminal handoff, not a machine-speed-dependent delay."""
        deadline = time.monotonic() + 10
        while time.monotonic() < deadline:
            if (os.tcgetpgrp(self.fd) != self.pid and
                    termios.tcgetattr(self.fd)[3] & termios.ISIG):
                return
            time.sleep(0.02)
        raise AssertionError("external job did not acquire the terminal")


def context_switches(pid):
    """Count voluntary context switches across all Linux threads."""
    total = 0
    for thread in Path(f"/proc/{pid}/task").iterdir():
        for line in (thread / "status").read_text().splitlines():
            if line.startswith("voluntary_ctxt_switches:"):
                total += int(line.split()[1])
    return total


def main():
    with tempfile.TemporaryDirectory(prefix="cclsh-session-") as directory:
        environment = dict(os.environ, XDG_RUNTIME_DIR=directory,
                           XDG_CONFIG_HOME=directory + "/safe-config",
                           CCLSH_SAFE="1", TERM="xterm")
        subprocess.run([LAUNCHER, "start"], env=environment, check=True, timeout=45)
        terminals = []
        try:
            status = subprocess.check_output([LAUNCHER, "status"], env=environment)
            server_pid = int(status.split(b"pid=")[1].split()[0])
            one = Terminal(environment)
            terminals.append(one)
            two = Terminal(environment)
            terminals.append(two)
            for terminal in terminals:
                output = terminal.command('(format t "PID=~d!~%" (sb-posix:getpid))')
                assert f"PID={server_pid}!".encode() in output, output
            print("Two terminals share SBCL pid", server_pid, flush=True)
            assert subprocess.run([LAUNCHER, "attach", "-c", "exit 23"],
                                  env=environment, timeout=20).returncode == 23

            for index, terminal in enumerate(terminals):
                cwd = Path(directory) / f"session-{index}"
                cwd.mkdir()
                terminal.command(f'cd {cwd}')
                terminal.command(f'(setenv "SESSION_MARK" "{index}")')
            for index, terminal in enumerate(terminals):
                output = terminal.command('/bin/sh -c \'printf "MARK=%s CWD=%s!\\n" "$SESSION_MARK" "$PWD"\'')
                assert f"MARK={index} CWD={directory}/session-{index}!".encode() in output, output
                output = terminal.command('/bin/pwd')
                assert f"{directory}/session-{index}".encode() in output, output
                # Programs started from Lisp (prompt helpers, libraries) are
                # forked by the server, but must still see the session's state.
                output = terminal.command(
                    '(format t "LCWD=~a!~%" (string-right-trim (list #\\Newline)'
                    ' (uiop:run-program (list "/bin/pwd") :output :string)))')
                assert f"LCWD={directory}/session-{index}!".encode() in output, output
                output = terminal.command(
                    '(format t "LMARK=~a!~%" (string-right-trim (list #\\Newline)'
                    ' (uiop:run-program (list "/bin/sh" "-c" "echo $SESSION_MARK")'
                    ' :output :string)))')
                assert f"LMARK={index}!".encode() in output, output
            print("Cwd and child environments are independent", flush=True)

            for form in ('(error "recoverable failure")', '(list . .)',
                         '(defun invalid-definition (x x) x)'):
                one.command(form)
                assert b"STILL-ALIVE!" in two.command('(format t "STILL-ALIVE!~%")')
            one.command('(defun shared-probe () 321)')
            assert b"VALUE=321!" in two.command('(format t "VALUE=~d!~%" (shared-probe))')
            output = one.command('(pipe (echo "abc") (rev))')
            assert b"cba" in output, output
            # A short-lived group leader must remain joinable until every
            # pipeline stage has been spawned (especially on NetBSD).
            for _ in range(20):
                output = one.command("/bin/sh -c 'exit 0' | /bin/sh -c 'exit 0' | /bin/sh -c 'exit 0'")
                assert b"cclsh:" not in output, output
            one.command('(pipe (echo "a") (sh "-c" "printf PIPE-$SESSION_MARK"))')
            print("Reader, evaluation and compiler errors stay in their session", flush=True)

            os.write(one.fd, b"/bin/sleep 30\n")
            one.until(b"\x1b]133;C")
            one.foreground_job()
            os.write(one.fd, b"\x1a")
            one.until(PROMPT)
            one.command("bg")
            os.write(one.fd, b"fg\n")
            one.until(b"\x1b]133;C")
            one.foreground_job()
            os.write(one.fd, b"\x03")
            one.until(PROMPT)
            assert b"OTHER-SESSION!" in two.command('(format t "OTHER-SESSION!~%")')
            print("Ctrl-Z, bg, fg and Ctrl-C preserve both sessions", flush=True)

            one.command('(pipe (echo "saved") (to "local-output"))')
            assert (Path(directory) / "session-0/local-output").read_text().strip() == "saved"
            one.command('(+ 10 20)')
            two.command('(+ 40 50)')
            assert b"LAST=30!" in one.command('(format t "LAST=~d!~%" *)')

            # A malformed packet must only close its own connection.
            endpoint = f"{directory}/cclshd-{os.getuid()}/server.sock"
            with socket.socket(socket.AF_UNIX, socket.SOCK_SEQPACKET) as bad:
                bad.connect(endpoint)
                bad.send(b"invalid packet")
                assert bad.recv(32) == b""
            assert b"PROTOCOL-OK!" in two.command('(format t "PROTOCOL-OK!~%")')

            if sys.platform.startswith("linux") and Path(f"/proc/{server_pid}/task").exists():
                time.sleep(0.4)
                pids = [server_pid, one.pid, two.pid]
                before = [context_switches(pid) for pid in pids]
                time.sleep(1.2)
                delta = [context_switches(pid) - start for pid, start in zip(pids, before)]
                assert all(count <= 2 for count in delta), delta
                print("Idle switches over 1.2 s (SBCL, client A, client B):", delta, flush=True)

            # Losing one frontend leaves the other session and server usable.
            one.close()
            terminals.remove(one)
            assert b"DISCONNECT-OK!" in two.command('(format t "DISCONNECT-OK!~%")')
            one = Terminal(environment)
            terminals.append(one)

            one.command('(in-package :cl-user)')
            assert b"PACKAGE=CCLSH-USER!" in two.command(
                '(format t "PACKAGE=~a!~%" (package-name *package*))')

            # Startup settings remain local and a broken startup is recoverable.
            configured = dict(environment)
            configured.pop("CCLSH_SAFE")
            configured["XDG_CONFIG_HOME"] = directory + "/configured"
            config = Path(configured["XDG_CONFIG_HOME"]) / "cclsh"
            config.mkdir(parents=True)
            (config / "startup.lisp").write_text(
                '(setf *prompt-function* (lambda (&key &allow-other-keys) "CUSTOM> "))\n')
            third = Terminal(configured)
            terminals.append(third)
            output = third.command('(format t "CONFIG-OK!~%")')
            assert b"CUSTOM> " in output and b"CONFIG-OK!" in output, output
            assert b"CUSTOM> " not in two.command('(values)')
            third.close()
            terminals.remove(third)
            (config / "startup.lisp").write_text('(defun broken-startup (')
            third = Terminal(configured)
            terminals.append(third)
            assert b"STARTUP-OK!" in third.command('(format t "STARTUP-OK!~%")')
            third.close()
            terminals.remove(third)
            print("Prompt settings and broken startup files stay local", flush=True)
            subprocess.run([LAUNCHER, "stop"], env=environment, check=True, timeout=10)
            for terminal in terminals:
                terminal.close()
            terminals.clear()
            # A terminal also falls back when the daemon is unavailable.
            fallback = Terminal(environment)
            terminals.append(fallback)
            output = fallback.command('(format t "FALLBACK=~d!~%" (sb-posix:getpid))')
            assert f"FALLBACK={fallback.pid}!".encode() in output, output
            os.write(fallback.fd, b"exit\n")
            fallback.close()
            terminals.clear()
            print("Shared session checks passed", flush=True)
        finally:
            subprocess.run([LAUNCHER, "stop"], env=environment,
                           stdout=subprocess.DEVNULL, timeout=10)
            for terminal in terminals:
                terminal.close()


if __name__ == "__main__":
    main()
