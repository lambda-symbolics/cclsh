"""Check idle wakeups and signal-driven resizing of the Linux fast launcher."""

import fcntl
import os
from pathlib import Path
import pty
import select
import signal
import socket
import struct
import subprocess
import tempfile
import termios
import time


def switches(pid):
    """Read voluntary context switches, including timer-induced sleeps."""
    for line in Path(f"/proc/{pid}/status").read_text().splitlines():
        if line.startswith("voluntary_ctxt_switches:"):
            return int(line.split()[1])
    raise AssertionError(f"missing context-switch count for {pid}")


def worker_switches(pid):
    """Count voluntary switches across all threads in a Lisp worker."""
    return sum(switches(thread.name)
               for thread in Path(f"/proc/{pid}/task").iterdir())


def read_until(fd, marker):
    """Wait for terminal output with a bounded failure deadline."""
    output = bytearray()
    deadline = time.monotonic() + 10
    while marker not in output:
        remaining = deadline - time.monotonic()
        if remaining <= 0 or not select.select([fd], [], [], remaining)[0]:
            raise AssertionError(f"missing {marker!r}: {output!r}")
        output.extend(os.read(fd, 65536))
    return output


def main():
    """Exercise an isolated daemon and one attached terminal."""
    launcher = str(Path("cclsh-fast").resolve())
    with tempfile.TemporaryDirectory(prefix="cclsh-idle-") as temporary:
        environment = dict(os.environ, XDG_RUNTIME_DIR=temporary,
                           CCLSH_SAFE="1", TERM="xterm")
        command = [launcher, "daemon"]
        subprocess.run(command + ["start"], env=environment, check=True)
        master = None
        child = None
        try:
            with socket.socket(socket.AF_UNIX, socket.SOCK_SEQPACKET) as peer:
                peer.connect(temporary + "/cclsh-fast/supervisor.sock")
                daemon, _, _ = struct.unpack(
                    "3i", peer.getsockopt(socket.SOL_SOCKET,
                                         socket.SO_PEERCRED, 12))
            child, master = pty.fork()
            if child == 0:
                for number in signal.valid_signals():
                    if number not in (signal.SIGKILL, signal.SIGSTOP):
                        signal.signal(number, signal.SIG_DFL)
                os.execve(launcher, [launcher], environment)
            read_until(master, b"\x1b]133;B")
            # Let the replacement worker and initial output settle first.
            time.sleep(0.3)
            status = subprocess.check_output(command + ["status"],
                                             env=environment)
            assert b"active=1" in status, status
            workers = Path(f"/proc/{daemon}/task/{daemon}/children").read_text().split()
            worker_before = [worker_switches(pid) for pid in workers]
            before = [switches(pid) for pid in (daemon, child)]
            time.sleep(1.2)
            after = [switches(pid) for pid in (daemon, child)]
            delta = [end - start for start, end in zip(before, after)]
            assert all(count <= 2 for count in delta), delta
            print(f"Idle voluntary context switches (1.2 s): "
                  f"supervisor={delta[0]}, relay={delta[1]}")
            print("Lisp worker context switches (all threads):", [
                worker_switches(pid) - start
                for pid, start in zip(workers, worker_before)])
            fcntl.ioctl(master, termios.TIOCSWINSZ,
                        struct.pack("4H", 39, 101, 0, 0))
            # The newline causes the shell to query the propagated PTY size.
            os.write(master, b'(multiple-value-bind (r c) '
                     b'(cclsh::terminal-size) '
                     b'(format t "SIZE=~d,~d!~%" r c))\n')
            read_until(master, b"SIZE=39,101!")
            os.write(master, b"exit 0\n")
            read_until(master, b"\x1b]133;D")
        finally:
            subprocess.run(command + ["stop"], env=environment, check=True)
            if master is not None:
                os.close(master)
            if child is not None:
                os.waitpid(child, 0)


if __name__ == "__main__":
    main()
