from subprocess import PIPE, Popen
import sys

if __name__ == "__main__":
    n = sys.argv[1]
    sp = Popen(f"dune exec ./eiopipe.exe -- {n}", shell=True, bufsize=4096, stdout=PIPE)

    while sp.poll() is None:
        sp.stdout.read() #type:ignore
