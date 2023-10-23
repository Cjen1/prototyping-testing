from subprocess import PIPE, Popen
import sys

if __name__ == "__main__":
    n = sys.argv[1]
    read = (sys.argv[2]).lower()
    sp = Popen(f"dune exec ./eiopipe.exe -- {n}", shell=True, pipesize=4096, bufsize=4096, stdout=PIPE)

    if read == 'true':
        while sp.poll() is None:
            sp.stdout.read() #type:ignore

    sp.wait()
