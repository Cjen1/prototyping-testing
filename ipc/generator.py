import subprocess
import time
import os

from struct import pack

duration = 10
rate = 10000

client = subprocess.Popen(['sudo','mnexec', '-da', str(os.getpid())] + ['./client'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
subprocess.Popen(['tee', 'stdout.log'], stdin = client.stdout, stdout=None)
subprocess.Popen(['tee', 'stderr.log'], stdin = client.stderr, stdout=None)


def send(client, op):
    cli = client
    size = pack('<L',len(op))
    cli.stdin.write(size)
    payload = op
    cli.stdin.write(payload)

start = time.time()
end = start + duration


inc = 1.0 / rate
op_start = start + inc
while(time.time() < end):
    #Busy wait until time for op
    while time.time() < op_start:
        pass
    op_start += inc
    print("sending", time.time())
    send(client, b'asdf')

print("CR: Finished")
stop_flag.value = True
