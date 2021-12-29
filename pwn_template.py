#!/usr/bin/env python3

from pwn import *

{bindings}

context.binary = {bin_name}

gdbscript = """
continue
"""

def conn():
    if args.REMOTE:
        r = remote("addr", 1337)
    elif args.GDB:
        return gdb.debug({proc_args}, gdbscript=gdbscript)
    else:
        r = process({proc_args})

    return r


def main():
    r = conn()

    # good luck pwning :)

    r.interactive()


if __name__ == "__main__":
    main()
