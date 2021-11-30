set sysroot
source /home/richyliu/ctf/pwndbg/gdbinit.py
set syntax-highlight-style arduino
set follow-fork-mode child

# context-sections: which context sections are displayed (controls order)
# default: regs disasm code ghidra stack backtrace expressions
set context-sections regs disasm code ghidra stack expressions
