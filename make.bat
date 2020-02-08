vasmm68k_mot_win32.exe -m68000 -Felf -noesc -quiet -no-opt %1.s -o %1.o
vlink -bataritos -tos-flags 7 %1.o -o c:\emu\steem\hd\%1.tos

rem vasm -m68000 -Ftos %1.s -o %1.tos
