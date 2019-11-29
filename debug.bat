@echo off
cls
hatari -W -D --debug-except all,autostart %1.tos
