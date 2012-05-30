
From http://www.martyn.se/code/emacs/darkroom-mode/

Emacs darkroom-mode and w32-fullscreen

Darkroom-mode is a "pseudo" mode for Emacs on Windows that tries to emulate the no-distractions writing environment in applications like "WriteRoom" and "DarkRoom". It's Windows only since going fullscreen in X is handled natively by most X window managers.

Darkroom mode enables "real" fullscreen mode on a frame, i.e, taking away even the titlebar. It also sets some useful margin- and color settings. It works with multiple monitors (i.e, multiple frames)

There is a screenshot available (here used together with Emacs Muse).

Source code available through bitbucket - darkroom-mode and w32-fullscreen

Download

Latest version available for download is v20090816

Files and short descriptions

w32-fullscreen.el - The module that handles going fullscreen on MS Windows
w32toggletitle.exe - .NET application that switches a windows' titlebar and frame on/off (requires .NET framework 2.0).
frame-local-vars.el - Module enabling "frame local" variables. Needed in darkroom-mode for remembering margins across buffers.
darkroom-mode.el - Additional to going fullscreen, this module sets some useful margins and other frame-properties to emulate the "DarkRoom" environment.
Requirements

MS Windows. Only support for MS Windows since going fullscreen in X is handled by most window managers anyway.
Microsoft .NET Framework Version 2.0
Also, it might require EmacsW32 available at http://www.ourcomments.org/Emacs/EmacsW32.html (I haven't tested without it yet)

Installation

First, see that you meet the requirements.

Next, download the latest distribution, place darkroom-mode.el, w32-fullscreen.el and frame-local-vars.el in a good place. Put w32toggletitle.exe somewhere inside your PATH, like C:\windows\system32.

Edit your .emacs and add the usual:

(add-to-list 'load-path "path-to-where-you-put-darkroom-mode")
(require 'darkroom-mode)

Usage

Use M-x darkroom-mode to toggle darkroom-mode on/off

If you only want fullscreen and not the other stuff, use M-x w32-fullscreen (toggles fullscreen on/off)

Questions & Feedback

Report issues and send patches through the projects at bitbucket:
darkroom-mode
w32-fullscreen
Changelog

v20090816, 2009-08-16 - changed from python-based titleToggler to .NET, small fixes, moved to bitbucket.
v0.2, 2008-02-14 - added multiple monitor support
v0.1, 2008-02-12 - initial release