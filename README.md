# GECO
You can download prebuilt tarballs of the source and documentation PDFs from here:
  http://hiwaay.net/~gpw/geco/geco.html

Genetic Evolution through Combination of Objects (GECO), version 2.1

GECO is an extensible, object-oriented framework for prototyping genetic
algorithms in Common Lisp. GECO makes extensive use of CLOS, the Common
Lisp Object System, to implement its functionality. The abstractions
provided by the classes have been chosen with the intent both of being
easily understandable to anyone familiar with the paradigm of genetic
algorithms, and of providing the algorithm developer with the ability to
customize all aspects of its operation. It comes with extensive
documentation, in the form of a PostScript(tm) file, and some simple
examples are also provided to illustrate its intended use.

The files available include the following:
    GECO-README.txt      Basic information about the software (this file)
    GECO-pdf.tgz         Unix-compressed documentation
      This archive includes:
       - geco.pdf, a normal printable PDF file
       - geco-hyperlinked.pdf, a hyperlinked version of geco.pdf,
         which is handy for online reading, but not as nice for printing
    GECO-v2.1.tgz        Unix-compressed source code
     This archive also includes:
       - CHANGES.txt
       - COPYING.LIB-2.0

Copyright (C) 1992,1993,2020  George P. W. Williams, Jr.

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

Author:
    George P. W. Williams, Jr.
    1334 Columbus City Rd.
    Scottsboro, AL 35769
    george.p.williams@pobox.com

This is an updated version as of 2020 (the original version has been long-neglected;
it hasn't been updated since 1993). The current version has been updated to use the
ASDF system definition/build system, and the files have all been converted to use
unix line endings (line-feeds; previous versions used old the old MacOS convention
of carriage-returns). I'm also now providing the documentation in PDF format
(instead of PostScript in the earlier versions).

Bug reports, improvements, and feature requests should be sent to the author at the
contact information above. I will try to respond to them.
Ports to other lisps are also welcome.

I have gone to a lot of effort to make GECO portable among CLtL2 compliant Common Lisps.
I tested the previous versions on several different lisp implemenations (MCL 2.0, ACL 4.1,
and LCL 4.1), but I currently only have Clozure CL (macOS 10.15.7), so that's all I can
vouch for at this time.

TO USE THIS SOFTWARE
 - There is detailed documentation (~100 pages!) in geco.pdf
   There is also a hyperlinked version: geco-hyperlinked.pdf
   Both of these files are contained in the GECO-pdf.tgz archive
 - Look at the system definition file geco.asd for some notes on how
   to load GECO. You should have a working installation of ASDF.
   (see https://common-lisp.net/project/asdf/)
   You will probably need to do something like this:
   (push #P"/Users/george/lisp/GECO-dev/" asdf:*central-registry*)
   and (asdf:load-system :geco)
 - There are some conditional compilation options. See packages.lisp for
   more information.
 - Some examples are provided to illustrate GECO's intended use.
   See the documentation for details.
