Producing documents with LaTeX requires a good bit of infrastructure that
many developers don't have. I use an interactive gui-based tool called
TeXstudio, which is part of the TeX LIve distribution
(http://www.tug.org/texlive/). For this reason, I don't have a Makefile
for producing the geco*.pdf documents.

If you're familiar with using LaTeX to produce documents, it's relatively
straightforward to produce the GECO documentation, using either
command-line tools or a gui-based approach.

The top-level file is geco.tex. It will also be necessary to run BibTex to
produce the bibliography, and makeindex to produce the index.