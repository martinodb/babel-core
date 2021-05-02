Babel - The all-in-one toolkit for multi-agent experiments on emergent communication
======

# _About_

 (From [the Babel website](https://emergent-languages.org/)):
> Babel is a flexible toolkit for implementing and running agent-based experiments on emergent communication. The Babel system includes advanced modules for constructional language processing and learning (Fluid Construction Grammar),
> conceptualising and interpreting procedural semantic structures (Incremental Recruitment Language), and conducting multi-agent experiments in simulated environments or using physical robots.
> An extensive monitoring system opens up every detail of Babel’s intermediate representations and underlying dynamics. A modular design ensures that the system can be used in a wide variety of scenarios.
> It is therefore possible to use each component individually, according to your needs.
> Babel is written in Common Lisp and runs in most major Lisp implementations (CCL, SBCL and LispWorks) on all major platforms (Linux, Mac OS X, Windows).
> The core packages of Babel, jointly developed by Sony Computer Science Laboratories Paris and Vrije Universiteit Brussel under an Apache 2.0 license, can be downloaded here. A version of the Babel system containing additional packages
> and functionalities can be obtained for free by filling out the form below.

Note: I have no connection with the Babel dev team, I'm just mirroring their open source software for convenience, among other reasons because their Github repository is outdated (compared to their Gitlab repository) and contains broken links, and their Gitlab repository is not well documented.

# _Useful links_

* [Gitlab repository](https://gitlab.ai.vub.ac.be/ehai/babel-core)
* [Outdated Github repo](https://github.com/dwarfmaster/Babel2)
   * In particular, see the [tutorial directory](https://github.com/dwarfmaster/Babel2/tree/master/tutorial), which was removed from the Gitlab repo for some reason.
     * Start with [nlp-tools-tutorial.lisp](https://github.com/dwarfmaster/Babel2/blob/master/tutorial/nlp-tools-tutorial.lisp). These examples work, except for `get-penelope-pos-tags` and `run-penelope-dependency-parser`.
     * Then see what's in [the fcg manual directory](https://github.com/dwarfmaster/Babel2/tree/master/tutorial/fcg-manual), especially [fcg-manual.lisp](https://github.com/dwarfmaster/Babel2/blob/master/tutorial/fcg-manual/fcg-manual.lisp) and [evaluation-tutorial.lisp](https://github.com/dwarfmaster/Babel2/blob/master/tutorial/fcg-manual/evaluation-tutorial.lisp). But I haven't tested those yet.
* To test Babel, open SBCL and [follow these instructions:](https://github.com/martinodb/babel-core/blob/master/test-babel-installation.lisp). Also see the lisp files in the [tutorial directory](https://github.com/dwarfmaster/Babel2/tree/master/tutorial) as described above. I haven't incorporated the tutorial directory into this repo yet because I want to keep a pristine upstream codebase (from the Gitlab repo) at least until I get a chance to test those examples.
* [Babel Toolkit website](https://emergent-languages.org/)
   * [Babel2 manual](https://emergent-languages.org/assets/pdfs/Babel2_Manual.pdf), I added it [here](https://github.com/martinodb/babel-core/blob/martinodb-main/Babel2_Manual.pdf). Note: it seems outdated.
   * [A Practical Guide to Studying Emergent Communication through Grounded Language Games](https://emergent-languages.org/assets/pdfs/babel-toolkit.pdf)
* [FCG website](https://www.fcg-net.org)
   * [Basics of FCG](https://www.fcg-net.org/wp-content/uploads/papers/basics-of-fcg.pdf), I added it [here](https://github.com/martinodb/babel-core/blob/martinodb-main/basics-of-fcg.pdf)

