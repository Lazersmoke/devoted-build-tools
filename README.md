Devoted Setup Script
====================

This is a simple script to setup a development environment for Devoted plugins.

You need to install these dependencies before using this utility: git, expect, vim, screen, nano, JDK 8, maven

To get started, simply install [`stack`](https://docs.haskellstack.org/en/stable/README/), either from your package manager or using `./getstack`. You may also need to `stack install parsec` if it isn't alredy installed.

After stack has installed, `./setup.hs` will take care of the rest.
