==================
 flycheck-haskell
==================

.. default-role:: code

Configure Haskell support in Flycheck_.

In Cabal projects, automatically configure Flycheck to according to the build
settings in the Cabal file.  Currently, just add the source directories of the
Cabal project to `flycheck-ghc-search-path`.

Also try to find a Cabal sandbox for the current Cabal project, and add the
package database of the sandbox to `flycheck-ghc-package-databases`.

Installation
============

As usual, from MELPA_ and Marmalade_.

In your Cask_ file:

.. code-block:: cl

   (source gnu)
   (source melpa)

   (depends-on "flycheck-haskell")

In your `init.el`:

.. code-block:: cl

   (eval-after-load 'flycheck
     '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

Usage
=====

Just use Flycheck as usual in your Cabal projects.

Customization
=============

- `M-x customize-group RET flycheck-haskell`

License
=======

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.

See COPYING_ for details.

.. _Flycheck: https://github.com/flycheck/flycheck
.. _Cask: https://github.com/cask/cask
.. _MELPA: http://melpa.milkbox.net
.. _Marmalade: http://marmalade-repo.org/
.. _COPYING: https://github.com/flycheck/flycheck-haskell/blob/master/COPYING
