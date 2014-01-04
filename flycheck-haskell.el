;;; flycheck-haskell.el --- Improved Haskell support for Flycheck -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/flycheck/flycheck-haskell
;; Keywords: tools, convenience
;; Version: 0.2-cvs
;; Package-Requires: ((flycheck "0.16"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Improve Haskell support in Flycheck.

;; Configure Flycheck automatically based on the Cabal file of a project, and on
;; active Cabal sandboxes.

;;;; Setup

;; (add-hook 'haskell-mode-hook #'flycheck-haskell-setup)

;;; Code:

(require 'flycheck)
(require 'dash)

(defun flycheck-haskell-cabal-locate-cabal-file ()
  )

(defun flycheck-haskell-cabal-source-directories (cabal-file)
  )

(defun flycheck-haskell-find-sandbox-package-db ()
  )

(defvar-local flycheck-haskell-cabal-file nil
  "The Cabal file for the current buffer.")

;;;###autoload
(defun flycheck-haskell-setup ()
  "Setup Haskell support for Flycheck.

If the current file is part of a Cabal project, configure
Flycheck to take the module paths of the Cabal projects into
account.

Also search for Cabal sandboxes and add them to the module search
path as well."
  (when (buffer-file-name)
    (-when-let (cabal-file (flycheck-haskell-locate-cabal-file))
      (setq flycheck-haskell-cabal-file cabal-file)
      (setq flycheck-ghc-search-path
            (append (flycheck-haskell-get-source-dirs cabal-file)
                    flycheck-ghc-search-path))
      (-when-let (sandbox-db (flycheck-haskell-find-sandbox-package-db))
        (push sandbox-db flycheck-ghc-package-databases)
        (setq flycheck-ghc-no-user-package-database t)))))

(provide 'flycheck-haskell)

;;; flycheck-haskell.el ends here
