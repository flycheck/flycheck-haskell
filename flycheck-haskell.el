;;; flycheck-haskell.el --- Flycheck: Haskell configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/flycheck/flycheck-haskell
;; Keywords: tools, convenience
;; Version: 0.2-cvs
;; Package-Requires: ((flycheck "0.16") (haskell-mode "13.7") (dash "2.4.0") (f "0.11.0"))

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

;; Configure Haskell syntax checking by Flycheck.

;;;; Cabal support

;; Try to find Cabal project files for Haskell buffers, and configure the
;; Haskell syntax checkers in Flycheck according to the contents of the Cabal
;; file:
;;
;; - Add all source directories to the GHC search path

;;;; Cabal sandboxes

;; Try to find a Cabal sandbox configuration for this project, and configure the
;; Haskell syntax checkers in Flycheck to use the package database from the
;; Sandbox.

;;;; Setup

;; (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

;;; Code:

(require 'haskell-cabal)
(require 'flycheck)
(require 'f)
(require 'dash)
(require 'rx)


;;;; Customization

(defgroup flycheck-haskell nil
  "Haskell support for Flycheck."
  :prefix "flycheck-haskell-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/flycheck/flycheck-haskell"))

(defcustom flycheck-haskell-runhaskell "runhaskell"
  "Path to the `runhaskell' executable.

This library uses `runhaskell' to run various Haskell helper
scripts to extract information from Cabal files."
  :type `(file :must-match t)
  :group 'flycheck-haskell)


;;;; Cabal support
(defconst flycheck-haskell-helpers-directory
  (f-join (f-dirname (f-this-file)) "helpers")
  "Directory of helpers.")

(defconst flycheck-haskell-get-source-directories
  (f-join flycheck-haskell-helpers-directory "get-source-directories.hs")
  "Helper to get source directories from a Cabal project.")

(defun flycheck-haskell-get-source-directories (cabal-file)
  "Get the source directories from a CABAL-FILE.

CABAL-FILE is a string denoting a Cabal project file.

Return a list of source directories.  Signal an error if
CABAL-FILE is not a valid project file, or if
`flycheck-haskell-runhaskell' does not exist."
  (let ((cabal-dir (f-dirname cabal-file))
        (source-files (process-lines flycheck-haskell-runhaskell
                                     flycheck-haskell-get-source-directories
                                     cabal-file)))
    ;; Expand all relative file names from the Cabal file
    (or (--map (f-join cabal-dir it) source-files)
        ;; Fall back to the root source directory
        (list cabal-dir))))

(defconst flycheck-haskell-sandbox-config "cabal.sandbox.config"
  "The file name of a Cabal sandbox configuration.")

(defconst flycheck-haskell-package-db-re
  (rx line-start (zero-or-more (any space)) "package-db:"
      (zero-or-more (any space))
      (group (one-or-more (not (any space))))
      (zero-or-more (any space) line-end))
  "Regular expression to parse the package db directory.")

(defun flycheck-haskell-get-package-db (sandbox-config-file)
  "Get the package database directory from SANDBOX-CONFIG-FILE.

Return the package database directory as string, or nil, if the
database was not found."
  (with-temp-buffer
    (insert-file-contents sandbox-config-file)
    (goto-char (point-min))
    (when (re-search-forward flycheck-haskell-package-db-re nil 'noerror)
      (match-string 1))))

(defun flycheck-haskell-find-sandbox-config ()
  "Find Cabal sandbox configuration for the current buffer.

Return the absolute path of the sandbox configuration file as
string, or nil, if no sandbox configuration file was found."
  (-when-let (root-dir (locate-dominating-file (buffer-file-name)
                                               flycheck-haskell-sandbox-config))
    (f-join root-dir flycheck-haskell-sandbox-config)))

;;;###autoload
(defun flycheck-haskell-setup ()
  "Setup Haskell support for Flycheck.

If the current file is part of a Cabal project, configure
Flycheck to take the module paths of the Cabal projects into
account.

Also search for Cabal sandboxes and add them to the module search
path as well."
  (when (buffer-file-name)
    (-when-let (cabal-file (haskell-cabal-find-file))
      (setq flycheck-ghc-search-path
            (append (flycheck-haskell-get-source-directories cabal-file)
                    flycheck-ghc-search-path))
      (-when-let* ((config (flycheck-haskell-find-sandbox-config))
                   (package-db (flycheck-haskell-get-package-db config)))
        (push package-db flycheck-ghc-package-databases)
        (setq flycheck-ghc-no-user-package-database t)))))

(provide 'flycheck-haskell)

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:

;;; flycheck-haskell.el ends here
