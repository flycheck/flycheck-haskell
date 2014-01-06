;;; flycheck-haskell.el --- Flycheck: Haskell configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/flycheck/flycheck-haskell
;; Keywords: tools, convenience
;; Version: 0.2-cvs
;; Package-Requires: ((flycheck "0.16") (haskell-mode "13.7"))

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

;;;; Setup

;; (add-hook 'haskell-mode-hook #'flycheck-haskell-setup)

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
  "Path to the `runhaskell' executable."
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
      (-when-let (sandbox-db (flycheck-haskell-find-sandbox-package-db))
        (push sandbox-db flycheck-ghc-package-databases)
        (setq flycheck-ghc-no-user-package-database t)))))

(provide 'flycheck-haskell)

;;; flycheck-haskell.el ends here
