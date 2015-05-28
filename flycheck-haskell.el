;;; flycheck-haskell.el --- Flycheck: Cabal projects and sandboxes -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015 Sebastian Wiesner <swiesner@lunaryorn.com>
;; Copyright (C) 2015 Michael Alan Dorman <mdorman@ironicdesign.com>
;; Copyright (C) 2015 Alex Rozenshteyn <rpglover64@gmail.com>
;; Copyright (C) 2014 Gracjan Polak <gracjanpolak@gmail.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/flycheck/flycheck-haskell
;; Keywords: tools, convenience
;; Version: 0.7-cvs
;; Package-Requires: ((flycheck "0.22") (haskell-mode "13.7") (dash "2.4.0") (let-alist "1.0.1"))

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
;; - Add build directories from Cabal to the GHC search path to speed up
;;   checking and support non-Haskell modules such as hsc files
;; - Add auto-generated files from Cabal to the GHC search path
;; - Set the language from Cabal
;; - Enable language extensions from Cabal

;;;; Cabal sandboxes

;; Try to find a Cabal sandbox configuration for this project, and configure the
;; Haskell syntax checkers in Flycheck to use the package database from the
;; Sandbox.

;;;; Setup

;; (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

;;; Code:

(eval-when-compile
  (require 'rx)
  (require 'let-alist))

(require 'haskell-cabal)
(require 'flycheck)
(require 'dash)


;;; Customization

(defgroup flycheck-haskell nil
  "Haskell support for Flycheck."
  :prefix "flycheck-haskell-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/flycheck/flycheck-haskell"))

(defcustom flycheck-haskell-runhaskell "runhaskell"
  "Path to the `runhaskell' executable.

This library uses `runhaskell' to run various Haskell helper
scripts to extract information from Cabal files."
  :type '(file :must-match t)
  :group 'flycheck-haskell)


;;; Cabal support
(defconst flycheck-haskell-directory
  (file-name-directory (if load-in-progress
                           load-file-name
                         (buffer-file-name)))
  "The package directory of flycheck-haskell.")

(defconst flycheck-haskell-helper
  (expand-file-name "get-cabal-configuration.hs" flycheck-haskell-directory)
  "The helper to dump the Cabal configuration.")

(defconst flycheck-haskell-flags-helper
  (expand-file-name "get-flags.hs" flycheck-haskell-directory)
  "The helper to get compiler flags for the Cabal helper.")

(defun flycheck-haskell--get-flags ()
  "Get GHC flags to run the Cabal helper."
  (process-lines flycheck-haskell-runhaskell
                 flycheck-haskell-flags-helper))

(defun flycheck-haskell-read-cabal-configuration (cabal-file)
  "Read the Cabal configuration from CABAL-FILE."
  (let ((args (append (flycheck-haskell--get-flags)
                      (list flycheck-haskell-helper cabal-file))))
    (with-temp-buffer
      (let ((result (apply 'call-process flycheck-haskell-runhaskell
                           nil t nil args)))
        (when (= result 0)
          (goto-char (point-min))
          (read (current-buffer)))))))


;;; Cabal configuration caching
(defconst flycheck-haskell-config-cache (make-hash-table :test 'equal)
  "Cache of Cabal configuration.

A hash table, mapping the name of a cabal file to a
cons-cell `(MODTIME . CONFIG)', where MODTIME is the modification
time of the cabal file, and CONFIG the extracted configuration.")

(defun flycheck-haskell-clear-config-cache ()
  "Clear the cache of configurations."
  (interactive)
  (clrhash flycheck-haskell-config-cache))

(defun flycheck-haskell-get-cached-configuration (cabal-file)
  "Get the cached configuration for CABAL-FILE.

Return the cached configuration, or nil, if there is no cache
entry, or if the cache entry is outdated."
  (pcase-let* ((cache-entry (gethash cabal-file flycheck-haskell-config-cache))
               (`(,modtime . ,config) cache-entry))
    (when (and modtime (file-exists-p cabal-file))
      (let ((current-modtime (nth 5 (file-attributes cabal-file))))
        (if (time-less-p modtime current-modtime)
            ;; The entry is outdated, drop it.  `remhash' always
            ;; returns nil, so we are safe to use it here.
            (remhash cabal-file flycheck-haskell-config-cache)
          ;; The configuration is up to date, use it
          config)))))

(defun flycheck-haskell-read-and-cache-configuration (cabal-file)
  "Read and cache configuration from CABAL-FILE.

Return the configuration."
  (let ((modtime (nth 5 (file-attributes cabal-file)))
        (config (flycheck-haskell-read-cabal-configuration cabal-file)))
    (puthash cabal-file (cons modtime config) flycheck-haskell-config-cache)
    config))

(defun flycheck-haskell-get-configuration (cabal-file)
  "Get the Cabal configuration from CABAL-FILE.

Get the configuration either from our cache, or by reading the
CABAL-FILE.

Return the configuration."
  (or (flycheck-haskell-get-cached-configuration cabal-file)
      (flycheck-haskell-read-and-cache-configuration cabal-file)))


;;; Cabal sandbox support
(defconst flycheck-haskell-cabal-config "cabal.config"
  "The file name of a Cabal configuration.")

(defconst flycheck-haskell-sandbox-config "cabal.sandbox.config"
  "The file name of a Cabal sandbox configuration.")

(defmacro flycheck-haskell-with-config-file-buffer (file-name &rest body)
  "Eval BODY in a buffer with the contents of FILE-NAME."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents ,file-name)
     (goto-char (point-min))
     ,@body))

(defun flycheck-haskell-get-config-value (key)
  "Get the value of a configuration KEY from this buffer."
  (substring-no-properties
   (save-excursion
     (goto-char (point-min))
     (haskell-cabal-get-setting key))))

(defun flycheck-haskell-get-package-db (sandbox-config-file)
  "Get the package database directory from SANDBOX-CONFIG-FILE.

Return the package database directory as string, or nil, if the
database was not found."
  (flycheck-haskell-with-config-file-buffer sandbox-config-file
    (flycheck-haskell-get-config-value "package-db")))

(defun flycheck-haskell-get-compiler (cabal-config)
  "Get the compiler path from CABAL-CONFIG.

Return the compiler path as string, or nil, if the database was
not found."
  (flycheck-haskell-with-config-file-buffer cabal-config
    (flycheck-haskell-get-config-value "with-compiler")))

(defun flycheck-haskell-find-config (config-file)
  "Find a CONFIG-FILE for the current buffer.

Return the absolute path of CONFIG-FILE as string, or nil if
CONFIG-FILE was not found."
  (-when-let (root-dir (locate-dominating-file (buffer-file-name) config-file))
    (expand-file-name config-file root-dir)))

(defun flycheck-haskell-find-cabal-config ()
  "Find Cabal configuration for the current buffer.

Return the absolute path of the configuration file as
string, or nil if no Cabal configuration file was found."
  (flycheck-haskell-find-config flycheck-haskell-cabal-config))

(defun flycheck-haskell-find-sandbox-config ()
  "Find Cabal sandbox configuration for the current buffer.

Return the absolute path of the sandbox configuration file as
string, or nil if no sandbox configuration file was found."
  (flycheck-haskell-find-config flycheck-haskell-sandbox-config))


;;; Buffer setup
(defun flycheck-haskell-process-configuration (config)
  "Process the a Cabal CONFIG."
  (let-alist config
    (setq-local flycheck-ghc-search-path
                (append .build-directories .source-directories
                        flycheck-ghc-search-path))
    (setq-local flycheck-ghc-language-extensions
                (append .extensions .languages
                        flycheck-ghc-language-extensions))
    (setq-local flycheck-ghc-args
                (append .other-options flycheck-ghc-args))))

(defun flycheck-haskell-configure ()
  "Set paths and package database for the current project."
  (interactive)
  (when (and (buffer-file-name) (file-directory-p default-directory))
    (-when-let* ((cabal-file (haskell-cabal-find-file))
                 (config (flycheck-haskell-get-configuration cabal-file)))
      (flycheck-haskell-process-configuration config))

    (-when-let* ((config (flycheck-haskell-find-cabal-config))
                 (compiler (flycheck-haskell-get-compiler config)))
      (setq-local flycheck-haskell-ghc-executable compiler))

    (-when-let* ((config (flycheck-haskell-find-sandbox-config))
                 (package-db (flycheck-haskell-get-package-db config)))
      (setq-local flycheck-ghc-package-databases
                  (cons package-db flycheck-ghc-package-databases))
      (setq-local flycheck-ghc-no-user-package-database t))))

;;;###autoload
(defun flycheck-haskell-setup ()
  "Setup Haskell support for Flycheck.

If the current file is part of a Cabal project, configure
Flycheck to take the module paths of the Cabal projects into
account.

Also search for Cabal sandboxes and add them to the module search
path as well."
  (add-hook 'hack-local-variables-hook #'flycheck-haskell-configure))

(provide 'flycheck-haskell)

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:

;;; flycheck-haskell.el ends here
