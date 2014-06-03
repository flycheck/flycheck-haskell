;;; flycheck-haskell-test.el --- Flycheck Haskell: Test suite  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>

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

;; The test suite for Flycheck Haskell.

;;; Code:

(require 'flycheck-haskell)

(require 'ert)
(require 'f)


;;; Directories

(defconst flycheck-haskell-test-dir (f-parent (f-this-file))
  "Directory of the test suite.")

(defconst flycheck-haskell-test-cabal-file
  (f-join flycheck-haskell-test-dir "flycheck-haskell-test.cabal")
  "Cabal file for our test suite.")


;;; Helpers

(defun flycheck-haskell-test-config ()
  "Get the Cabal configuration from the test file."
  (flycheck-haskell-get-cabal-configuration flycheck-haskell-test-cabal-file))


;;; Test cases

(ert-deftest flycheck-haskell-runhaskell/default-value ()
  (should (string= flycheck-haskell-runhaskell "runhaskell")))

(ert-deftest flycheck-haskell-get-cabal-configuration/has-all-extensions ()
  (should (equal (assq 'extensions (flycheck-haskell-test-config))
                 '(extensions "OverloadedStrings"
                              "YouDontKnowThisOne"
                              "GeneralizedNewtypeDeriving"))))


(ert-deftest flycheck-haskell-get-cabal-configuration/has-all-languages ()
  (should (equal (assq 'languages (flycheck-haskell-test-config))
                 '(languages "Haskell98" "SpamLanguage" "Haskell2010"))))

(ert-deftest flycheck-haskell-get-cabal-configuration/source-dirs ()
  (let* ((builddirs '("lib/" "." "src/"))
         (expanddir (lambda (fn) (file-name-as-directory
                                  (f-join flycheck-haskell-test-dir fn)))))
    (should (equal
             (assq 'source-directories (flycheck-haskell-test-config))
             (cons 'source-directories (-map expanddir builddirs))))))

(ert-deftest flycheck-haskell-get-cabal-configuration/build-dirs ()
  (let* ((distdir (f-join flycheck-haskell-test-dir "dist/"))
         (expanddir (apply-partially #'f-join distdir))
         (builddirs '("build" "build/autogen"
                 "build/flycheck-haskell-unknown-stuff/flycheck-haskell-unknown-stuff-tmp"
                 "build/flycheck-haskell-test/flycheck-haskell-test-tmp")))
    (should (equal
             (assq 'build-directories (flycheck-haskell-test-config))
             (cons 'build-directories (-map expanddir builddirs))))))

(ert-deftest flycheck-haskell-process-configuration/language-extensions ()
  (with-temp-buffer                     ; To scope the variables
    (flycheck-haskell-process-configuration (flycheck-haskell-test-config))
    (should (equal flycheck-ghc-language-extensions
                   '("OverloadedStrings"
                     "YouDontKnowThisOne"
                     "GeneralizedNewtypeDeriving"
                     "Haskell98"
                     "SpamLanguage"
                     "Haskell2010")))))

(ert-deftest flycheck-haskell-process-configuration/search-path ()
  (let* ((distdir (f-join flycheck-haskell-test-dir "dist/"))
         (builddir (apply-partially #'f-join distdir))
         (builddirs '("build" "build/autogen"
                      "build/flycheck-haskell-unknown-stuff/flycheck-haskell-unknown-stuff-tmp"
                      "build/flycheck-haskell-test/flycheck-haskell-test-tmp"))
         (sourcedir (lambda (fn) (file-name-as-directory
                                  (f-join flycheck-haskell-test-dir fn))))
         (sourcedirs '("lib/" "." "src/")))
    (with-temp-buffer
      (flycheck-haskell-process-configuration (flycheck-haskell-test-config))
      (should (equal flycheck-ghc-search-path
                     (append (-map builddir builddirs)
                             (-map sourcedir sourcedirs)))))))

(provide 'flycheck-haskell-test)

;;; flycheck-haskell-test.el ends here
