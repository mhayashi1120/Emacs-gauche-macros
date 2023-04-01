;;; gauche-macros-pcase.el --- Gauche :heartbeat: macros -*- lexical-binding: t -*-

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: lisp tools
;; Emacs: GNU Emacs
;; URL: https://github.com/mhayashi1120/Emacs-gauche-macros
;; Package-Requires: ((emacs "24.3"))
;; Version: 0.5.0

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; `pcase' extensions originally come from gauche `util.match' module.
;; This module use `pcase-exhaustive' since imitate util.match behavior.
;;
;; ## Usage:
;;
;; ```
;; (mapcar
;;  (pcase-switch
;;    (`(,distance m)
;;     distance)
;;    (`(,distance km)
;;     (* distance 1000))
;;    (`(,distance mile)
;;     (* distance 1.60934 1000)))
;;  '(
;;    (1000 m)
;;    (160 km)
;;    (100 mile)
;;    ))
;; ```
;;
;; ```
;; (mapcar*
;;  (pcase-switch*
;;    (`(,distance m)
;;     distance)
;;    (`(,distance km)
;;     (* distance 1000))
;;    (`(,distance mile)
;;     (* distance 1.60934 1000)))
;;  '(1000 160 100) '(m km mile)
;;    )
;; ```
;; ## TODO:
;;
;;  Otherwise introduce gauche-macros-match.el
;;

;;; Code:

(require 'cl-lib)

(defmacro pcase-switch (&rest cases)
  "Generate `pcase` function which accept one argument.
This come from gauche `match-lambda`
"
  (declare (indent 0) (debug t))
  `(lambda (args)
     (pcase-exhaustive args
       ,@cases)))

(defmacro pcase-switch* (&rest cases)
  "Generate `pcase` function which accept arguments.
This come from gauche `match-lambda*`
"
  (declare (indent 0) (debug t))
  `(lambda (&rest args)
     (pcase-exhaustive args
       ,@cases)))

(provide 'gauche-macros-pcase)

;;; gauche-macros-pcase.el ends here
