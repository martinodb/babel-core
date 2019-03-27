;; Copyright 2019 AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================
;;;; functions for inventing new words

(in-package :utils)

(export 'make-new-word)

(defvar *vowels* '("a" "e" "i" "o" "u"))
(defvar *consonants* '("b" "d" "f" "g" "k" "l" "m" "n" "p" "r" "s" "t" "v" "w" "x" "z"))
(defvar *words-so-far* nil)

(defun random-syllable ()
  (format nil "~a~a"
          (random-elt *consonants*)
          (random-elt *vowels*)))

(defun random-word (&optional (nof-syllables 3))
  (format nil "~{~a~}"
          (loop for i from 1 to nof-syllables
                collect (random-elt *consonants*)
                collect (random-elt *vowels*))))

(defun make-new-word (&optional (nof-syllables 3))
  (loop for word = (random-word nof-syllables)
    unless (member word *words-so-far*)
    do (setf *words-so-far* (push word *words-so-far*))
    (return word)))
