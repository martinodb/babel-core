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
;;;;
;;;; File: common-babel-symbols.lisp
;;;;
;;;; This file is there to avoid package conflicts for often used symbols 
;;;; by exporting them from the :utils package
;;;;

(in-package :utils)

(export 
 '(topic utterance meaning partial-meaning sensory-context 
   name robot box region robot-p box-p region-p a b 
   x y orientation width height
   image-schema prototype
   value
   score
   status
   cyclic
   top-node
   label))

