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
(in-package :fcg)

(export '(load-demo-grammar))

(defun load-demo-grammar ()
  "This function loads the demo grammar (the linguist likes the mouse)
from the tutorial directory and returns the fcg construction set."
  (load (babel-pathname :directory '("systems" "fcg")
                        :name "demo-grammar"
                        :type "lisp"))
  (make-demo-grammar-cxns))


(defun show-grammar (grammar)
  "Show the grammar in the web interface"
  (add-element (make-html grammar)))
  