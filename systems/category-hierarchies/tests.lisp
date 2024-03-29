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
(in-package :type-hierarchies)

(deftest test-type-hierarchies-classes-and-methods ()
  (let ((type-hierarchy (make-instance 'type-hierarchy)))
    (test-assert (add-categories '(gender m f n) type-hierarchy))
    (test-assert (add-link  'm 'gender type-hierarchy))
    (add-link  'f 'gender type-hierarchy)
    (add-link  'n 'gender type-hierarchy)
    (test-assert (add-category  'number type-hierarchy))
    (add-category  'sg type-hierarchy)
    (add-category  'pl type-hierarchy)
    (add-link  'sg 'number type-hierarchy)
    (add-link  'pl 'number type-hierarchy)
    (add-categories '(noun common-noun proper-noun count-noun mass-noun) type-hierarchy)
    (add-link  'common-noun 'noun type-hierarchy)
    (add-link  'proper-noun 'noun type-hierarchy)
    (add-link  'count-noun 'common-noun type-hierarchy)
    (add-link  'mass-noun 'common-noun type-hierarchy)

    (test-assert (eq t (node-p 'gender type-hierarchy)))
    (test-assert (eq t (connected-p 'gender 'm type-hierarchy)))
    (test-assert (eq t (connected-p 'm 'gender type-hierarchy)))
    (test-assert (eq nil (connected-p 'm 'number type-hierarchy)))
    (test-assert (eq t (connected-p 'mass-noun 'noun type-hierarchy)))
    (test-assert (eq t (connected-p 'noun 'mass-noun type-hierarchy)))
    (test-assert (eq t (connected-p 'proper-noun 'mass-noun type-hierarchy)))
    (test-assert (eq t (connected-p 'proper-noun 'mass-noun type-hierarchy)))
    
    (test-assert (eq t (directed-path-p 'mass-noun 'noun type-hierarchy)))
    (test-assert (eq nil (directed-path-p 'noun 'mass-noun type-hierarchy)))
    (test-assert (eq nil (directed-path-p 'mass-noun 'proper-noun type-hierarchy)))
    (test-assert (eq nil (directed-path-p 'proper-noun 'mass-noun type-hierarchy)))

    (test-assert (eq nil (directed-distance  'noun 'mass-noun type-hierarchy)))
    (test-assert (eq 2 (directed-distance  'mass-noun 'noun type-hierarchy)))
    (test-assert (eq 1 (directed-distance  'common-noun 'noun type-hierarchy)))
    (test-assert (eq nil (directed-distance  'noun 'common-noun type-hierarchy)))

    (test-assert (eq nil (directed-distance 'noun 'gender type-hierarchy)))
    (test-assert (eq nil (directed-distance 'common-noun 'proper-noun type-hierarchy)))
    (test-assert (eq nil (directed-distance 'mass-noun 'count-noun type-hierarchy)))

    (test-assert (eq 2 (undirected-distance  'noun 'mass-noun type-hierarchy)))
    (test-assert (eq 2 (undirected-distance  'mass-noun 'noun type-hierarchy)))
    (test-assert (eq 1 (undirected-distance  'common-noun 'noun type-hierarchy)))
    (test-assert (eq 1 (undirected-distance  'noun 'common-noun type-hierarchy)))
    
    (test-assert (eq nil (undirected-distance 'noun 'gender type-hierarchy)))
    (test-assert (eq 2 (undirected-distance 'common-noun 'proper-noun type-hierarchy)))
    (test-assert (eq 2 (undirected-distance 'mass-noun 'count-noun type-hierarchy)))))

;; (test-type-hierarchies-classes-and-methods)