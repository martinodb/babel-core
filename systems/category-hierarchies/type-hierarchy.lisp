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

(export '(def-fcg-constructions-with-type-hierarchy
          type-hierarchy
          fcg-construction-set-with-type-hierarchy
          add-category add-categories add-link
          get-type-hierarchy
          directed-path-p
          node-p
          coherence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classes for using type hierarchies ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass type-hierarchy ()
  ((graph
    :accessor graph
    :initarg :graph
    :initform (make-graph :directed? t)
    :type 'directed-graph
    :documentation "graph of type-hierarchy"))
  (:documentation "The class for type-hierarchies, containing a graph"))

(defmacro def-fcg-constructions-with-type-hierarchy (name &body keys-and-defs)
  "Macro that defines an fcg-construction-set with typehierarchy,
   It holds the :type-hierarchy slot, then calls def-fcg-constructions, and
   sets the typehierarchy in the blackboard of the fcg-construction-set."
  (let ((type-hierarchy (find-key-arg keys-and-defs :type-hierarchy))
        (new-keys-and-defs (remove-key-arg keys-and-defs :type-hierarchy)))
    `(let ((cxn-inventory (def-fcg-constructions ,name ,@new-keys-and-defs)))
       (set-data (blackboard cxn-inventory) :type-hierarchy (or ,type-hierarchy (make-instance 'type-hierarchy)))
       cxn-inventory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic methods for type-hierarchies ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod copy-object-content ((source type-hierarchy) (destination type-hierarchy))
  (setf (graph destination) (graph source)))

(defgeneric add-category (category type-hierarchy))
(defmethod add-category ((category symbol) (type-hierarchy type-hierarchy))
  "add category to type-hierarchy"
  (graph-utils:add-node (graph type-hierarchy) category))

(defgeneric add-categories (categories type-hierarchy))
(defmethod add-categories ((categories list) (type-hierarchy type-hierarchy))
  "add list of categories to type-hierarchy"
  (dolist (category categories)
    (graph-utils:add-node (graph type-hierarchy) category))
  (graph type-hierarchy))

(defgeneric add-link (child mother type-hierarchy &key weight))
(defmethod add-link ((child symbol) (mother symbol) (type-hierarchy type-hierarchy) &key (weight 0.5))
  "add link to type-hierarchy"
  (unless (and (node-p child type-hierarchy) (node-p mother type-hierarchy))
    (error (format nil "For adding a link to a type-hierarchy, both nodes need to be declared first.
            This was not the case for eiter ~a or ~a." child mother)))
  (add-edge (graph type-hierarchy) child mother :weight weight))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying the weights on the nodes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(set-link-weight link-weight incf-link-weight decf-link-weight))

(defgeneric set-link-weight (child mother type-hierarchy weight))
(defmethod set-link-weight ((child symbol) (mother symbol) (type-hierarchy type-hierarchy) weight)
  "For setting the weight of an edge."
  (graph-utils::set-edge-weight (graph type-hierarchy) child mother weight))

(defgeneric link-weight (child mother type-hierarchy))
(defmethod link-weight ((child symbol) (mother symbol) (type-hierarchy type-hierarchy))
  "For getting the weight of an edge."
  (edge-weight (graph type-hierarchy) child mother))

(defgeneric incf-link-weight (child mother type-hierarchy delta))
(defmethod incf-link-weight ((child symbol) (mother symbol) (type-hierarchy type-hierarchy) (delta number))
  "For getting the weight of an edge."
  (graph-utils::incf-edge-weight (graph type-hierarchy) child mother :delta delta))

(defgeneric decf-link-weight (child mother type-hierarchy delta))
(defmethod decf-link-weight ((child symbol) (mother symbol) (type-hierarchy type-hierarchy) (delta number))
  "For getting the weight of an edge."
  (graph-utils::decf-edge-weight (graph type-hierarchy) child mother :delta delta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding paths, directed and undirected, and distances ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun node-p (node th)
  "returns true if node is a node in type-hierarchy th"
  (let ((node (gethash node (graph-utils::nodes (graph th)))))
    (when node t)))

(defun connected-p (n1 n2 th)
  "returns true if a path exists between n1 and n2 in th, nil otherwise, direction does not matter"
  (when (and (node-p n1 th)
             (node-p n2 th)
             (undirected-path n1 n2 th))
    t))

(defun directed-path-p (n1 n2 th)
  "returns true if n1 is connected to n2 via directed edges (n1->n2 or n1<-n2)"
  (when (and (node-p n1 th)
             (node-p n2 th)
             (find-cheapest-path (graph th) n1 n2))
    t))

(defun directed-distance (n1 n2 th)
  "returns the distance of the shortest link via directed edges between nodes n2 and n1, nil otherwise"
  (let ((path (when (and (node-p n1 th)
                         (node-p n2 th))
                (find-cheapest-path (graph th) n1 n2))))
    (when path
      (find-weight-of-path (graph th) path))))
  
(defun undirected-distance (n1 n2 th)
  "returns the distance fo the shortest link via edges between nodes n2 and n1 (does not take into account direction),
   nil otherwise"
  (let ((path (when (and (node-p n1 th)
                         (node-p n2 th))
                (undirected-path n1 n2 th))))
    (when path
      (find-weight-of-path (graph th) path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-type-hierarchy (cxn-inventory)
  "Returns the type-hierarchy of cxn-inventory or nil if there is no such slot"
  (when (field? (blackboard cxn-inventory) :type-hierarchy)
    (get-data (blackboard cxn-inventory) :type-hierarchy)))

(defgeneric undirected-path (node1 node-2 type-hierarchy))
(defmethod undirected-path ((n1 integer) (n2 integer) (type-hierarchy type-hierarchy))
  "returns any path between two nodes, doesn't take into account direction of links"
  (let* ((graph (graph type-hierarchy))
         (nodes (graph-utils::node-ids graph)))
    (let ((distances (mapcar (lambda (n) (cons n most-positive-fixnum)) nodes))
	  (previous (mapcar (lambda (n) (cons n nil)) nodes)))
      (setf (cdr (assoc n1 distances)) 0)
      (loop until (null nodes) do
	   (setf distances (sort distances '< :key 'cdr))
	   (let ((next (first (remove-if-not (lambda (d)
                                               (member (car d) nodes))
					     distances))))
	     (when (= (cdr next) most-positive-fixnum)
	       (return nil))
	     (when (= (car next) n2)
	       (return-from undirected-path
		 (nreverse (graph-utils::reconstruct-path previous n2))))
	     (setq nodes (remove (car next) nodes))
	     (dolist (neighbor (neighbors graph (car next)))
	       (let ((distance (1+ (cdr (assoc (car next) distances)))))
		 (when (< distance (cdr (assoc neighbor distances)))
		   (setf (cdr (assoc neighbor distances)) distance
			 (cdr (assoc neighbor previous)) (car next))))))))))

(defmethod undirected-path ((n1 symbol) (n2 symbol) (type-hierarchy type-hierarchy))
  "finds nodes by node-ids and calls method for that."
  (undirected-path (gethash n1 (graph-utils::nodes (graph type-hierarchy)))
                   (gethash n2 (graph-utils::nodes (graph type-hierarchy)))
                   type-hierarchy))


(defmethod coherence ((type-hierarchy type-hierarchy))
  (coherence (graph type-hierarchy)))

(defmethod coherence ((graph graph))
  (let ((in-node-counter 0)
        (out-node-counter 0))
    (loop with matrix = (graph-utils::matrix (graph-utils::matrix graph))
          for node being the hash-keys of matrix
          do
          (incf in-node-counter)
          (loop with outgoing-edges =  (gethash node matrix)
                for out-node being the hash-keys of outgoing-edges
                when (< (gethash out-node outgoing-edges) 1.0)
                do
                (incf out-node-counter)))
    (if (> out-node-counter 0)
      (/ out-node-counter in-node-counter)
      1)))

;; (coherence origins-of-syntax::*th*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additions to graph-utils ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :graph-utils)
(export '(set-edge-weight find-cheapest-path find-weight-of-path))

(defmethod set-edge-weight ((graph graph) n1 n2 weight &key &allow-other-keys)
  (set-edge-weight graph (gethash n1 (graph-utils::nodes graph)) (gethash n2 (graph-utils::nodes graph)) weight))

(defmethod find-cheapest-path ((graph graph) (n1 integer) (n2 integer))
  "Dijkstra's algorithm for finding the shortest path between two nodes."
  (let ((nodes (node-ids graph)))
    (let ((distances (mapcar (lambda (n) (cons n most-positive-fixnum)) nodes))
	  (previous (mapcar (lambda (n) (cons n nil)) nodes)))
      (setf (cdr (assoc n1 distances)) 0)
      (loop until (null nodes) do
	   (setf distances (sort distances '< :key 'cdr))
	   (let ((next (first (remove-if-not (lambda (d)
                                               (member (car d) nodes))
					     distances))))
	     (when (= (cdr next) most-positive-fixnum)
	       (return nil))
	     (when (= (car next) n2)
	       (return-from find-cheapest-path
		 (nreverse (reconstruct-path previous n2))))
	     (setq nodes (remove (car next) nodes))
	     (dolist (neighbor (if (directed? graph)
				   (outbound-neighbors graph (car next))
				   (neighbors graph (car next))))
               (let ((distance (+ (cdr (assoc (car next) distances)) (edge-weight graph (car next) neighbor))))
		 (when (< distance (cdr (assoc neighbor distances)))
		   (setf (cdr (assoc neighbor distances)) distance
			 (cdr (assoc neighbor previous)) (car next))))))))))

(defmethod find-cheapest-path ((graph graph) n1 n2)
  (find-cheapest-path graph
		      (gethash n1 (nodes graph))
		      (gethash n2 (nodes graph))))

(defmethod find-weight-of-path ((graph graph) path)
  "Returns the total weight of a path."
  (loop for (n1 n2) in path summing (edge-weight graph n1 n2)))

