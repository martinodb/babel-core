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

(export '(visualize-type-hierarchy))

(defmethod visualize-type-hierarchy ((graph graph) &key (file "/var/tmp/graph.dot") render? weights?
                                     colors sizes (format "svg") colored-edges-0-1)
  "Save a dot file of this graph. Render can be one of (:heirarchical
:circular :radial :spring), which will render the graph using the appropriate
Graphviz tool."
  (let ((memory (make-hash-table :test 'equalp))
	(connector (if (directed? graph) "->" "--")))
    (with-open-file (out file
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (format out
              "~A graphutils~%{~%  splines=true;~%  node [ color = black, "
	      (if (directed? graph) "digraph" "graph"))
      (format out "fillcolor = white, style = filled, fontsize=10, fontname = times];~%")
      (map-nodes (lambda (name id)
                   (let ((neighbors (if (directed? graph)
                                        (outbound-neighbors graph id)
                                        (neighbors graph id))))
                     (dolist (n neighbors)
                       (unless (if (directed? graph)
                                   (gethash (list id n) memory)
                                   (or (gethash (list id n) memory)
                                       (gethash (list n id) memory)))
                         (setf (gethash (list id n) memory) t)
                         (when (not (directed? graph))
                           (setf (gethash (list n id) memory) t))
                         (if weights?
                           (format out
                                   "  \"~A\" ~A \"~A\" [w=~,2f,label=~,2f,fontsize=10, ~a, ~a, ~a];~%"
                                   name
                                   connector
                                   (gethash n (ids graph))
                                   (saref (matrix graph) id n)
                                   (saref (matrix graph) id n)
                                   (if (and colored-edges-0-1
                                            (<= (saref (matrix graph) id n) 1.0)
                                            (>= (saref (matrix graph) id n) 0.0))
                                       (format nil "fontcolor=grey~a" (min (round (* 100 (saref (matrix graph) id n))) 80))
                                       (format nil "fontcolor=black"))
                                   (if (and colored-edges-0-1
                                            (<= (saref (matrix graph) id n) 1.0)
                                            (>= (saref (matrix graph) id n) 0.0))
                                       (format nil "color=grey~a" (min (round (* 100 (saref (matrix graph) id n))) 80))
                                       (format nil "color=black"))
                                   (if colored-edges-0-1
                                     (cond ((and (< (saref (matrix graph) id n) 0.1)
                                                 (>= (saref (matrix graph) id n) 0.0))
                                            (format nil "style=bold"))
                                           ((and (<= (saref (matrix graph) id n) 1)
                                                 (> (saref (matrix graph) id n) 0.9))
                                            (format nil "style=dotted"))
                                           (t
                                            (format nil "style=filled")))))
                           (format out
                                   "  \"~A\" ~A \"~A\" [w=~,2f];~%"
                                   name
                                   connector
                                   (gethash n (ids graph))
                                   (saref (matrix graph) id n)))))
                     (format out "  \"~A\" [fillcolor=\"~A\""
                             name
                             (if (hash-table-p colors)
                                 (gethash name colors)
                                 "#ffff00"))
                     (if (hash-table-p sizes)
                         (progn
                           (format out ",shape=box,width=~F,fontname=Helvetica,"
                                   (gethash name sizes))
                           (format out "fixedsize=true,fontsize=~D"
                                   (truncate (* 10 (gethash name sizes)))))
                         "")
                     (format out "];~%")))
		 graph)
      (format out "}~%"))
    (if render?
	(let ((f (regex-replace "\.[a-z]+$" file (format nil "\.~A" format)))
	      (program (case render?
			 (:hierarchical (which "dot"))
			 (:circular     (which "circo"))
			 (:radial       (which "twopi"))
			 (:spring       (or (which "fdp") (which "neato")))
			 (otherwise     (or (which "fdp") (which "dot"))))))
	  (if program
	      (multiple-value-bind (output error-output exit-status)
		  (trivial-shell:shell-command
		   (format nil "~A -T~A -o ~A ~A" program format f file))
		(unless (= 0 exit-status)
		  (error "~A exited with status ~A: ~A ~A~%" program exit-status output error-output)))
	      (format t "Unable to create PNG of graph ~A.  Graphviz not in your path.~%" graph))
	  f)
	file)))

