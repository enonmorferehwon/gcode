#! /usr/bin/env -S sbcl --script ;; if needed, change the shebang

(defun read-directed-edges-from-file (filename)
  "Reads a list of directed graphs from a file: each graph is represented as a list of edges"
  (with-open-file (stream filename :direction :input)
    (loop for graph = (ignore-errors (read stream))
          while graph
          collect graph)))

(defun make-undirected-graph (directed-edges)
  "Given a list of edges from a directed graph, returns the corresponding undirected edge list with duplicates removed"
  (let ((undirected-edges '()))
    (dolist (edge directed-edges)
      (let* ((node1 (first edge))
             (node2 (second edge))
             (sorted-edge (if (< node1 node2)
                              (list node1 node2)
                              (list node2 node1))))
        ;; Adds a sorted undirected edge if it is not already present
        (unless (member sorted-edge undirected-edges :test #'equal)
          (push sorted-edge undirected-edges))))
    ;; Returns the edge list of the undirected graph
    (reverse undirected-edges)))

(defun build-adjacency-list (edges)
  "Creates an adjacency list from an edge list"
  (let ((adj-list (make-hash-table :test 'equal)))
    (dolist (edge edges)
      (let ((node1 (first edge))
            (node2 (second edge)))
        (push node2 (gethash node1 adj-list))
        (push node1 (gethash node2 adj-list))))
    adj-list))

(defun is-connected (edges)
  "Checks whether an undirected graph (represented as an edge list) is connected"
  (let ((adj-list (build-adjacency-list edges))
        (visited (make-hash-table :test 'equal)))
    (labels ((visit (node)
               (setf (gethash node visited) t)
               (dolist (neighbor (gethash node adj-list))
                 (unless (gethash neighbor visited)
                   (visit neighbor)))))
      ;; Starts the traversal from each node
      (visit (first (first edges))))
    ;; Checks whether all nodes have been visited
    (= (hash-table-count visited) (hash-table-count adj-list))))

(defun bfs-girth (graph start)
  "Traverses the graph from the selected 'start' node to determine the length of a cycle"
  (let ((queue (list (list start nil 0)))  ;; (current node, previous node, depth)
        (visited (make-hash-table)))
    (setf (gethash start visited) 0)
    (loop while queue do
         (let* ((current (pop queue))
                (node (first current))
                (prev (second current))
                (depth (third current)))
           (dolist (neighbor (gethash node graph))
             (if (and (gethash neighbor visited)
                      (not (equal neighbor prev)))
                 ;; If an already visited node other than the previous one is found, a cycle has been detected
                 (return-from bfs-girth (+ depth 1 (gethash neighbor visited)))
                 ;; If the neighboring node has not been visited yet, add it to the traversal structure
                 (unless (gethash neighbor visited)
                   (setf (gethash neighbor visited) (1+ depth))
                   (push (list neighbor node (1+ depth)) queue))))))))

(defun girth (graph)
  "Computes the girth (i.e., the length of its shortest cycle) of an undirected graph"
  ;;  The graph is represented as an adjacency list
  (let ((min-cycle most-positive-fixnum))
    (maphash (lambda (node neighbors)
               (let ((cycle-length (bfs-girth graph node)))
                 (when cycle-length
                   (setf min-cycle (min min-cycle cycle-length)))))
             graph)
    (if (eql min-cycle most-positive-fixnum)
        nil  ;; No cycle found
        min-cycle)))  ;; Returns the length of the shortest cycle

(defun initialize-output-file (filename)
  "Initializes the output file with supersede to overwrite its previous contents"
  ;; The file is only opened and closed
  (with-open-file (stream filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    ))

(defun write-directed-edges-to-file (filename graph)
  "Writes a directed graph to the output file"
  (with-open-file (stream filename :direction :output :if-exists :append :if-does-not-exist :create)
    (format stream "(")
    (dolist (edge graph)
      (format stream "~a~%" edge))
    (format stream ")~%")))

(defun main ()
  "Driver"
  (let* ((input-file "sink_out-1")
         (output-file "sink_out_0")
         (directed-graphs (read-directed-edges-from-file input-file))
         (total-count 0)
         (saved-count 0))
    ;; Initializes the output file
    (initialize-output-file output-file)
    ;; Processes the directed graphs
    (dolist (graph directed-graphs)
      (incf total-count)
      (let* ((undirected-graph (make-undirected-graph graph))
             (adj-list (build-adjacency-list undirected-graph)))
        (when (and (is-connected undirected-graph) (not (girth adj-list)))
          ;; Saves the directed graph if the corresponding undirected graph is acyclic and connected
          (write-directed-edges-to-file output-file graph)
          (incf saved-count))))
    ;; Prints results
    (format t "Number of analyzed graphs: ~a~%" total-count)
    (format t "Number of registered graphs in ~a: ~a~%" output-file saved-count)))

(main)
