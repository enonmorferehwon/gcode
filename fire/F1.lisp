#! /usr/bin/env -S sbcl --script ;; If needed, change the shebang

(defun read-graphs-from-file (file-path)
  "Reads a list of graphs from a file, with each graph represented as a list of tuples"
  (with-open-file (in file-path :direction :input)
    (loop for graph = (read in nil)
          while graph
          collect graph)))

(defun find-edge-weight (edge graph)
  "Finds the weight of a given edge in the graph"
  (let ((src (first edge))
        (dst (second edge)))
    (third (find-if (lambda (e) (and (equal (first e) src)
                                     (equal (second e) dst)))
                    graph))))

(defun calculate-spanning-tree-weight (spanning-tree graph)
  "Computes the total weight of the given spanning tree"
  (reduce #'+ (mapcar (lambda (edge)
                        (find-edge-weight edge graph))
                      spanning-tree)))

(defun process-spanning-trees-from-file (file-path graph graph-index output-file)
  "Reads spanning trees from a file, computes the weight of each, and writes the results to a file"
  (with-open-file (in file-path :direction :input)
    (loop for line = (read in nil)
          while line
          for tree-index from 1
          do (let ((spanning-tree line)
                   (total-weight (calculate-spanning-tree-weight line graph)))
               (format output-file "Graph ~d, sub-graph ~d:~%~a~%" graph-index tree-index spanning-tree)
               (format output-file "The weight of the sub-graph ~d is: ~a~%~%" tree-index total-weight)))))

;; Sub-graph is either a spanning tree or a branched cycle

;; ==================== Modified to Accept Command-Line Arguments ====================
;;   Gets the lmn argument from the command line to build the T+lmn filename
 (let* ((args sb-ext:*posix-argv*) 
             (lmn (second args)) ;; Argument passed (the temperature lmn, e.g. "473")
 (t-file (format nil "T~a" lmn))) ;; Builds the filename (following the example T473)

  (defun write-results-to-files (input-files output-files)
    "Writes the processing results to a sequence of files"
    ;; Reads graphs from the dynamic t-file instead of the hardcoded Ttemperature file
    (let ((graphs (read-graphs-from-file t-file)))
      (loop for input-file in input-files
            for output-file in output-files
            do (with-open-file (out output-file :direction :output :if-exists :supersede)
                 (loop for graph in graphs
                       for graph-index from 1
                       do (format out "Processing the Graph ~d: ~a~%" graph-index graph)
                       
		       ;; Processes the graph using the spanning trees from the current file
                       
		       (process-spanning-trees-from-file input-file graph graph-index out))))))

  ;; Driver-like function; input and output files have to be specified
  (write-results-to-files
   '("W2_0" "W2_1" "W2_2" "W2_3" "W2_4" "W2_5" "W2_6" "W2_7" "W2_8" "W2_9"
     "W2_10" "W2_11" "W2_12" "C1" "C2" "C3" "C4" "C5" "C6" "C7") ;; input
   '("END_0" "END_1" "END_2" "END_3" "END_4" "END_5" "END_6" "END_7"
     "END_8" "END_9" "END_10" "END_11"  "END_12"  "END_C1"  "END_C2"
     "END_C3" "END_C4" "END_C5" "END_C6" "END_C7") ;; output
   ))

