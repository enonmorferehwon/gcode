#! /usr/bin/env -S sbcl --script ;; if needed, change the shebang

(defun read-edges-from-file (filename)
  (with-open-file (stream filename)
    (let (lists)
      (loop for edges = (read stream nil nil)
            while edges do
              (push edges lists))
      (reverse lists))))

(defun find-symmetric-edges (edges)
  (let ((symmetric-pairs '()))
    (dolist (edge edges)
      (let ((reverse-edge (reverse edge)))
        (when (and (member reverse-edge edges :test #'equal)
                   (not (member (list edge reverse-edge) symmetric-pairs :test #'equal))
                   (not (member (list reverse-edge edge) symmetric-pairs :test #'equal)))
          (push (list edge reverse-edge) symmetric-pairs))))
    symmetric-pairs))

(defun generate-graphs-recursively (edges)
  (let ((symmetric-pairs (find-symmetric-edges edges)))
    (if (null symmetric-pairs)
        (list edges)
        (let ((pair (first symmetric-pairs)))
          (append (generate-graphs-recursively (remove (second pair) edges :test #'equal))
                  (generate-graphs-recursively (remove (first pair) edges :test #'equal)))))))

(defun find-sinks (edges)
  (let ((out-degrees (make-hash-table))
        (in-degrees (make-hash-table)))
    (dolist (edge edges)
      (let ((src (first edge))
            (dst (second edge)))
        (incf (gethash src out-degrees 0))
        (incf (gethash dst in-degrees 0))))
    (remove-if-not
     (lambda (node)
       (and (= (gethash node out-degrees 0) 0)
            (> (gethash node in-degrees 0) 0)))
     (hash-table-keys in-degrees))))

(defun find-nodes-with-multiple-out-degrees (edges)
  (let ((out-degrees (make-hash-table))
        (in-degrees (make-hash-table)))
    (dolist (edge edges)
      (let ((src (first edge))
            (dst (second edge)))
        (incf (gethash src out-degrees 0))
        (incf (gethash dst in-degrees 0))))
    (remove-if-not
     (lambda (node)
       (and (> (gethash node out-degrees 0) 1)
            (= (gethash node in-degrees 0) 0)))
     (hash-table-keys out-degrees))))

(defun hash-table-keys (hash-table)
  (let (keys)
    (maphash (lambda (key value)
               (push key keys))
             hash-table)
    keys))

(defun filter-graphs (graphs)
  (remove-if
   (lambda (edges)
     (or (/= (length (find-sinks edges)) 1)
         (not (null (find-nodes-with-multiple-out-degrees edges)))))
   graphs))

(defun write-graphs-grouped-by-sink (sink-graph-alist output-filename)
  (with-open-file (stream output-filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (entry sink-graph-alist)
      (let ((sink (car entry))
            (graphs (cdr entry)))
        (format stream "=== Graphs with sink: ~a ===~%" sink)
        (dolist (graph graphs)
          (format stream "~a~%" graph))
        (format stream "~%")))))

(defun process-and-write-graphs (input-filename output-filename)
  (let ((all-edge-lists (read-edges-from-file input-filename))
        (sink-graph-hash (make-hash-table :test #'equal)))
    (dolist (edges all-edge-lists)
      (let* ((all-graphs (generate-graphs-recursively edges))
             (filtered-graphs (filter-graphs all-graphs)))
        (dolist (graph filtered-graphs)
          (let ((sink (first (find-sinks graph))))
            (push graph (gethash sink sink-graph-hash))))))

    ;; Builds the output alist (association list) and prints a summary
    (let (sink-graph-alist)
      (maphash (lambda (key value)
                 (push (cons key (reverse value)) sink-graph-alist))
               sink-graph-hash)

      ;; 1. Prints on the file
      (write-graphs-grouped-by-sink sink-graph-alist output-filename)

      ;; 2. Prints a summary to the screen
      (format t "~%=== Spanning tree summary for each sink ===~%")
      (dolist (entry sink-graph-alist)
        (let ((sink (car entry))
              (graphs (cdr entry)))
          (format t "Sink ~a: ~d spanning tree~%" sink (length graphs)))))))

;; The following function is used as an alternative to the previous one
;; You do not need to erase that 
(defun process-and-write-graphs (input-filename output-filename)
  (let ((all-edge-lists (read-edges-from-file input-filename))
        (sink-graph-hash (make-hash-table :test #'equal))
        (total-graphs 0)           ;; Counter for the extracted spanning trees
        (total-input-graphs 0))    ;; Counter for the evaluated graphs
    (dolist (edges all-edge-lists)
      (incf total-input-graphs)  ;; Each graph red is evaluated
      (let* ((all-graphs (generate-graphs-recursively edges))
             (filtered-graphs (filter-graphs all-graphs)))
        (dolist (graph filtered-graphs)
          (let ((sink (first (find-sinks graph))))
            (push graph (gethash sink sink-graph-hash))))
        (incf total-graphs (length filtered-graphs)))) ;; Found spanning trees

    ;; Builds the output alist and prints a summary
    (let (sink-graph-alist)
      (maphash (lambda (key value)
                 (push (cons key (reverse value)) sink-graph-alist))
               sink-graph-hash)

      ;; 1. Prints on the file
      (write-graphs-grouped-by-sink sink-graph-alist output-filename)

      ;; 2.Prints a summary to the screen 
      (format t "~%=== Spanning tree summary for each sink ===~%")
      (dolist (entry sink-graph-alist)
        (let ((sink (car entry))
              (graphs (cdr entry)))
          (format t "Sink ~a: ~d spanning tree~%" sink (length graphs))))

      ;; 3. Prints a whole summary
      (format t "~%Total number of graphs evaluated: ~d~%" total-input-graphs)
      (format t "Total spanning trees: ~d~%" total-graphs))))

;; A kind of driver
(let ((input-file "sink_out_0")
      (output-file "spanning_trees_by_sink"))
  (process-and-write-graphs input-file output-file))

