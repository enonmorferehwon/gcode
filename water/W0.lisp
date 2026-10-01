#! /usr/bin/env -S sbcl --script ;; if needed, change the shebang 

(defun filtra-grafi-branch-ammissibili (grafi)
  "Filters out directed graphs in which any node has an in-degree or out-degree greater than 1"
  (remove-if (lambda (grafo)
               (or (has-duplicate-nodes-in-position grafo 0) ; Check for duplicate source nodes 
                   (has-duplicate-nodes-in-position grafo 1))) ; Check for duplicate target nodes
             grafi))

(defun has-duplicate-nodes-in-position (grafo pos)
  "Checks for duplicate nodes at the specified position (0 or 1)"
  (let ((nodes (mapcar (lambda (arco) (nth pos arco)) grafo)))
    (/= (length nodes) (length (remove-duplicates nodes)))))

(defun genera-grafi-diretti (grafo)
  "Generates all possible directed graphs by removing symmetric edges"
  (let* ((archi-simmetrici
          (remove-if-not (lambda (arco)
                           (find (reverse arco) grafo :test #'equal))
                         grafo))
         ;; Removes duplicates to avoid counting the same pair twice
         (coppie-simmetriche
          (remove-duplicates
           (mapcar (lambda (arco) (list arco (reverse arco)))
                   archi-simmetrici)
           :test (lambda (a b)
                   (or (equal a b)
                       (equal a (reverse b))))))
         ;; Generates all possible combinations of directed graphs
         (combinazioni
          (reduce (lambda (acc coppia)
                    (let ((arco1 (first coppia))
                          (arco2 (second coppia)))
                      (loop for g in acc
                            append (list (remove arco2 g :test #'equal)
                                         (remove arco1 g :test #'equal)))))
                  coppie-simmetriche
                  :initial-value (list grafo))))
    ;; Removes duplicates
    (remove-duplicates combinazioni :test #'equal)))

(defun rimuovi-e-costruisci (grafo-modificato n1 n2)
  "Removes edges originating from n1 or n2 and generates directed graphs from symmetric edge pairs"
  (let* ((n1 (first n1)) ; Extracts the node value
         (n2 (first n2)) ; Extracts the node value
         ;; Removes edges originating from n1 or n2
         (grafo-senza-n1-n2 (remove-if (lambda (arco)
                                         (or (equal (first arco) n1)
                                             (equal (first arco) n2)))
                                       grafo-modificato))
         ;; Generates directed graphs
         (grafi-diretti (genera-grafi-diretti grafo-senza-n1-n2)))
    ;; Prints the generated directed graphs
    (format t "Directed graphs generated:~%") 
    (dolist (g grafi-diretti)
      (format t "  ~a~%" g))
    ;; Filters valid branches
    (let ((grafi-branch-ammissibili (filtra-grafi-branch-ammissibili grafi-diretti)))
      (if (null grafi-branch-ammissibili)
          (progn
            (format t "No valid branch graphs found.~%")nil)
          (progn
            (format t "Valid branches:~%")
            (dolist (g grafi-branch-ammissibili)
              (format t "  ~a~%" g)))))))

(defun rimuovi-arco (grafo arco)
  "Removes one edge from the graph"
  (remove arco grafo :test #'equal))

(defun rimuovi-coppia-archi (grafo arco)
  "Removes one edge and its reverse edge from the graph"
  (let ((simmetrico (reverse arco)))
    (remove simmetrico (rimuovi-arco grafo arco) :test #'equal)))

(defun genera-grafi (grafo)
  "Generates valid graphs by removing one edge or one pair of symmetric edges at a time, avoiding duplicates for symmetric pairs"
  (let ((grafi '())
        (visitati '())) ;; To track the edges that have already been processed
    (dolist (arco grafo)
      (unless (member arco visitati :test #'equal)
        ;; Adds the edge and its reverse edge to the list of processed edges
        (push arco visitati)
        (push (reverse arco) visitati)
        ;; Removes the edge and its reverse edge (if present)
        (if (member (reverse arco) grafo :test #'equal)
            (push (list (rimuovi-coppia-archi grafo arco) arco) grafi)
            ;; Removes the edge only
            (push (list (rimuovi-arco grafo arco) arco) grafi))))
    (remove-duplicates grafi :test #'equal)))

;; Driver-like function
(defun main ()
  "Reads the graph and the reference/terminal nodes, then generates the modified graphs"
  
  (let* ((grafo
;; The graph must be specified in the correct order: edges must follow the sequence that defines the graph
;; NOTE: if the first and last edges terminate at the same node the endpoint of the second edge must be set to 0; node2 must then be adjusted to 0 accordingly
;; Example: tuple ((1 2) (2 1) (2 3) (3 4) (4 1)) becomes ((1 2) (2 1) (2 3) (3 4) (4 0)) with nodo1 = 1 and nodo2 = 0

	  '((5 8) (8 5) (8 9) (9 8) (9 10) (10 9) (10 11) (11 12) (12 1)) ;; Terminal nodes: 1 and 5
         ;; '((5 6) (6 5) (6 7) (7 1)) ;; Terminal nodes: 1 and 5
	 ;; '((7 1) (6 7) (5 6) (6 5) (5 8) (8 5) (9 8) (8 9) (10 9) (9 10) (10 11) (11 12) (12 1)) ;; To be adjusted (not use)
	 ;; '((7 1) (6 7) (5 6) (6 5) (5 8) (8 5) (9 8) (8 9) (10 9) (9 10) (10 11) (11 12) (12 0)) ;; Terminal nodes: 1 and 1; to be used
	 ;; '((1 2) (2 1) (2 3) (3 4) (4 1)) ;; To be adjusted (not use)
	 ;; '((1 2) (2 1) (2 3) (3 4) (4 0)) ;; Terminal nodes: 1 and 1; to be used
	 ;; '((1 2) (2 1) (2 3) (3 4) (4 5)) ;; Terminal nodes: 1 and 5
	 ;; '((1 2) (2 1) (2 3) (3 4)) ;; Terminal nodes: 1 and 4

;; NOTE: uncomment one line at a time to generate the different branches to be combinatorially reconnected to form the branched cycles
;; NOTE: partially overlapping circuits may produce duplicated branches
	 
	    )
         (nodo1 '(1))  ; First reference node
         (nodo2 '(5))) ; Second reference node 

    ;; Prints the original graph and the reference/terminal nodes
    (format t "~%Original graph: ~a~%" grafo)
    ;; Counts and represents the number of nodes
    (let* ((archi (copy-tree grafo)))          ;; Local copy of the graph
      ;; Generates and prints the modified graphs
      (let* ((grafi-modificati (genera-grafi grafo)))
        (dolist (g grafi-modificati)
          ;; Extracts the components of the modified graph
          (let* ((grafo-modificato (first g))
                 (n1 nodo1)
                 (n2 nodo2))
	    ;; Prints information about the current graph
	    (format t "~%Extracted edge --> ~a ~%Corresponding graph:" (second g))
            (format t "~%~a~%" grafo-modificato)
            ;; Call to the remove-and-build function
            (let ((grafi-derivati (rimuovi-e-costruisci grafo-modificato n1 n2)))
              ;; Prints the derived graphs
              (dolist (g-derivato grafi-derivati)
                (format t "  ~a~%" g-derivato)))))))))


(main)
