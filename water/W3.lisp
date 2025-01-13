#! sbcl --script

(defun read-edges-from-file (filename)
  "Legge un file contenente una lista di liste di archi."
  (with-open-file (stream filename)
    (let (lists)
      (loop for edges = (read stream nil nil)
            while edges do
              (push edges lists))
      (reverse lists))))

(defun find-symmetric-edges (edges)
  "Trova tutte le coppie di archi simmetrici in una lista di archi."
  (let ((symmetric-pairs '()))
    (dolist (edge edges)
      (let ((reverse-edge (reverse edge)))
        (when (and (member reverse-edge edges :test #'equal)
                   (not (member (list edge reverse-edge) symmetric-pairs :test #'equal))
                   (not (member (list reverse-edge edge) symmetric-pairs :test #'equal)))
          (push (list edge reverse-edge) symmetric-pairs))))
    symmetric-pairs))

(defun generate-graphs-recursively (edges)
  "Genera ricorsivamente tutti i grafi eliminando una delle direzioni per ogni coppia di archi simmetrici."
  (let ((symmetric-pairs (find-symmetric-edges edges)))
    (if (null symmetric-pairs)
        (list edges)  ; Caso base: nessuna coppia simmetrica, restituiamo il grafo attuale.
        (let ((pair (first symmetric-pairs)))
          (append (generate-graphs-recursively (remove (second pair) edges :test #'equal))
                  (generate-graphs-recursively (remove (first pair) edges :test #'equal)))))))

(defun find-sinks (edges)
  "Trova i nodi che sono pozzi (solo in-degree, nessun out-degree)."
  (let ((out-degrees (make-hash-table))
        (in-degrees (make-hash-table)))
    ;; Conta gli out-degree e in-degree
    (dolist (edge edges)
      (let ((src (first edge))
            (dst (second edge)))
        (incf (gethash src out-degrees 0))
        (incf (gethash dst in-degrees 0))))
    ;; Identifica i pozzi
    (remove-if-not
     (lambda (node)
       (and (= (gethash node out-degrees 0) 0)
            (> (gethash node in-degrees 0) 0)))
     (hash-table-keys in-degrees))))

(defun find-nodes-with-multiple-out-degrees (edges)
  "Trova i nodi che hanno più out-degree e nessun in-degree."
  (let ((out-degrees (make-hash-table))
        (in-degrees (make-hash-table)))
    ;; Conta gli out-degree e in-degree
    (dolist (edge edges)
      (let ((src (first edge))
            (dst (second edge)))
        (incf (gethash src out-degrees 0))
        (incf (gethash dst in-degrees 0))))
    ;; Identifica i nodi con più out-degree e senza in-degree
    (remove-if-not
     (lambda (node)
       (and (> (gethash node out-degrees 0) 1)
            (= (gethash node in-degrees 0) 0)))
     (hash-table-keys out-degrees))))

(defun hash-table-keys (hash-table)
  "Restituisce una lista di tutte le chiavi presenti nella hash table."
  (let (keys)
    (maphash (lambda (key value)
               (push key keys))
             hash-table)
    keys))

(defun filter-graphs (graphs)
  "Filtra i grafi per mantenere solo quelli con un unico pozzo e senza nodi con più out-degree e nessun in-degree."
  (remove-if
   (lambda (edges)
     (or (/= (length (find-sinks edges)) 1) ;; Deve esserci esattamente un pozzo
         (not (null (find-nodes-with-multiple-out-degrees edges))))) ;; Nessun nodo con più out-degree senza in-degree
   graphs))

(defun process-graphs-from-file (filename)
  "Legge liste di archi da un file e processa ciascuna lista per identificare i grafi con un unico pozzo."
  (let ((all-edge-lists (read-edges-from-file filename))
	(counter_1 1)   ;; Inizializzazione del contatore per numerare i grafi di partenza
	(counter_2 1)   ;; Inizializzazione del contatore per numerare i grafi ridotti
	)  
    (dolist (edges all-edge-lists)
      (format t "+++++++++++++++++++++++++ ~%")
      (format t "Processo il grafo n. ~a: ~%~a~%" counter_1 edges)
      (let* ((all-graphs (generate-graphs-recursively edges))
             (filtered-graphs (filter-graphs all-graphs)))
        (dolist (graph filtered-graphs)
          (let ((sink (first (find-sinks graph)))) ;; Trova il pozzo
            (format t "e ottengo il grafo ridotto n. ~a: ~%~a~% che è riferibile al pozzo -> sink_~a~%~%" counter_2 graph sink))
	  (incf counter_2)))
       (incf counter_1))))

;; Nome del file da cui leggere le liste di archi
(let ((filename "sink_out_0"))
  (process-graphs-from-file filename))
