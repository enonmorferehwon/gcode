#! sbcl --script

(defun read-directed-edges-from-file (filename)
  "Legge una lista di grafi diretti da un file. Ogni grafo è rappresentato come una lista di archi."
  (with-open-file (stream filename :direction :input)
    (loop for graph = (ignore-errors (read stream))
          while graph
          collect graph)))

(defun make-undirected-graph (directed-edges)
  "Data una lista di archi di un grafo diretto, restituisce una lista di archi del grafo non diretto corrispondente senza duplicati."
  (let ((undirected-edges '()))
    (dolist (edge directed-edges)
      (let* ((node1 (first edge))
             (node2 (second edge))
             (sorted-edge (if (< node1 node2)
                              (list node1 node2)
                              (list node2 node1))))
        ;; Aggiunge l'arco non diretto ordinato se non è già presente
        (unless (member sorted-edge undirected-edges :test #'equal)
          (push sorted-edge undirected-edges))))
    ;; Restituisce la lista degli archi del grafo non diretto
    (reverse undirected-edges)))

(defun build-adjacency-list (edges)
  "Crea una lista di adiacenza da una lista di archi."
  (let ((adj-list (make-hash-table :test 'equal)))
    (dolist (edge edges)
      (let ((node1 (first edge))
            (node2 (second edge)))
        (push node2 (gethash node1 adj-list))
        (push node1 (gethash node2 adj-list))))
    adj-list))

(defun is-connected (edges)
  "Verifica se un grafo non diretto rappresentato da una lista di archi è connesso."
  (let ((adj-list (build-adjacency-list edges))
        (visited (make-hash-table :test 'equal)))
    (labels ((visit (node)
               (setf (gethash node visited) t)
               (dolist (neighbor (gethash node adj-list))
                 (unless (gethash neighbor visited)
                   (visit neighbor)))))
      ;; Inizia la visita da un nodo qualsiasi
      (visit (first (first edges))))
    ;; Verifica se tutti i nodi sono stati visitati
    (= (hash-table-count visited) (hash-table-count adj-list))))

(defun bfs-girth (graph start)
  "Esegue una BFS sul grafo per trovare il ciclo più breve che inizia e termina nel nodo 'start'."
  (let ((queue (list (list start nil 0)))  ; (nodo-attuale nodo-precedente profondità)
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
                 ;; Se troviamo un nodo visitato che non è il precedente, abbiamo trovato un ciclo
                 (return-from bfs-girth (+ depth 1 (gethash neighbor visited)))
                 ;; Se il vicino non è stato visitato, aggiungilo alla coda
                 (unless (gethash neighbor visited)
                   (setf (gethash neighbor visited) (1+ depth))
                   (push (list neighbor node (1+ depth)) queue))))))))

(defun girth (graph)
  "Trova la girth di un grafo non orientato rappresentato come lista di adiacenza."
  (let ((min-cycle most-positive-fixnum))
    (maphash (lambda (node neighbors)
               (let ((cycle-length (bfs-girth graph node)))
                 (when cycle-length
                   (setf min-cycle (min min-cycle cycle-length)))))
             graph)
    (if (eql min-cycle most-positive-fixnum)
        nil  ; Nessun ciclo trovato
        min-cycle)))  ; Restituisce la lunghezza del ciclo più breve

(defun initialize-output-file (filename)
  "Inizializza il file di output con :supersede per cancellare il contenuto precedente."
  (with-open-file (stream filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    ;; Nessun contenuto da scrivere, il file viene solo aperto e chiuso
    ))

(defun write-directed-edges-to-file (filename graph)
  "Scrive un grafo diretto nel file di output."
  (with-open-file (stream filename :direction :output :if-exists :append :if-does-not-exist :create)
    (format stream "(")
    (dolist (edge graph)
      (format stream "~a~%" edge))
    (format stream ")~%")))

(defun main ()
  "Funzione principale."
  (let* ((input-file "sink_out-1")
         (output-file "sink_out_0")
         (directed-graphs (read-directed-edges-from-file input-file))
         (total-count 0)
         (saved-count 0))
    ;; Inizializza il file di output
    (initialize-output-file output-file)
    ;; Processa i grafi diretti
    (dolist (graph directed-graphs)
      (incf total-count)
      (let* ((undirected-graph (make-undirected-graph graph))
             (adj-list (build-adjacency-list undirected-graph)))
        (when (and (is-connected undirected-graph) (not (girth adj-list)))
          ;; Salva il grafo diretto se il grafo indiretto è aciclico e connesso
          (write-directed-edges-to-file output-file graph)
          (incf saved-count))))
    ;; Stampa i risultati
    (format t "Numero totale di grafi analizzati: ~a~%" total-count)
    (format t "Numero di grafi scritti in ~a: ~a~%" output-file saved-count)))

(main)
