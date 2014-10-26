

(defmacro dohash ((k v hash) &rest body)
  `(loop for ,k being the hash-keys of ,hash
      using (hash-value ,v) do ,@body))


(defmacro loop-hash ((k v hash) &rest body)
  `(loop for ,k being the hash-keys of ,hash 
      using (hash-value ,v) ,@body))


(defun copy-hash-table (hash-table &key (empty NIL))
  "return a copy of a hash table. If :empty is T then returns an empty copy"
  (let ((table (make-hash-table 
		:test (hash-table-test hash-table)
		:rehash-size (hash-table-rehash-size hash-table)
		:rehash-threshold (hash-table-rehash-threshold hash-table)
		:size (hash-table-size hash-table))))
    (if empty 
	table
	(progn (dohash (k v hash-table) (setf (gethash k table) v)) table))))


(defun hash-union (func &rest hashes)
  "returns the union of hashes, merged using func"
  (let ((table (copy-hash-table (first hashes) :empty T)))
    (dolist (hash hashes)
      (dohash (k v hash)
	(if (null (gethash k table))
	    (setf (gethash k table) v)
	    (setf (gethash k table) (funcall func v (gethash k table))))))
    table))


(defun hash-union-with-accum (func accum &rest hashes)
  "similar to hash-unions, but accepts an accumulator as well as a function"
  (let ((table (copy-hash-table (first hashes) :empty T)))
    (dolist (hash hashes)
      (dohash (k v hash)
	(setf (gethash k table) (funcall func v (gethash k table accum)))))
    table))


(defun hash-keys (hash) 
  (loop-hash (key _ hash) collect key))

(defun hash-vals (hash) 
  (loop-hash (_ vals hash) collect key))



