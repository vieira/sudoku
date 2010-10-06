;;;; Simple Search Algorithms

;;; Here we define a general search function and then a set of search functions
;;; that follow specific search strategies.

(defun general-search (problem queuing-fn)
  "Expand nodes according to the specification of PROBLEM until we find
  a solution or run out of nodes to expand.  The QUEUING-FN decides which
  nodes to look at first. [p 73]"
  (let ((nodes (make-initial-queue problem queuing-fn))
	node)
    (loop (if (empty-queue? nodes) (RETURN nil))
	  (setq node (remove-front nodes))
	  (if (goal-test problem (node-state node)) (RETURN node))
	  (funcall queuing-fn nodes (expand node problem)))))
