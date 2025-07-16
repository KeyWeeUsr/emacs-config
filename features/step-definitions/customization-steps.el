;; -*- lexical-binding: t; -*-
(Given "^buffer contains \"\\([^\"]+\\)\" repeated \"\\([^\"]+\\)\" times$"
  (lambda (text repetitions)
    (unless test-buffer (error "Missing test buffer"))
    (with-current-buffer (get-buffer test-buffer)
      (insert (apply #'concat (make-list (string-to-number repetitions)
                                         text))))))

(Given "^filling paragraph is called$"
  (lambda ()
    (unless test-buffer (error "Missing test buffer"))
    (with-current-buffer (get-buffer test-buffer)
      (fill-paragraph))))

(Then "^buffer contains \"\\([^\"]+\\)\" same lines long \"\\([^\"]+\\)\" chars$"
  (lambda (num-lines line-length)
    (unless test-buffer (error "Missing test buffer"))
    (with-current-buffer (get-buffer test-buffer)
      (should (= (string-to-number num-lines)
                 (count-lines (point-min) (point-max))))
      (dotimes (_ (string-to-number num-lines))
        (should (= (string-to-number line-length)
                   (length (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))))))))
