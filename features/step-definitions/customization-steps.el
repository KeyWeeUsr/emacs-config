(defvar feature-customization-test-buffer "*feature-customization*")

(And "^test buffer contains \"\\([^\"]+\\)\" repeated \"\\([^\"]+\\)\" times$"
  (lambda (text repetitions)
    (with-current-buffer (get-buffer-create feature-customization-test-buffer)
      (insert (apply #'concat (make-list (string-to-number repetitions)
                                         text))))))

(And "^filling paragraph is called$"
  (lambda ()
    (with-current-buffer (get-buffer-create feature-customization-test-buffer)
      (fill-paragraph))))

(Then "^test buffer contains \"\\([^\"]+\\)\" same lines long \"\\([^\"]+\\)\" chars$"
  (lambda (num-lines line-length)
    (with-current-buffer (get-buffer-create feature-customization-test-buffer)
      (should (= (string-to-number num-lines)
                 (count-lines (point-min) (point-max))))
      (dotimes (_ (string-to-number num-lines))
        (should (= (string-to-number line-length)
                   (length (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))))))))

(After
 (kill-buffer (get-buffer-create feature-customization-test-buffer)))
