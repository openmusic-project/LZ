;=============================
; LZ Library
; by G. Assayag
;=============================
;; OM(6)-sepcific aspects of the LZ library:


;;;====================================
;;; LZ DATA IS TOO BIG: NEVER SAVE/LOCK THE VALUE

(defclass boxforlz (OMBoxCall) ())

(defmethod get-boxcallclass-fun ((self (eql 'LZify))) 'boxforlz)

(defmethod omNG-save ((self boxforlz) &optional (values? nil))
  "Save a box"
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self)))
         )
    `(om-load-boxcall ',(saveBox? self) ,(name self) ',(save-reference self) ',inputs ,(om-save-point (frame-position self)) 
                      ,(om-save-point (frame-size self)) nil nil ,(frame-name self) ,(numouts self))))

(defun update-lz-boxes (oldbox newbox)
  (setf (value newbox) nil)
  (setf (frame-position newbox) (borne-position (frame-position oldbox)))
  (setf (frame-size newbox) (frame-size oldbox))
  (setf (frame-name newbox) (frame-name oldbox))
  (setf (allow-lock newbox) nil)
  (setf (inputs newbox) (eval (omNG-copy (inputs oldbox))))
  (set-box-to-inputs (inputs newbox) newbox)
  newbox)

(defmethod omNG-copy ((self boxforlz))
  `(let* ((copy ,(omNG-make-new-boxcall (fdefinition (reference self))
                                        (frame-position self)
                                        (name self))))
     (setf copy (update-lz-boxes ,self copy))
     copy))


;;;====================================
;;; ADAPTS ALL MAIN FUNCTIONS TO CLASS MIDIFILE

(defmethod! Transposer ((midifile MidiFile) (offset integer))
  (transposer (mf-info midiFile)))

(defmethod! TimeScaler ((midifile MidiFile) (scaler float))
   (TimeScaler (mf-info midifile) scaler))

(defmethod! Crop ((midifile midifile) (begin number) (end number))
   (Crop (mf-info midifile) begin end))


(defmethod! Midi->chordseq ((self list))
   :initvals '(nil )
   :indoc '("A list, Output of a mf-info box, or the output of a MidiFile box" )
   :icon :lz 
   :doc  "Converts the output of a midifile box, or the output of a mf-info box 
to a  single chord-seq object.
The tracks will be merged, and events occuring quasi-simultaneously
will be grouped into chords with regards to the approximation parameter 'delta value' (ms)
available in the preferences of OpenMusic.
"
   (let ((newcs (make-instance 'chord-seq))
         (midilist (sort (loop for item in self append item) '< :key 'second)))
     (setQValue newcs 1000 :recursive nil)
     (setf (inside newcs) nil)
     (setf (inside newcs)  (make-quanti-chords midilist *global-deltachords*))
     (adjust-extent newcs)
     (QNormalize newcs)
     newcs))

(defmethod! Midi->chordseq ((midifile MidiFile))
   (Midi->chordseq (mf-info midifile)))

(defmethod! Midi->chordseqs ((midifile MidiFile))
   (Midi->chordseqs (mf-info midifile)))

(defmethod! Midi->Cross ((midifile MidiFile) &optional
                         (legatime nil) (arpegtime 50) (releastime 0) (staccatime 0) (toltime 10))
  (Midi->Cross (mf-info midifile) legatime arpegtime releastime staccatime toltime))


