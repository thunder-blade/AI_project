(defglobal ?*analysis* = NIL) ; Global Variable
(defglobal ?*advice-file* = advice.txt) ; Global Variable

;;;; Symptom Details Template ;;;;

(deftemplate symptom-details
    (slot symptom-name)
    (slot plant-name)
    (slot disease-or-pest)
    (slot prescence (default no))
    (slot weight (default 0)))

(deftemplate disease-weight
    (slot disease-or-pest-name )
    (slot plant-name )
    (slot weight (default 0)))

; Deffunctions 

(deffunction quest (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction YorN (?question)
   (bind ?response (quest ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
        then yes 
    else no))

(deffunction display-cal (?response)
   (if (or (eq ?response yes) (eq ?response y))
        then (bind ?*analysis* TRUE) 
    else (bind ?*analysis* FALSE) ))


(deffunction which-plant (?question)
    (bind ?response (quest ?question 1 2 3))
    (if (eq ?response 1)
        then cabbage 
    else (if (eq ?response 2)
        then banana
    else (if (eq ?response 3)
        then rose 
    else nil))))

    
; Get confidence level and Give Diagnosis based on Threshold
(deffunction diagnose-plant (?plant-name ?disease-or-pest ?threshold)
    (bind ?weight 1)
    (if ?*analysis* ; if analysis mode is TRUE
        then
        (printout t "" crlf 
                    "Calculating total confidence level for "
                    ?disease-or-pest "..." crlf))
    (do-for-all-facts ((?g symptom-details)) 
        (and 
            (eq ?g:prescence yes)
            (eq ?g:plant-name ?plant-name)
            (eq ?g:disease-or-pest ?disease-or-pest))
        (if ?*analysis*
            then
            (printout t ?g:symptom-name " confidence level: " ?g:weight crlf)
            (printout t "Calculation: " ?weight " * (1 - "  ?g:weight ")" crlf))
        (bind ?weight (* ?weight (- 1 ?g:weight)))
        (if ?*analysis*
            then
            (printout t "= " ?weight crlf "" crlf)))
    (if ?*analysis*
        then
        (printout t "--------------------------------------------------" crlf
        "Total confidence level for " ?disease-or-pest " (1 - " ?weight ")"crlf))
    (bind ?weight (- 1 ?weight))
    (if ?*analysis*
        then
        (printout t "= " ?weight crlf 
        "And Threshold" crlf "= "?threshold crlf
        "--------------------------------------------------" crlf "" crlf))
    (assert (disease-weight (disease-or-pest-name ?disease-or-pest)
                            (plant-name ?plant-name)
                            (weight ?weight)))
    (if (> ?weight ?threshold)
        then TRUE))


;;;; Query Rules ;;;;
;____________________;

(defrule determine-plant "Rules for when no plant name or diagnosis is available"
    (analysis)
    (not (diagnosis ?))
    (not (plant-name ?))
    =>
    (assert (plant-name (which-plant "Which Plant has a problem?  (1.Cabbage 2.Banana 3.Rose)? "))))

;;;; Query Plant Conditions ;;;;


; Dynamically generate rules to check for diagnosis;
(deffunction get-diagnosis (
                                ?plant-name
                                ?disease-or-pest)
    (bind ?symptom-rule-name (str-cat "check-" ?disease-or-pest "-diagnosis"))
    (build (str-cat
            "(defrule " ?symptom-rule-name
                "(not (diagnosis ?))
                 (plant-name " ?plant-name ")
                =>
                (assert
                    (" ?symptom-rule-name ")))"
            )))

(deffunction give-advice (?plant-name ?disease-or-pest $?filename)
    (if (eq (length ?filename) 0)
        then (bind ?filename advice.txt)
    else 
        (bind ?filename (implode$ (subseq$ ?filename 1 1))))
    (open ?filename file-data) ; open the file and store data in file-data
    (bind ?stop FALSE) ; initialize stop variable to FALSE
    (bind ?plant-name-tmp (read file-data)) ; 1st line of the beginning of a new pest or disease is the plant name
    (bind ?disease-or-pest-tmp (read file-data)) ; 2nd line of the beginning of a new pest or disease is the disease or pest name
    (printout t crlf crlf "------------ADVICE-------------" crlf crlf)
    (while (not ?stop) ; while stop variable is not TRUE
        (bind ?temp-line (readline file-data))
        (if (and (eq ?plant-name ?plant-name-tmp)
            (eq ?disease-or-pest ?disease-or-pest-tmp))
            then 
                (if (eq ?temp-line EOF) ; if End of File
                    then 
                    (printout t crlf "Goodbye!" crlf)
                    (bind ?stop TRUE) ; Set stop variable to TRUE
                else (if (eq ?temp-line "ENDGROUP") ; If "ENDGROUP" check for the diagnosis of the disease or pest
                    then
                    (printout t crlf crlf 
                        "                        ***" crlf
                        "        Thank You for Using our Expert System!" crlf crlf)
                    (bind ?stop TRUE)
                else (if (eq ?temp-line "") ; If reads empty string, do nothing
                        then (printout t "") ; Do nothing
                else
                    (printout t crlf ?temp-line crlf)
                    )))
        else (if (eq ?temp-line "ENDGROUP") ; if End of File
                then 
                (bind ?plant-name-tmp (read file-data)) ; 1st line of the beginning of a new pest or disease is the plant name
                (bind ?disease-or-pest-tmp (read file-data)) ; 2nd line of the beginning of a new pest or disease is the disease or pest name
            else if (eq ?temp-line "EOF"
                then
                (printout t "Sorry, we could not find any advice for this Diagnosis")
                (bind ?stop TRUE)
                )))
        ) ; end while loop
    (close)) ;close the file when done

; Dynamically generate diagnosis rules ;
(deffunction create-diagnosis-rules (?plant-name ?disease-or-pest ?diagnosis-st)
    (bind ?diagnosis-rule-name (str-cat "confirm-" ?disease-or-pest))
    (bind ?diagnosis-fact-name (str-cat "check-" ?disease-or-pest "-diagnosis"))
    (bind ?threshold 0.7) ; threshold value
    (build (str-cat
            "(defrule " ?diagnosis-rule-name "
              ?f <- (" ?diagnosis-fact-name ")
                =>
                (retract ?f)
                (if (diagnose-plant " ?plant-name " " ?disease-or-pest " " ?threshold ")
                    then
                    (assert (diagnosis " ?diagnosis-st " " ?plant-name " " ?disease-or-pest "))
                    ))"
            )))


    


(deffunction read-from-diagnoses-file (?file)
    (open ?file file-data) ; open the file and store data in file-data
    (bind ?stop FALSE) ; initialize stop variable to FALSE
    (bind ?plant-name (read file-data)) ; 1st line of the beginning of a set of diagnoses is the plant name
    (while (not ?stop) ; while stop variable is not TRUE
        (bind ?temp-line (readline file-data)) ; read entire line from text file
        (if (eq ?temp-line EOF) ; if End of File
            then (bind ?stop TRUE) ; Set stop variable to TRUE
        else (if (eq ?temp-line "ENDGROUP") ; If "ENDGROUP" check for the diagnosis of the disease or pest
            then
            (bind ?plant-name (read file-data)) ; Read plant name of next set of diagnoses
        else (if (eq ?temp-line "") ; If reads empty string, do nothing
                then (printout t "") ; Do nothing
        else
            (bind ?exp-line (explode$ ?temp-line)) ; delimit the line read using spaces
            (create-diagnosis-rules ;create the rules needed to diagnose the plant
                ?plant-name
                (implode$ (subseq$ ?exp-line 1 1))
                (implode$ (subseq$ ?exp-line 2 2)))
            ))))
    (close)) ;close the file when done
	
	
	;Rose
		(defrule determine-yellow-patch-leaves
			(not (diagnosis ?))
                 (plant-name rose)
                =>
                (assert (symptom-details (symptom-name yellow-patch-leaves) (plant-name rose) (disease-or-pest rose-rust) (prescence  (YorN "Does the plant have yellow patches on its leaves? (yes/no?) ")) (weight 0.4))))
		(defrule determine-orange-spores-leaves
			(not (diagnosis ?))
                 (plant-name rose)
                =>
                (assert (symptom-details (symptom-name orange-spores-leaves) (plant-name rose) (disease-or-pest rose-rust) (prescence  (YorN "Does the plant have Orange pustules of spores underneath the leaves? (yes/no?) ")) (weight 0.5))))
		(defrule determine-leaves-fall
			(not (diagnosis ?))
                 (plant-name rose)
                =>
                (assert (symptom-details (symptom-name leaves-fall) (plant-name rose) (disease-or-pest rose-rust) (prescence  (YorN "Do the affected leaves fall prior to healthy ones? (yes/no?) ")) (weight 0.3))))
		(defrule determine-plants-defoliated
			(not (diagnosis ?))
                 (plant-name rose)
                =>
                (assert (symptom-details (symptom-name plants-defoliated) (plant-name rose) (disease-or-pest rose-rust) (prescence  (YorN "Are the plants defoliated in serious infections? (yes/no?) ")) (weight 0.6)))
				(get-diagnosis rose rose-rust))
				
		(defrule determine-black-spots-leaves
				(not (diagnosis ?))
                (plant-name rose)
                =>
                (assert (symptom-details (symptom-name black-spots-leaves) (plant-name rose) (disease-or-pest black-spot) (prescence  (YorN "Does the plant have black spots on its leaves? (yes/no?) ")) (weight 0.6))))
				
		(defrule determine-distance-between-spots
			(not (diagnosis ?))
			(plant-name rose)
			=>
			(assert (symptom-details (symptom-name distance-between-spots) (plant-name rose) (disease-or-pest black-spot) (prescence  (YorN "Are the spots as far as 12mm apart? (yes/no?) ")) (weight 0.5))))

		(defrule determine-circular-spots-irregular-edge-yellow-halo

			(not (diagnosis ?))
			(plant-name rose)
            =>
            (assert (symptom-details (symptom-name circular-spots-irregular-edge-yellow-halo) (plant-name rose) (disease-or-pest black-spot) (prescence  (YorN "Does the plant have circular spots with an irregular edge with a yellow halo? (yes/no?) ")) (weight 0.5))))

		(defrule determine-yellow-leaves-fall-early
			(not (diagnosis ?))
				(plant-name rose)
            =>
            (assert (symptom-details (symptom-name yellow-leaves-fall-early) (plant-name rose) (disease-or-pest black-spot) (prescence  (YorN "Do the plants' leaves turn yellow and fall early? (yes/no?) ")) (weight 0.4))))

		(defrule determine-continual-defoliation-cause-death
			(not (diagnosis ?))
            (plant-name rose)
            =>
            (assert (symptom-details (symptom-name continual-defoliation-cause-death) (plant-name rose) (disease-or-pest black-spot) (prescence  (YorN "Does the plant have continual defoliation that causes weakness, dieback or death of the plant? (yes/no?) ")) (weight 0.4)))
			(get-diagnosis rose black-spot))
			
		(defrule determine-chewed-irregular-shaped-holes
			(not (diagnosis ?))
				(plant-name rose)
                =>
                (assert (symptom-details (symptom-name chewed-irregular-shaped-holes) (plant-name rose) (disease-or-pest metallic-flea-beetles) (prescence  (YorN "Are there chewed holes of irregular shapes in young leaves and buds? (yes/no) ")) (weight 0.6))))

		(defrule determine-leaves-holes-enlarge
			(not (diagnosis ?))
                 (plant-name rose)
                =>
                (assert (symptom-details (symptom-name leaves-holes-enlarge) (plant-name rose) (disease-or-pest metallic-flea-beetles) (prescence  (YorN "Do the holes enlarge as the leaves enlarge? (yes/no) ")) (weight 0.6)))
				(get-diagnosis rose metallic-flea-beetles))
				

		(defrule determine-silvering-of-leaves
			(not (diagnosis ?))
                 (plant-name rose)
                =>
                (assert (symptom-details (symptom-name silvering-of-leaves) (plant-name rose) (disease-or-pest two-spotted-mites) (prescence  (YorN "Do the plants' leaves silver? (yes/no)?")) (weight 0.6))))
				
		(defrule determine-fine-webbing-eggs-underside
			(not (diagnosis ?))
                 (plant-name rose)
                =>
                (assert (symptom-details (symptom-name fine-webbing-eggs-underside) (plant-name rose) (disease-or-pest two-spotted-mites) (prescence  (YorN "Do the plants have fine webbing and eggs on the undersides of the leaves? (yes/no)? ")) (weight 0.5)))
				(get-diagnosis rose two-spotted-mites))
				
				
	
	;Cabbage 
	(defrule determine-irregular-yellow-patches-leaves
		(not (diagnosis ?))
		(plant-name cabbage)
        =>
		(assert (symptom-details (symptom-name irregular-yellow-patches-leaves) (plant-name cabbage) (disease-or-pest downy-mildew) (prescence (YorN "Does the plant have yellow patches on its leaves? (yes/no?) ")) (weight 0.5)))
	)
	(defrule determine-fluffy-gray-growth-leaves
		(not (diagnosis ?))
		(plant-name cabbage)
        =>
		(assert	(symptom-details (symptom-name fluffy-gray-growth-leaves) (plant-name cabbage) (disease-or-pest downy-mildew) (prescence (YorN "Does the plant have fluffy gray growth on underside of leaves? (yes/no)? ")) (weight 0.6)))
		(get-diagnosis cabbage downy-mildew)
	)
	
	(defrule determine-white-pustules
		(not (diagnosis ?))
		(plant-name cabbage)
        =>
		(assert (symptom-details (symptom-name white-pustules) (plant-name cabbage) (disease-or-pest white-rust) (prescence (YorN "Does the plant have white pustules on leaves, stems or flowers? (yes/no)? ")) (weight 0.6)))
	)
	
	(defrule determine-leaves-roll-thicken
		(not (diagnosis ?))
		(plant-name cabbage)
        =>
		(assert (symptom-details (symptom-name leaves-roll-thicken) (plant-name cabbage) (disease-or-pest white-rust) (prescence (YorN "Have the plant leaves rolled and thickened? (yes/no)? ")) (weight 0.4)))
		(get-diagnosis cabbage white-rust)
	)
	
	(defrule determine-irregularly-shaped-holes-leaves-stem
		(not (diagnosis ?))
		(plant-name cabbage)
        =>
		(assert (symptom-details (symptom-name irregularly-shaped-holes-leaves-stem) (plant-name cabbage) (disease-or-pest slugs-and-snails) (prescence (YorN "Do the plants have irregularly shaped holes in leaves and stems? (yes/no)? ")) (weight 0.4)))
	)
	
	(defrule determine-slime-trails
		(not (diagnosis ?))
		(plant-name cabbage)
        =>
		(assert (symptom-details (symptom-name slime-trails) (plant-name cabbage) (disease-or-pest slugs-and-snails) (prescence (YorN "Are there slime trails present on the plant and surrounding soil? (yes/no)? ")) (weight 0.6)))
	)
	
	(defrule determine-shredded-leaves
		(not (diagnosis ?))
		(plant-name cabbage)
        =>
		(assert (symptom-details (symptom-name shredded-leaves) (plant-name cabbage) (disease-or-pest slugs-and-snails) (prescence (YorN "Do the plants possess shredded leaves? (yes/no)? ")) (weight 0.4)))
		(get-diagnosis cabbage slugs-and-snails)
	)
	
	(defrule determine-ragged-holes
		(not (diagnosis ?))
		(plant-name cabbage)
        =>
		(assert (symptom-details (symptom-name ragged-holes) (plant-name cabbage) (disease-or-pest cabbageworm) (prescence (YorN "Do the plants have ragged holes in leaves bored into head? (yes/no)? ")) (weight 0.4)))
	)
	
	(defrule determine-frass
		(not (diagnosis ?))
		(plant-name cabbage)
        =>
		(assert (symptom-details (symptom-name frass) (plant-name cabbage) (disease-or-pest cabbageworm) (prescence (YorN "Do the plants have green-brown faeces(frass) on leaves? (yes/no)? ")) (weight 0.4)))
	)
	
	(defrule determine-caterpillar-green-hairy
		(not (diagnosis ?))
		(plant-name cabbage)
        =>
		(assert (symptom-details (symptom-name caterpillar-green-hairy) (plant-name cabbage) (disease-or-pest cabbageworm) (prescence (YorN "Presence of a caterpillar that is green and hairy? (yes/no)? ")) (weight 0.6)))
		(get-diagnosis cabbage cabbageworm)
	)
	
	
	;Banana
		(defrule determine-yellow-brown-watery-internal-tissue
			(not (diagnosis ?))
                 (plant-name banana)
                =>
                (assert (symptom-details (symptom-name yellow-brown-watery-internal-tissue) (plant-name banana) (disease-or-pest rhizome-rot) (prescence  (YorN "Is the internal tissue yellow/brown and watery? (yes/no)? ")) (weight 0.5)))
		)
				
		(defrule determine-pseudostem-breaks-from-rhizome
			(not (diagnosis ?))
                 (plant-name banana)
                =>
                (assert (symptom-details (symptom-name pseudostem-breaks-from-rhizome) (plant-name banana) (disease-or-pest rhizome-rot) (prescence  (YorN "Does the pseudostem break from the rhizome? (yes/no)? ")) (weight 0.5)))
		)
				
		(defrule determine-rhizome-fails-to-germinate
			(not (diagnosis ?))
                 (plant-name banana)
                =>
                (assert (symptom-details (symptom-name rhizome-fails-to-germinate) (plant-name banana) (disease-or-pest rhizome-rot) (prescence  (YorN "Does the rhizome fail to germinate (does not form new leaves)? (yes/no)? ")) (weight 0.6)))
				(get-diagnosis banana rhizome-rot))
				
		(defrule determine-chlorotic-mottling-stripes-on-foliage
			(not (diagnosis ?))
                 (plant-name banana)
                =>
                (assert (symptom-details (symptom-name chlorotic-mottling-stripes-on-foliage) (plant-name banana) (disease-or-pest banana-mosaic) (prescence  (YorN "Is there chlorotic mottling or stripes on foliage? (yes/no)? ")) (weight 0.6))))
				
		(defrule determine-distorted-fruit-with-chlorotic-streaks-mottling
			(not (diagnosis ?))
                 (plant-name banana)
                =>
                (assert (symptom-details (symptom-name distorted-fruit-with-chlorotic-streaks-mottling) (plant-name banana) (disease-or-pest banana-mosaic) (prescence  (YorN "Does the plant have distorted fruit which may have chlorotic streaks or mottling? (yes/no)? ")) (weight 0.6))))
				
		(defrule determine-distorted-leaves
			(not (diagnosis ?))
                 (plant-name banana)
                =>
                (assert (symptom-details (symptom-name distorted-leaves) (plant-name banana) (disease-or-pest banana-mosaic) (prescence  (YorN "Does the plant have distorted leaves? (yes/no)? ")) (weight 0.4))))
				
		(defrule determine-leaf-necrosis
			(not (diagnosis ?))
                 (plant-name banana)
                =>
                (assert (symptom-details (symptom-name leaf-necrosis) (plant-name banana) (disease-or-pest banana-mosaic) (prescence  (YorN "Do the leaves have black, brown or tan spots, holes or discolourations? (yes/no)? ")) (weight 0.5)))
				(get-diagnosis banana banana-mosaic))
				
		(defrule determine-deformed-plants-curled-shrivelled-leaves
			(not (diagnosis ?))
                 (plant-name banana)
                =>
                (assert (symptom-details (symptom-name deformed-plants-curled-shrivelled-leaves) (plant-name banana) (disease-or-pest banana-aphid) (prescence  (YorN "Are the plants deformed with curled, shrivelled leaves? (yes/no)? ")) (weight 0.4))))
				
		(defrule determine-galls-form-on-leaves
			(not (diagnosis ?))
                 (plant-name banana)
                =>
                (assert (symptom-details (symptom-name galls-form-on-leaves) (plant-name banana) (disease-or-pest banana-aphid) (prescence  (YorN "Do galls form on the leaves of the plant? (yes/no)? ")) (weight 0.5))))
				
		(defrule determine-colonies-of-aphids-present
			(not (diagnosis ?))
                 (plant-name banana)
                =>
                (assert (symptom-details (symptom-name colonies-of-aphids-present) (plant-name banana) (disease-or-pest banana-aphid) (prescence  (YorN "Are colonies of aphids present in the crown of the plant at the base of pseudostem or between the outer leaf sheaths? (yes/no)? ")) (weight 0.7)))
				(get-diagnosis banana banana-aphid))
				
		(defrule determine-visible-tunnels-in-corm
			(not (diagnosis ?))
                 (plant-name banana)
                =>
                (assert (symptom-details (symptom-name visible-tunnels-in-corm) (plant-name banana) (disease-or-pest banana-weevil) (prescence  (YorN "Do the plants have visible tunnels in corm as rounded holes up to 8 mm in diameter? (yes/no)? ")) (weight 0.5))))
				
		(defrule determine-plants-wilting-toppling
			(not (diagnosis ?))
                 (plant-name banana)
                =>
                (assert (symptom-details (symptom-name plants-wilting-toppling) (plant-name banana) (disease-or-pest banana-weevil) (prescence  (YorN "Do the plants wilt and topple over? (yes/no)? ")) (weight 0.4))))
				
		(defrule determine-destruction-root-system
			(not (diagnosis ?))
                 (plant-name banana)
                =>
                (assert (symptom-details (symptom-name destruction-root-system) (plant-name banana) (disease-or-pest banana-weevil) (prescence  (YorN "Do the plants face destruction of the root system? (yes/no)? ")) (weight 0.4))))
				
		(defrule determine-reduced-plant-growth
			(not (diagnosis ?))
                 (plant-name banana)
                =>
                (assert (symptom-details (symptom-name reduced-plant-growth) (plant-name banana) (disease-or-pest banana-weevil) (prescence  (YorN "Do the plants have reduced growth? (yes/no)? ")) (weight 0.4))))
				
		(defrule determine-reduced-fruit-production
			(not (diagnosis ?))
                 (plant-name banana)
                =>
                (assert (symptom-details (symptom-name reduced-fruit-production) (plant-name banana) (disease-or-pest banana-weevil) (prescence  (YorN "Do the plants have reduced fruit production? (yes/no)? ")) (weight 0.4))))
				
		(defrule determine-black-beetle-between-leaf-sheaths
			(not (diagnosis ?))
                 (plant-name banana)
                =>
                (assert (symptom-details (symptom-name black-beetle-between-leaf-sheaths) (plant-name banana) (disease-or-pest banana-weevil) (prescence  (YorN "Are there black hard-shelled beetles between leaf sheaths? (yes/no)? ")) (weight 0.6)))
				(get-diagnosis banana banana-weevil))
					
	
; Call the read-from-symptoms-file and read-from-diagnoses-file functions to generate the rules
(defrule begin
   (declare (salience 1))
   =>
   (read-from-diagnoses-file diagnoses.txt))

(defrule check-most-probable-diagnosis
   (not (diagnosis ?))
   =>
   (assert (check-most-probable)))
	
(defrule find-most-probable-disease
    (not (diagnosis ?))
    ?f <- (check-most-probable)
    (disease-weight (disease-or-pest-name ?disease-or-pest-name1)
                    (plant-name ?plant-name)
                    (weight ?weight1))
   (not (disease-weight (weight ?weight2&:(> ?weight2 ?weight1))))
   =>
   ; (retract ?f)
   (printout t crlf crlf "---------------------DIAGNOSIS--------------------" crlf crlf
                "  No sufficient information on the condition of your plant." crlf
    "  But your " ?plant-name " most likey suffers from " ?disease-or-pest-name1 crlf))





; Startup n Conclusion Rules

(defrule system-banner ""
    (declare (salience 10))
    =>
    (printout t crlf)
    (printout t "`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~" crlf)
    (printout t "        PLANT DOCTOR" crlf)
    (printout t "`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~`~")
    (printout t crlf crlf))

(defrule determine-analysis "Rules for when no analysis mode has been set"
    (declare (salience 9))
    (not (analysis ?))
    =>
    (display-cal (YorN "Do you want to see the calculations for threshold and confidence level? (yes/no)? "))
    (printout t "" crlf)
    (assert (analysis)))

(defrule print-diagnosis ""
    (declare (salience 10))
    (diagnosis ?statement ?plant-name ?disease-or-pest)
    =>
    (printout t crlf crlf)
    (printout t "---------------------DIAGNOSIS--------------------")
    (printout t crlf crlf)
    (format t " %s%n%n" ?statement)
    (give-advice ?plant-name ?disease-or-pest)
    (assert (diagnosis TRUE)))
