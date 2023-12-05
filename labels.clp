/*
** December 16, 2022
** Chiling Han
**
** An expert system to label Github issues using only their titles with probabilities calculated for
** four distinct tags for ATCS:Expert Systems. The four labels are documentation, question, bug,
** and enhancement.
** 
** My expert for this project was Ted Chang, an IBM open source software engineer.
** 
** Facts, which represent specific content in the title, are backward chained so that whenever the fact is
** needed for pattern matching, its corresponding rule will fire. These rules' RHS asks the user
** if the title has a specific content and also calculates the probability of each label from the given information.
** Labels are represented by forward-chained rules. Once the LHS of a label rule is satisfied, the system outputs
** the probability of that label and halts the rule engine.
** 
** firstToken    - returns lowercased first character of user's response to a question
** validate      - returns "y" or "n" according to user's input and keeps asking the given question till valid input
** checkQuestion - calls validate; returns the character validate returns
** average       - returns the average of two numbers
** bindProb      - calculates the probability of the labels via averaging
** label         - prints calculated probability and determined label
** finish        - halts the rule engine
** 
** To run the program, run Jess in terminal and then type (batch "./labels.clp").
*/

(clear)
(reset)

;;;;;;;;;;;;

(batch "./utilities_v4.clp")

/*
** This global variable is a list containing the current calculated probability percentage for all four labels.
** The percentage at index 1 is the probability that the issue is documentation, index 2 is question, index 3
** is bug, and index 4 is enhancement.
*/
(defglobal ?*LABEL_PROB* = (create$ 25 25 25 25))

(defglobal ?*DOC_INDEX* = 1)         ; constant for documentation label's index in ?*LABEL_PROB*
(defglobal ?*QUESTION_INDEX* = 2)    ; constant for question label's index in ?*LABEL_PROB*
(defglobal ?*BUG_INDEX* = 3)         ; constant for bug label's index in ?*LABEL_PROB*
(defglobal ?*ENHANCEMENT_INDEX* = 4) ; constant for enhancement label's index in ?*LABEL_PROB*

(do-backward-chaining doc)
(do-backward-chaining questionMark)
(do-backward-chaining interrogative)
(do-backward-chaining moreInfo)
(do-backward-chaining bugOrErrorOrIssue)
(do-backward-chaining fails)
(do-backward-chaining fix)
(do-backward-chaining improveOrAdd)
(do-backward-chaining inRepo)
(do-backward-chaining deviates)
(do-backward-chaining text)
(do-backward-chaining update)

/********************************
**
** Rule to capture the program's uncertainty in labeling
** the issue. This rule will go onto the agenda immediately,
** but because the salience level is so low, it will
** just sit there until there are no rules left to
** fire. Only then will it be examined, at which point
** if we have not determined a label, we can't match the 
** user's inputted keywords of the title to a label in our program.
**
**/
(defrule uncertain "Can't find a definite label that matches user's answers to questions."
   (declare (salience -100))
   =>
   (printline "Sorry, I cannot determine a definite label given the answers you've provided about the Github issue's title.")
   (finish)
)

/********************************
**
** Startup rule that will fire first because of the high salience
**
**/
(defrule promptUser "Rule that starts the Github issues' labeling expert system."
   (declare (salience 100))
   =>
   (printout t crlf
               "Github Issue Labeling Expert System" crlf
               "Read just the title of your Github repository's issue." crlf
               "I will ask a series of questions about the title to try to label your issue." crlf
               "The four labels that I will use are documentation, question, bug, or enhancement." crlf
               "You can input Y for yes and N for no to the questions I ask about the issue's title." crlf
               "--------------------------------------------------------" crlf crlf)
) ; defrule promptUser

/********************************
**
** Backward Chaining Rules
** Only ask for more information on the Github issue's
** title if the label has not been determined. Will also
** use weights corresponding to each fact to calculate confidence
** of outputted label.
**
*/
(defrule hasDoc
   (need-doc ?)
   =>
   (bind ?IMPACT_DOC 100) ; constant for probability that issue concerns 'documentation' if its title contains 'doc'
   
   (bind ?check (checkQuestion "Does the title include the word 'doc'"))
   (bindProb ?check ?*DOC_INDEX* ?IMPACT_DOC)
   (assert (doc ?check))
) ; defrule hasDoc

(defrule textualAspect "title deals with textual aspects of repo"
   (need-text ?)
   =>
   (bind ?IMPACT_DOC 100) ; constant for probability that issue concerns 'documentation' if its title deals with textual aspects
   
   (bind ?check (checkQuestion "Does the title deal with textual aspects of the repository (.md files, comments, etc.)"))
   (bindProb ?check ?*DOC_INDEX* ?IMPACT_DOC)
   (assert (text ?check))
) ; defrule textualAspect

(defrule hasUpdate
   (need-update ?)
   =>
   (bind ?IMPACT_DOC 80)     ; constant for probability that issue concerns 'documentation' if its title contains 'update'
   (bind ?IMPACT_ENHANCE 20) ; constant for probability that issue concerns 'enhancement' if its title contains 'update'

   (bind ?check (checkQuestion "Does the title include the word 'update'"))
   (bindProb ?check ?*ENHANCEMENT_INDEX* ?IMPACT_ENHANCE)
   (bindProb ?check ?*DOC_INDEX* ?IMPACT_DOC)
   (assert (update ?check))
) ; defrule hasUpdate

(defrule hasQuestionMark
   (need-questionMark ?)
   =>
   (bind ?IMPACT_QUESTION 100) ; constant for probability that issue concerns 'question' if its title contains '?'
   
   (bind ?check (checkQuestion "Does the title include a question mark"))
   (bindProb ?check ?*QUESTION_INDEX* ?IMPACT_QUESTION)
   (assert (questionMark ?check))
) ; defrule hasQuestionMark

(defrule hasInterrogative
   (need-interrogative ?)
   =>
   (bind ?IMPACT_QUESTION 100) ; constant for probability that issue concerns 'question' if its title contains interrogatives
   
   (bind ?check (checkQuestion "Does the title include any interrogatives (how, what, etc.)"))
   (bindProb ?check ?*QUESTION_INDEX* ?IMPACT_QUESTION)
   (assert (interrogative ?check))
) ; defrule hasInterrogative

(defrule needsMoreInfo "title is asking for more information about the codebase"
   (need-moreInfo ?)
   =>
   (bind ?IMPACT_QUESTION 100) ; constant for probability that issue concerns 'question' if the title asks for more information

   (bind ?check (checkQuestion "Is the title asking for more information about the codebase"))
   (bindProb ?check ?*QUESTION_INDEX* ?IMPACT_QUESTION)
   (assert (moreInfo ?check))
) ; defrule needsMoreInfo

(defrule deviatesFromExpected "title states that code deviates from expected behavior"
   (need-deviates ?)
   =>
   (bind ?IMPACT_BUG 100) ; constant for probability that issue concerns 'bug' if the code deviates from expected behavior

   (bind ?check (checkQuestion "Does the title state that the code deviates from expected behavior"))
   (bindProb ?check ?*BUG_INDEX* ?IMPACT_BUG)
   (assert (deviates ?check))
) ; defrule deviatesFromExpected

(defrule hasBugOrErrorOrIssue
   (need-bugOrErrorOrIssue ?)
   =>
   (bind ?IMPACT_BUG 65)     ; constant for probability that issue concerns 'bug' if title contains 'bug/error/issue'
   (bind ?IMPACT_DOC 25)     ; constant for probability that issue concerns 'documentation' if title contains 'bug/error/issue'
   (bind ?IMPACT_ENHANCE 10) ; constant for probability that issue concerns 'enhancement' if title contains 'bug/error/issue'
   
   (bind ?check (checkQuestion "Does the title include the word 'bug', 'error', or 'issue'"))
   (bindProb ?check ?*BUG_INDEX* ?IMPACT_BUG)
   (bindProb ?check ?*DOC_INDEX* ?IMPACT_DOC)
   (bindProb ?check ?*ENHANCEMENT_INDEX* ?IMPACT_ENHANCE)
   (assert (bugOrErrorOrIssue ?check))
) ; defrule hasBugOrErrorOrIssue

(defrule hasFails
   (need-fails ?)
   =>
   (bind ?IMPACT_BUG 100) ; constant for probability that issue concerns 'bug' if its title contains 'fails'

   (bind ?check (checkQuestion "Does the title include the word 'fails'"))
   (bindProb ?check ?*BUG_INDEX* ?IMPACT_BUG)
   (assert (fails ?check))
) ; defrule hasFails

(defrule hasFix
   (need-fix ?)
   =>
   (bind ?IMPACT_ENHANCE 100) ; constant for probability that issue concerns 'enhancement' if its title contains 'fix'

   (bind ?check (checkQuestion "Does the title include the word 'fix'"))
   (bindProb ?check ?*ENHANCEMENT_INDEX* ?IMPACT_ENHANCE)
   (assert (fix ?check))
) ; defrule hasFix

(defrule hasImproveOrAdd
   (need-improveOrAdd ?)
   =>
   (bind ?IMPACT_ENHANCE 100) ; constant for probability that issue concerns 'enhancement' if its title contains 'improve/add'

   (bind ?check (checkQuestion "Does the title include the word 'improve' or 'add'"))
   (bindProb ?check ?*ENHANCEMENT_INDEX* ?IMPACT_ENHANCE)
   (assert (improveOrAdd ?check))
) ; defrule hasImproveOrAdd

(defrule codeInRepo "title relates to code in the codebase"
   (need-inRepo ?)
   =>
   (bind ?IMPACT_DOC 30)     ; constant for probability that issue concerns 'documentation' if title relates to code in repo
   (bind ?IMPACT_ENHANCE 35) ; constant for probability that issue concerns 'enhancement' if title relates to code in repo
   (bind ?IMPACT_BUG 35)     ; constant for probability that issue concerns 'bug' if title relates to code in repo

   (bind ?check (checkQuestion "Does the title relate to code included in the repository"))
   (bindProb ?check ?*DOC_INDEX* ?IMPACT_DOC)
   (bindProb ?check ?*ENHANCEMENT_INDEX* ?IMPACT_ENHANCE)
   (bindProb ?check ?*BUG_INDEX* ?IMPACT_BUG)
   (assert (inRepo ?check))
) ; defrule codeInRepo

/********************************
**
** Forward Chaining Rules
** The program labels the Github issue given the user's information
** about its title once the dynamically asserted 
** facts matches the pattern. The rules will short circuit so that
** no unnecessary questions are asked.
** 
*/

(defrule documentation-1
   (inRepo y)
   (doc y) ; check if title contains 'doc' keyword
   =>
   (label ?*DOC_INDEX*)
)

(defrule documentation-2
   (inRepo n) ; differentiates from all other labels
   (text y)
   (update y)
   =>
   (label ?*DOC_INDEX*)
)

(defrule documentation-3
   (inRepo y)
   (text y) ; differentiates from 'bug' label
   (bugOrErrorOrIssue y)
   =>
   (label ?*DOC_INDEX*)
)

(defrule question-1
   (questionMark y)  ; first, check if title is a question and interrogative not used as other parts of speech
   (interrogative y) ; then, check if title contains interrogatives
   =>
   (label ?*QUESTION_INDEX*)
)

(defrule question-2
   (questionMark n) ; title doesn't contain question mark
   (moreInfo y)     ; instead, check if title's asking for more information even without question mark
   =>
   (label ?*QUESTION_INDEX*)
)

(defrule bug-1
   (inRepo y)
   (text n)
   (fails y) ; check if code in repository is failing and differentiates from 'enhancement' label
   =>
   (label ?*BUG_INDEX*)
)

(defrule bug-2
   (inRepo y)
   (text n)
   (fails n)
   (bugOrErrorOrIssue y)
   (fix n) ; differentiates from 'enhancement' label
   =>
   (label ?*BUG_INDEX*)
)

(defrule bug-3
   (inRepo y)
   (bugOrErrorOrIssue n)
   (fails n)
   (deviates y) ; check if title's describing unwanted behavior even without bug-related keywords
   =>
   (label ?*BUG_INDEX*)
)

(defrule enhancement-1
   (inRepo y)
   (text n)
   (fails n)        ; differentiates from 'bug' label
   (improveOrAdd y) ; check if improving or adding to the codebase
   =>
   (label ?*ENHANCEMENT_INDEX*)
)

(defrule enhancement-2
   (inRepo y)
   (text n)
   (fails n)  ; differentiates from 'bug' label
   (update y) ; check if updating a part of the codebase
   =>
   (label ?*ENHANCEMENT_INDEX*)
)

(defrule enhancement-3
   (inRepo y)
   (text n)
   (bugOrErrorOrIssue y)
   (fix y) ; differentiates from 'bug' label
   =>
   (label ?*ENHANCEMENT_INDEX*)
)

/********************************
**
** Functions
**
*/

/*
** This function returns the lowercased first character in the user's response when the user is asked the given question.
** 
** Function assumes ?q is a string that is the question to ask the user
*/
(deffunction firstToken (?question)
   (return (lowcase (sub-string 1 1 (askQuestion ?question))))
)

/*
** This function returns "y" for yes and "n" for no. It will continuously ask the user to re-input their answer
** if their answer does not match the above characters: "y" or "n".
** 
** Function assumes ?q is a string that is the question to ask the user
*/
(deffunction validate (?q)
   (bind ?char (firstToken ?q))
   (while (not (or (= ?char "y") (= ?char "n"))) do
      (printline "Please enter a Y for yes or N for no")
      (bind ?char (firstToken ?q))
   )

   (return ?char)
) ; deffunction validate

/*
** This function returns a symbol y if the first character is a Y or y and n if the first character is a N or n.
** It will validate the user's answers by calling validate.
** 
** Function assumes ?prompt is a string that is the question to ask the user
*/
(deffunction checkQuestion (?prompt)   
   (bind ?char (validate ?prompt))

   (return (sym-cat ?char))
) ; deffunction checkQuestion

/*
** This function returns the average of two numbers.
** 
** Function assumes ?num1 and ?num2 are numbers to be averaged
*/
(deffunction average (?num1 ?num2)
   (return (/ (+ ?num1 ?num2) 2))
)

/*
** When ?check is equal to the symbol y, this function changes the probability of the label at a
** given index in the ?*LABEL_PROB* list with the average of ?numProb and the label's current probability.
** 
** Function assumes ?check is either the symbol y or n, ?index is a positive integer between 1-4, and ?numProb
** is a probability that will be averaged with the label's current probability
*/
(deffunction bindProb (?check ?index ?numProb)
   (if (= ?check y) then
      (bind ?*LABEL_PROB* (replace$ ?*LABEL_PROB* ?index ?index (create$ (average (nth$ ?index ?*LABEL_PROB*) ?numProb))))
   )
   (return)
) ; deffunction bindProb

/*
** The system will declare that your Github issue should belong to the label at the given index of ?*LABEL_PROB*.
** It will also output the probability of this label and will halt the rule engine.
** 
** Function assumes ?index is a positive integer between 1-4
*/
(deffunction label (?index)
   (bind ?LABELS (create$ "documentation" "question" "bug" "enhancement")) ; list of four labels with globally defined indices

   (printline (str-cat "The probability that your Github issue's label is '" (nth$ ?index ?LABELS) "'"
                        " is " (nth$ ?index ?*LABEL_PROB*) "%."))
   (finish)

   (return)
) ; deffunction label

/*
** This function exits the program.
** 
*/
(deffunction finish ()
   (halt)
   (return)
) ; deffunction finish