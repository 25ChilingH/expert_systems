/*
** September 9, 2022
** Chiling Han
**
** A user-interfaced program to generate a count of the alphabet letters in a given text
** for ATCS:Expert Systems.
** 
** slice$ - returns a list of characters from a string
** histo  - prints the character count for each letter of the alphabet used in a paragraph of ASCII text
**
** To run the program, run Jess in terminal and then type (batch "./histogram.clp").
*/

(batch "./utilities_v4.clp") ; import the utilities functions

/*
** This function explodes and returns a list of characters given a string.
** 
*/
(deffunction slice$ (?string)
   (bind ?list ())

   (for (bind ?i 1) (<= ?i (str-length ?string)) (++ ?i)
      (bind ?char (sub-string ?i ?i ?string))
      (bind ?list (append$ ?list ?char))
   )

   (return ?list)
) ; deffunction slice$

/*
** This function asks the user for a paragraph and then prints the number of times each character from A-Z
** regardless of case appears in the text.
**
** It creates a fixed list of size 256 filled with 0s and explodes the string into a list of characters using slice$. Then,
** it converts each character into its ascii code to increment the corresponding character count in the fixed list. It
** iterates through the fixed list, adding the lowercase and uppercase character count of the alphabet to print this
** final count.
**
** Function assumes that the user input will be a paragraph of ASCII text, meaning character codes from 0-255.
*/
(deffunction histo ()
   (bind ?HIGHEST_ASCII_VAL 255) ; constant for highest ascii character code program accepts is 255
                                 ; used to create the fixed array of 256 filled with 0s

   (bind ?ASCII_UPPER_A 65)      ; constant for ascii code value of character "A" is 65
                                 ; used as the lower bound for counting number of uppercase alphabetical characters in text

   (bind ?ASCII_UPPER_Z 90)      ; constant for ascii code value of character "Z" is 90
                                 ; used as the upper bound for counting number of uppercase alphabetical characters in text

   (bind ?ASCII_LOWER_A 97)      ; constant for ascii code value of character "a" is 97
                                 ; used to also count the number of lowercase alphabetical characters in text

   (bind ?countAlpha ())
   (for (bind ?i 0) (<= ?i ?HIGHEST_ASCII_VAL) (++ ?i)
      (bind ?countAlpha (append$ ?countAlpha 0l))
   )

   (bind ?string (askline "Enter some text: "))
   (bind ?charList (slice$ ?string))

   (foreach ?char ?charList
      (bind ?index (asc ?char))
      (++ ?index)

      (bind ?countAlpha (replace$ ?countAlpha ?index ?index (+ (nth$ ?index ?countAlpha) 1)))
   )

   (for (bind ?i ?ASCII_UPPER_A) (<= ?i ?ASCII_UPPER_Z) (++ ?i)
      (bind ?upperCount (nth$ (+ ?i 1) ?countAlpha))
      (bind ?lowerCount (nth$ (+ ?i (- ?ASCII_LOWER_A ?ASCII_UPPER_A) 1) ?countAlpha))

      (printline (str-cat "The number of " (asciiToChar ?i) "'s are: " (+ ?upperCount ?lowerCount)))
   )

   (return)
) ; deffunction histo