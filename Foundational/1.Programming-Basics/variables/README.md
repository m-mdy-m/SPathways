# Tipes:

# Variable

“Variable” is a term frequently used in research projects. It is pertinent to define and identify the variables while designing quantitative research projects. A variable incites excitement in any research than constants. It is therefore critical for beginners in research to have clarity about this term and the related concepts. Variable, to put in layman statement is something that can change and or can have more than one value. ''A variable, as the name implies, is 1” something that varies”. It may be weight, height, anxiety levels, income, body temperature and so on. Each of these properties varies from one person to another and also has different values along a continuum. It could be demographic, physical or social and include religion, income, occupation, temperature, humidity, language, food, fashion, etc. Some variables can be quite concrete and clear, such as gender, birth order, types of blood group etc while others can be considerably more abstract and vague. “Variable is a property that takes on different 2 3 values''. It is also a logical grouping of attributes. Attributes are characteristics or qualities that describe an object. For example if gender is a variable then male and female are the attributes. If residence is the variable then urban, semi urban, rural become the attributes. So attributes here describe the residence of an individual. It is pertinent for a researcher to know as how certain variables within a study are related to each other. It is thus important to define the variables to facilitate accurate explanation of the relationship between the variables. There is no limit to the number of variables that can be measured, although the more variables, the more complex the
study and the more complex the statistical analysis. Moreover the longerthe list of variables, the longer the time required for data collection. are qualities, properties, or characteristics of person, things, or situations that change or vary. Chin and kramer stated that ‘variables are concepts at different level of abstraction that are concisely defined to promote their measurement or manipulation within study’. Variables are classified based on their nature, action, and effects on the variables. All research projects are based around variables. A variable is the characteristic or attribute of an individual, group, educational system, or the environment that is of interest in a research study. Variables can be straightforward and easy to measure, such as gender, age, or course of study. Other variables are more complex, such as socioeconomic status, academic achievement, or attitude toward school. Variables may also include an aspect of the educational system, such as a specific teaching method or counseling program. Characteristics of the environment may also be variables, such as the amount of school funding or availability of computers. Therefore, once the general research topic has been identified, the researcher should identify the key variables of interest. For example, a researcher is interested in low levels of literacy. Literacy itself is still a broad topic. In most instances, the broad topic and general variables need to be specifically identified. For example, the researcher needs to identify specific variables that define literacy: reading fluency (the ability to read a text out loud), reading comprehension (understanding what is read), vocabulary, interest in reading, etc. If a researcher is interested in motivation, what specific motivation variables are of interest: external motivation, goals, need for achievement, etc? Reading other research studies about your chosen topic will help you better identify the specific variables of interest. From a programmer’s point of view, a variable is a location in your computer’s memory in which you can store a value and from which you can later retrieve that value. To understand this, you must first understand a bit about how computer memory works. Your computer’s memory can be thought of as a series of cubby holes, all lined up in a long row. Each cubby hole—or memory location—is numbered sequentially. These numbers are known as memory addresses. Variables not only have addresses, they have names. For example, you might create a variable named myAge. Your variable is a label on one of these cubby holes so that you can find it easily, without knowing its actual memory address.

## What Is RAM?

RAM is Random Access Memory. It is the electronic memory your computer uses while executing programs. Any information in RAM is lost when your computer is turned off. When you run your program, it is loaded into RAM from the disk file (hard drive storage). All variables are created in RAM as well. When programmers talk of memory, it is usually RAM to which they are referring.

## Setting Aside Memory

When you define a variable in C++, you must tell the compiler not only what its name is, but also what kind of information it will hold: integer, character, and so forth. This is the variable’s type. Another word for type that you may see is datatype. The type of the variable tells the compiler how much room to set aside in memory to hold the variable’s value. Each cubby is one byte large. If the type of variable you create is two bytes in size, it needs two bytes of memory, or two cubbies. The type of the variable (for example, int) tells the compiler how much memory (how many cubby holes) to set aside for the variable. Because computers use bits and bytes to represent values, and because memory is measured in bytes, it is important that you understand and are comfortable with these concepts.

## Data and Memory

• Each piece of data used in a program must be stored in the computer’s memory.
• The term variable is used to refer to the place in memory where a data item is stored.
• Each variable has a name, a datatype and a fixed size.
• Computer memory is composed of electronic circuits.
• Each circuit is either on or off.
• All data must be encoded in some way to be represented in a series of “on”s and “off”s.

## Binary Numbers

• The binary digits 1 and 0 can be used to represent a circuit that is on or off.
• Each binary digit is called a bit.
• The positional number system that uses 0 and 1 as digits and base 2 is called the binary number system.

## signed and unsigned

In addition, all these types come in two varieties: signed and unsigned.
Sometimes you need negative numbers, and sometimes you don’t. Integers (shortand long) without the word “unsigned” are assumed to be signed. signed integers are either negative or positive. unsigned integers are always positive. Remember: signed is the default for integer variables. Because you have the same number of bytes (and therefore the number of bits) for both signed and unsigned integers, the largest number you can store in an unsigned integer is twice as big as the largest positive number you can store in a signed integer. An unsigned short integer can handle numbers from 0 to 65,535. Half the numbers represented by a signed short are negative, thus a signed short can only represent numbers from –32,768 to 32,767.
Bit for signed and unsigned Integers
For signed integers, one bit is used to handle the sign itself; for unsigned that bit is used for more numbers. If you counted all the possible numbers available in signed and unsigned, you would find that you have the same count. The difference is how they are represented!

### Unsigned Integers

• 255 is the largest unsigned integer that can be represented in 8 bits.
 Can you develop a formula that uses an exponent to express this?
 Can you generalize this formula to give you the largest unsigned integer that can be represented in 16 bits? 32 bits? N bits?
 Why does this matter to you as a programmer?

### Signed Integers

• By definition, -1 is the number that can be added to 1 to get 0.
00000001

- ????????
  00000000
  • Before you can answer that question you have to be able to add binary numbers.
  • The left most bit is the sign bit.
  • 11111111 = -1
   It’s the largest negative integer.
  • 10000000 = - 128
   It’s the smallest negative integer that can be represented in 8 bits.
   Can you generalize that to 16 bits? 32 bits? N bits?
   What’s the formula for the largest positive signed integer?
  • The notation for negative binary numbers is called 2’s complement.
  • To represent a negative # in 2’s complement

1. Convert the absolute value of the # to its binary representation
2. Flip each of the bits in the number from step 1 to the “opposite” binary digit.
3. Add 1 to the result from step 2. • Let’s verify that with –1 and – 128.
