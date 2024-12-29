## Imperative Programming Paradigms: Suggested Projects and Languages

To deeply understand Imperative, Structured, and Procedural Programming, here are project suggestions and the best languages to use for each paradigm. Each project is chosen to align with the paradigm’s characteristics, and the suggested languages are chosen based on how well they naturally support the paradigm.

### **1. Imperative Programming**

- **Why Imperative?** Focuses on step-by-step instructions and maintaining state changes.

#### **Language: C**

**Why C?**

- C is one of the most foundational imperative languages.
- It operates close to the hardware, emphasizing explicit control flow, memory management, and the sequential execution of instructions.

**Projects and Challenges:**

1. **[Calculator Program](./1.Imperative/1.Calculator-Program/)**

   - Build a console-based calculator that supports basic operations (add, subtract, multiply, divide) with user input.
   - **Challenge:** Extend it to support operations like factorials, trigonometric functions, and error handling for invalid inputs.

2. **[Snake Game](https://github.com/medishen/sgt)**

   - Build a terminal-based snake game.
   - **Challenge:** Implement levels, save high scores, and make the game more interactive with dynamic screen sizes.

3. **[Memory Allocator](https://github.com/medishen/pick.git)** // Maybe it will be implemented at another time :))

   - Implement your version of `malloc` and `free` to understand memory management.
   - **Challenge:** Add error detection, alignment, and performance optimization.

4. **REPL (Read-Eval-Print Loop)** // Maybe it will be implemented at another time :))

- **Objective**: Build a simple REPL for a mathematical expression evaluator.
- **Description**: Implement a REPL where the user can input mathematical expressions (e.g., `3 + 4 * 2`) and get immediate feedback with the result.
- **Challenge**:
  - Add support for parentheses and basic operator precedence.
  - Implement error handling for invalid syntax or division by zero.

5. **Simple Shell** // Maybe it will be implemented at another time :))

- **Objective**: Create a basic Unix-like shell that can execute system commands.
- **Description**: Implement a shell that takes user input, parses it, and executes the corresponding command (e.g., `ls`, `pwd`, `echo`).
- **Challenge**:
  - Add support for piping commands (`ls | grep txt`).
  - Implement redirection to handle output to files (`echo "Hello" > file.txt`).
  - Add signal handling for terminating processes (`Ctrl+C`).

6. **[TideityIQ](https://github.com/medishen/TideityIQ)**

- **Objective**: Develop a command-line tool that analyzes the algorithmic complexity (time complexity) of code snippets in Python, JavaScript, and C.
- **Description**: The program should parse user-supplied code snippets, identify loops, function calls, and recursive patterns, and compute time complexities in terms of Big O, Θ (Theta), and Ω (Omega) notations.
- **Challenge**:
  - Extend support for additional languages, such as Java or Rust.
  - Implement dynamic visualization of algorithmic complexity using ASCII charts.
  - Add static analysis for identifying potential bottlenecks and optimization suggestions.
  - Include advanced complexity patterns like amortized analysis for data structures (e.g., dynamic arrays).

---

### **2. Structured Programming**

- **Why Structured?** Emphasizes control flow through sequence, selection, and iteration without relying on goto.

#### **Language: Python**

**Why Python?**

- Python’s clean syntax and indentation-based structure make it ideal for practicing structured programming concepts like loops, conditionals, and modular design.

**Projects and Challenges:**

1. **Student Management System**

   - Build a CLI app to manage students' grades and records.
   - **Challenge:** Add search, update, and reporting functionality with a hierarchical menu structure.

2. **Maze Solver**

   - Write a program that finds the shortest path through a maze (provided as a grid).
   - **Challenge:** Implement a visualization for the maze and the solving process.

3. **Expense Tracker**

   - Create an expense tracker that categorizes spending and provides analytics.
   - **Challenge:** Export reports in different formats (CSV, JSON, or text) and handle file I/O.

4. **Library Management System**

   - Build a CLI-based library system with book borrowing and returning features.
   - **Challenge:** Introduce fine calculations for overdue books and a reservation system.

5. **Basic Encryption/Decryption Tool**
   - Implement simple ciphers like Caesar cipher or substitution cipher.
   - **Challenge:** Extend it to handle complex algorithms like Vigenère cipher and add file encryption.

---

### **3. Procedural Programming**

- **Why Procedural?** Focuses on breaking the program into reusable procedures or functions.

#### **Language: JavaScript**

**Why JavaScript?**

- JavaScript is versatile for both procedural and event-driven programming.
- It allows you to write functions, organize reusable code blocks, and directly interact with the DOM for visual feedback.

**Projects and Challenges:**

1. **Todo List App**

   - Build a browser-based todo list with simple add, edit, delete functionality.
   - **Challenge:** Add data persistence using `localStorage` and filtering for completed/incomplete tasks.

2. **Clock with Alarms**

   - Create a digital clock that allows users to set alarms.
   - **Challenge:** Include a snooze feature, recurring alarms, and custom alarm tones.

3. **Weather Dashboard**

   - Use a weather API to display weather data for a user-specified location.
   - **Challenge:** Add a 5-day forecast and an option to display weather graphs.

4. **Basic Drawing App**

   - Implement a canvas-based drawing app with tools like brush, eraser, and color picker.
   - **Challenge:** Add undo/redo functionality and save the drawing as an image.

5. **Number Puzzle Game**
   - Create a sliding number puzzle (e.g., 15-puzzle) in the browser.
   - **Challenge:** Track move counts and add a timer to encourage speed-solving.

---

### Why These Languages?

- **C**: Best for **imperative programming** because it allows you to experience low-level control over execution flow, memory, and logic explicitly.
- **Python**: Ideal for **structured programming** because of its simplicity and natural enforcement of proper structure.
- **JavaScript**: Great for **procedural programming** as it allows building both functional and event-driven projects easily with a natural progression to higher paradigms.
