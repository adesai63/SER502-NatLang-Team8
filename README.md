# SER502-NatLang-Team8

## NatLang

**NatLang** is a conversational programming language inspired by natural English. Programs begin with **"Hi"** and end with **"Bye"**, making coding feel like a dialogue. It's designed for clarity, readability, and ease of learning, especially for beginners.

The goal of NatLang is to bridge the gap between human language and programming syntax, allowing users to write code in a way that feels natural and intuitive.

NatLang makes programming more accessible to:
- Non-programmers  
- Students  
- Anyone intimidated by traditional syntax  

With its friendly structure and human-like keywords, NatLang encourages logical thinking while removing unnecessary complexity. This makes it an excellent tool for:
- Educational purposes  
- Workshops  
- Early exposure to computational thinking

---

## 🧩 Components

### 1. Lexical Analyzer
We use **Python** to tokenize the NatLang code, identifying:
- Keywords
- Identifiers
- Values
- Punctuation

This step converts the raw code into a stream of meaningful tokens that can be processed by the parser.

### 2. DCG for Parsing
Using **Definite Clause Grammar (DCG)** in **Prolog**, we define the structure of valid NatLang syntax and generate the **parse tree**.  
This helps enforce language rules and ensures the code follows the expected structure.

### 3. Parse Tree Traversal
A **Python interpreter** processes the parse tree:
- Handles assignments
- Executes print statements
- Manages memory and variables
- Simulates execution of the program

---

> ⚠️ **Note:**  
> We have selected the tech stack as per our current understanding of the project.  
> This is subject to change as the project evolves.

---


