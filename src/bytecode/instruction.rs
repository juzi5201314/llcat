use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction {
    // Constants and Stack Manipulation
    Const(usize),  // Push constant from constant pool onto stack (usize is index in Chunk.constants)
    Pop,          // Remove top value from stack
    Dup,          // Duplicate the top value on the stack

    // Variable Access
    GetGlobal(String), // Push value of global variable onto stack
    SetGlobal(String), // Pop value from stack and assign to global variable

    GetLocal(usize), // Fetch a local variable at `usize` offset from the base of the current stack frame
    SetLocal(String), // Set a local variable at `usize` offset from the base of the current stack frame

    GetVariable(String),

    // Arithmetic and Logical Operations
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Gt,
    Lt,
    Ge,
    Le,
    And,
    Or,
    Not,
    Neg,

    // String Operations
    StringConcat,

    // Array Operations
    ArrayNew,      // Create a new, empty array on the heap.
    ArrayGet,      // Array[Index], Array and Index need to be on the stack
    ArraySet,      // Array[Index] = Value, need Array, Index and Value on the stack

    // Object Operations
    ObjectNew,
    ObjectGet(String), //Object.Key , where object is on top of the stack
    ObjectSet(String), // Object.Key = Value, the stack needs Object, then Value

    // Control Flow
    Jump(usize),     // Unconditional jump to instruction offset
    JumpIfFalse(usize), // Jump if top of stack is false (or falsy)

    // Function calls
    Call(Rc<String>, usize),    // Call function with arity arguments.
    Return,

    DebugPrint, // Print the value on top of stack
}