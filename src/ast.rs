#[derive(Debug, PartialEq, Clone)]
pub struct CompUnit {
    pub items: Vec<GlobalItem>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum GlobalItem {
    Decl(Decl),
    FuncDef(FuncDef),
}

/// Represent const declaration or non-const variable declaration.
///
/// One declaration statement may contains more than one declaration.
#[derive(Debug, PartialEq, Clone)]
pub enum Decl {
    ConstDecl(Vec<ConstDef>),
    VarDecl(Vec<VarDef>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ConstDef {
    NormalConstDef(NormalConstDef),
    ArrayConstDef(ArrayConstDef),
}

#[derive(Debug, PartialEq, Clone)]
pub struct NormalConstDef {
    pub name: String,
    pub value: ConstExpr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayConstDef {
    pub name: String,
    pub shape: Vec<ConstExpr>,
    pub values: ConstArray,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ConstArray {
    Val(ConstExpr),
    Array(Vec<ConstArray>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum VarDef {
    NormalVarDef(NormalVarDef),
    ArrayVarDef(ArrayVarDef),
}

#[derive(Debug, PartialEq, Clone)]
pub struct NormalVarDef {
    pub name: String,
    pub value: Option<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayVarDef {
    pub name: String,
    pub shape: Vec<ConstExpr>,
    pub values: Option<ExprArray>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprArray {
    Val(Expr),
    Array(Vec<ExprArray>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr(pub Box<AddExpr>);

#[derive(Debug, PartialEq, Clone)]
pub struct FuncDef {
    pub name: String,
    pub params: Vec<FuncFParam>,
    pub ret_type: DataType,
    pub body: Block,
}

/// Represent function parameter in declaration.
#[derive(Debug, PartialEq, Clone)]
pub enum FuncFParam {
    NormalFParam(NormalFParam),
    ArrayFParam(ArrayFParam),
}

#[derive(Debug, PartialEq, Clone)]
pub struct NormalFParam {
    pub name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayFParam {
    pub name: String,
    /// Whether the array has a placeholder. For example, `int a[]` has a placeholder.
    pub placeholder: bool,
    pub shape: Vec<ConstExpr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataType {
    Void,
    Int,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub id: i32,
    pub items: Vec<BlockItem>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BlockItem {
    Stmt(Stmt),
    Decl(Decl),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Assign(Assign),
    Expr(Expr),
    Block(Block),
    If(If),
    While(While),
    Return(Option<Expr>),
    Break,
    Continue,
    Empty,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assign {
    pub target: LVal,
    pub value: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LVal {
    Var(String),
    ArrayElem(ArrayElem),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayElem {
    pub name: String,
    pub indices: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct If {
    pub cond: LOrExpr,
    pub then_stmt: Box<Stmt>,
    pub else_stmt: Option<Box<Stmt>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct While {
    pub cond: LOrExpr,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LOrExpr {
    LAndExpr(LAndExpr),
    Or(Box<LOrExpr>, Box<LAndExpr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum LAndExpr {
    EqExpr(EqExpr),
    And(Box<LAndExpr>, Box<EqExpr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum EqOp {
    Eq,
    Ne,
}

#[derive(Debug, PartialEq, Clone)]
pub enum EqExpr {
    RelExpr(RelExpr),
    Eq(Box<EqExpr>, EqOp, Box<RelExpr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum RelOp {
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, PartialEq, Clone)]
pub enum RelExpr {
    AddExpr(AddExpr),
    Rel(Box<RelExpr>, RelOp, Box<AddExpr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum AddOp {
    Add,
    Sub,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AddExpr {
    MulExpr(MulExpr),
    Add(Box<AddExpr>, AddOp, Box<MulExpr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
}

#[derive(Debug, PartialEq, Clone)]
pub enum MulExpr {
    UnaryExpr(UnaryExpr),
    Mul(Box<MulExpr>, MulOp, Box<UnaryExpr>),
}

/// Unary operator.
#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    /// Positive(+).
    Pos,
    /// Negative(-).
    Neg,
    /// Logical not(!).
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryExpr {
    PrimaryExpr(PrimaryExpr),
    FuncCall(FuncCall),
    Unary(UnaryOp, Box<UnaryExpr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrimaryExpr {
    Expr(Expr),
    LVal(LVal),
    Number(i32),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncCall {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConstExpr(pub Box<AddExpr>);

#[cfg(test)]
mod test_ast;
