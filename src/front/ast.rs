use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct CompUnit {
    pub items: Vec<GlobalItem>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum GlobalItem {
    Decl(Decl),
    FuncDef(Rc<FuncDef>),
}

/// Represent const declaration or non-const variable declaration.
///
/// One declaration statement may contains more than one declaration.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Decl {
    ConstDecl(Vec<Rc<ConstDef>>),
    VarDecl(Vec<Rc<VarDef>>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ConstDef {
    NormalConstDef(NormalConstDef),
    ArrayConstDef(ArrayConstDef),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct NormalConstDef {
    pub name: String,
    pub value: ConstExpr,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ArrayConstDef {
    pub name: String,
    pub shape: Vec<ConstExpr>,
    pub values: ConstArray,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ConstArray {
    Val(ConstExpr),
    Array(Vec<ConstArray>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum VarDef {
    NormalVarDef(NormalVarDef),
    ArrayVarDef(ArrayVarDef),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct NormalVarDef {
    pub name: String,
    pub value: Option<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ArrayVarDef {
    pub name: String,
    pub shape: Vec<ConstExpr>,
    pub values: Option<ExprArray>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ExprArray {
    Val(Expr),
    Array(Vec<ExprArray>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Expr(pub Rc<AddExpr>);

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FuncDef {
    pub name: String,
    pub params: Vec<Rc<FuncFParam>>,
    pub ret_type: DataType,
    pub body: Block,
}

/// Represent function parameter in declaration.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum FuncFParam {
    NormalFParam(NormalFParam),
    ArrayFParam(ArrayFParam),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct NormalFParam {
    pub name: String,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ArrayFParam {
    pub name: String,
    /// Whether the array has a placeholder. For example, `int a[]` has a placeholder.
    pub placeholder: bool,
    pub shape: Vec<ConstExpr>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum DataType {
    Void,
    Int,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Block {
    pub id: i32,
    pub items: Vec<BlockItem>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum BlockItem {
    Stmt(Stmt),
    Decl(Decl),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Assign {
    pub target: LVal,
    pub value: Expr,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum LVal {
    Var(String),
    ArrayElem(ArrayElem),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ArrayElem {
    pub name: String,
    pub indices: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct If {
    pub cond: LOrExpr,
    pub then_stmt: Rc<Stmt>,
    pub else_stmt: Option<Rc<Stmt>>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct While {
    pub cond: LOrExpr,
    pub body: Rc<Stmt>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum LOrExpr {
    LAndExpr(LAndExpr),
    Or(Rc<LOrExpr>, Rc<LAndExpr>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum LAndExpr {
    EqExpr(EqExpr),
    And(Rc<LAndExpr>, Rc<EqExpr>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum EqOp {
    Eq,
    Ne,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum EqExpr {
    RelExpr(RelExpr),
    Eq(Rc<EqExpr>, EqOp, Rc<RelExpr>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum RelOp {
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum RelExpr {
    AddExpr(AddExpr),
    Rel(Rc<RelExpr>, RelOp, Rc<AddExpr>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum AddOp {
    Add,
    Sub,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum AddExpr {
    MulExpr(MulExpr),
    Add(Rc<AddExpr>, AddOp, Rc<MulExpr>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum MulExpr {
    UnaryExpr(UnaryExpr),
    Mul(Rc<MulExpr>, MulOp, Rc<UnaryExpr>),
}

/// Unary operator.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum UnaryOp {
    /// Positive(+).
    Pos,
    /// Negative(-).
    Neg,
    /// Logical not(!).
    Not,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum UnaryExpr {
    PrimaryExpr(PrimaryExpr),
    FuncCall(FuncCall),
    Unary(UnaryOp, Rc<UnaryExpr>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum PrimaryExpr {
    Expr(Expr),
    LVal(LVal),
    Number(i32),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FuncCall {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ConstExpr(pub Rc<AddExpr>);

impl VarDef {
    pub fn get_name(&self) -> &str {
        match self {
            VarDef::NormalVarDef(normal_var_def) => &normal_var_def.name,
            VarDef::ArrayVarDef(array_var_def) => &array_var_def.name,
        }
    }
}

impl ConstDef {
    pub fn get_name(&self) -> &str {
        match self {
            ConstDef::NormalConstDef(normal_const_def) => &normal_const_def.name,
            ConstDef::ArrayConstDef(array_const_def) => &array_const_def.name,
        }
    }
}

impl FuncDef {
    pub fn get_name(&self) -> &str {
        &self.name
    }
}

impl FuncFParam {
    pub fn get_name(&self) -> &str {
        match self {
            FuncFParam::NormalFParam(normal_f_param) => &normal_f_param.name,
            FuncFParam::ArrayFParam(array_f_param) => &array_f_param.name,
        }
    }
}

#[cfg(test)]
mod test_ast;
