use super::*;
use crate::parser;
use crate::util::{BlockIdGenerator, IdentifierName};
use lalrpop_util::ParseError;

#[test]
fn test_number() {
    let number_parser = parser::NumberParser::new();

    let mut generator = BlockIdGenerator::new();

    assert_eq!(number_parser.parse(&mut generator, "123"), Ok(123));
    assert_eq!(number_parser.parse(&mut generator, "-123"), Ok(-123));
    assert_eq!(number_parser.parse(&mut generator, "0"), Ok(0));
    assert_eq!(number_parser.parse(&mut generator, "000"), Ok(0));
    assert_eq!(
        number_parser.parse(&mut generator, "66666666666666666666"),
        Err(ParseError::User {
            error: "number out of range"
        })
    );
    assert!(number_parser.parse(&mut generator, "123a").is_err());
}

#[test]
fn test_identifier() {
    let identifier_parser = parser::IdentParser::new();

    let mut generator = BlockIdGenerator::new();

    assert_eq!(
        identifier_parser.parse(&mut generator, "a"),
        Ok(IdentifierName {
            name: "a".to_string(),
            block_id: 0,
        })
    );

    assert_eq!(
        identifier_parser.parse(&mut generator, "a0"),
        Ok(IdentifierName {
            name: "a0".to_string(),
            block_id: 0,
        })
    );

    assert!(identifier_parser.parse(&mut generator, "0a").is_err());
    assert!(identifier_parser.parse(&mut generator, "_a").is_err());
    assert!(identifier_parser.parse(&mut generator, "a_").is_err());
}

#[test]
fn test_expr() {
    let expr_parser = parser::ExprParser::new();
    let mut generator = BlockIdGenerator::new();

    let input = "1 + 2 * -a";
    let result = expr_parser.parse(&mut generator, input);
    assert_eq!(
        result,
        Ok(Expr(Box::new(AddExpr::Add(
            Box::new(AddExpr::MulExpr(MulExpr::UnaryExpr(
                UnaryExpr::PrimaryExpr(PrimaryExpr::Number(1))
            ))),
            AddOp::Add,
            Box::new(MulExpr::Mul(
                Box::new(MulExpr::UnaryExpr(UnaryExpr::PrimaryExpr(
                    PrimaryExpr::Number(2)
                ))),
                MulOp::Mul,
                Box::new(UnaryExpr::Unary(
                    UnaryOp::Neg,
                    Box::new(UnaryExpr::PrimaryExpr(PrimaryExpr::LVal(LVal::Var(
                        "a".to_string()
                    ))))
                ))
            ))
        ))))
    );
}

#[test]
fn test_comp_unit() {
    let comp_unit_parser = parser::CompUnitParser::new();
    let mut generator = BlockIdGenerator::new();

    let input = r#"
    // This is a comment.
    int a = 10;
    const int b[10][15] = {1};
    /*
    This is a block comment.
    */
    void f() {
        int c = 30;
    }
    
    int g() {
        return 0;
    }
    "#;

    let result = comp_unit_parser.parse(&mut generator, input);

    assert!(result.is_ok());
    let result = result.unwrap();
    assert_eq!(result.items.len(), 4);
    let items = result.items;
    assert_eq!(
        items[0],
        GlobalItem::Decl(Decl::VarDecl(vec![VarDef::NormalVarDef(NormalVarDef {
            name: "a".to_string(),
            value: Some(Expr(Box::new(AddExpr::MulExpr(MulExpr::UnaryExpr(
                UnaryExpr::PrimaryExpr(PrimaryExpr::Number(10))
            )))))
        })]))
    );
    assert_eq!(
        items[1],
        GlobalItem::Decl(Decl::ConstDecl(vec![ConstDef::ArrayConstDef(
            ArrayConstDef {
                name: "b".to_string(),
                shape: vec![
                    ConstExpr(Box::new(AddExpr::MulExpr(MulExpr::UnaryExpr(
                        UnaryExpr::PrimaryExpr(PrimaryExpr::Number(10))
                    )))),
                    ConstExpr(Box::new(AddExpr::MulExpr(MulExpr::UnaryExpr(
                        UnaryExpr::PrimaryExpr(PrimaryExpr::Number(15))
                    ))))
                ],
                values: ConstArray::Array(vec![ConstArray::Val(ConstExpr(Box::new(
                    AddExpr::MulExpr(MulExpr::UnaryExpr(UnaryExpr::PrimaryExpr(
                        PrimaryExpr::Number(1)
                    )))
                )))]),
            }
        )]))
    );
}
