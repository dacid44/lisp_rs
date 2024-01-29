use std::num::ParseIntError;

use nom::{
    branch::alt,
    bytes::{
        complete::{is_a, take_while1},
        streaming::tag,
    },
    character::{
        complete::digit1,
        streaming::{char, multispace0, multispace1},
    },
    combinator::{all_consuming, map_res, opt, recognize, value},
    error::{FromExternalError, ParseError},
    multi::separated_list0,
    sequence::{delimited, tuple},
    IResult, Parser,
};

use crate::syntax::{Expression, Operator};

fn operator<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Operator, E> {
    alt((
        value(Operator::Lambda, char('\\')),
        value(Operator::Def, tag("def")),
    ))(input)
}

fn name_char(c: char) -> bool {
    !c.is_whitespace() && c != ',' && c != '(' && c != ')'
}

fn integer<'a, E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError>>(
    input: &'a str,
) -> IResult<&'a str, i32, E> {
    map_res(
        take_while1(name_char).and_then(all_consuming(digit1)),
        str::parse,
    )
    .parse(input)
}

fn name<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, String, E> {
    take_while1(name_char).map(ToString::to_string).parse(input)
}

pub fn expression<'a, E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError>>(
    input: &'a str,
) -> IResult<&'a str, Expression, E> {
    let separator = || recognize(tuple((multispace0, char(','), multispace0))).or(multispace1);

    alt((
        operator.map(Expression::Operator),
        integer.map(Expression::Integer),
        name.map(Expression::Name),
        delimited(
            char('(').and(multispace0),
            separated_list0(separator(), expression).map(Expression::List),
            opt(separator()).and(char(')')),
        ),
    ))(input)
}

#[test]
fn test_simple_expressions() {
    assert_eq!(
        expression::<()>("def"),
        Ok(("", Expression::Operator(Operator::Def)))
    );
    assert_eq!(expression::<()>("42"), Ok(("", Expression::Integer(42))));
}

#[test]
fn test_names() {
    assert_eq!(
        expression::<()>("asdf"),
        Ok(("", Expression::Name("asdf".to_string())))
    );
    assert_eq!(
        expression::<()>("3ab asdf"),
        Ok((" asdf", Expression::Name("3ab".to_string())))
    );
}

#[test]
fn test_lists() {
    use Expression as E;
    assert_eq!(
        expression::<()>("(1 2,3 ,4, 5)"),
        Ok((
            "",
            E::List(vec![
                E::Integer(1),
                E::Integer(2),
                E::Integer(3),
                E::Integer(4),
                E::Integer(5)
            ])
        ))
    );
    assert_eq!(
        expression::<()>("(((((42), ) ,),) )"),
        Ok((
            "",
            E::List(vec![E::List(vec![E::List(vec![E::List(vec![E::List(
                vec![E::Integer(42)]
            )])])])])
        )),
    );
}
