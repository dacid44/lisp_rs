use winnow::stream::ContainsToken;

use crate::{
    error::{LispError, LispResult},
    syntax::{Expression, Operator},
};

use self::{expr::parse_expression, tokens::tokenize};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Quote,
    Operator(Operator),
    Boolean(bool),
    Integer(i64),
    String(String),
    Name(String),
}

impl ContainsToken<Self> for Token {
    fn contains_token(&self, token: Self) -> bool {
        *self == token
    }
}

pub mod tokens {
    use winnow::{
        ascii::escaped_transform,
        combinator::{alt, delimited, not, peek, repeat, terminated},
        error::{ContextError, ParseError, ParserError},
        token::{one_of, take_till, take_while},
        Located, PResult, Parser,
    };

    use crate::syntax::Operator;
    use std::ops::Range;

    use super::Token;

    type TokenSpan = (Token, Range<usize>);

    fn is_name_char(c: char) -> bool {
        c.is_ascii_alphanumeric() || "*+!-_'?<>=/".contains(c)
    }

    fn is_whitespace(c: char) -> bool {
        c.is_ascii_whitespace() || c == ','
    }

    fn punctuation<'a, E: ParserError<Located<&'a str>>>(
        input: &mut Located<&'a str>,
    ) -> PResult<(Token, Range<usize>), E> {
        use Token as T;
        alt([
            '('.value(T::LeftParen),
            ')'.value(T::RightParen),
            '['.value(T::LeftBracket),
            ']'.value(T::RightBracket),
            '\''.value(T::Quote),
        ])
        .with_span()
        .parse_next(input)
    }

    fn operator<'a, E: ParserError<Located<&'a str>>>(
        input: &mut Located<&'a str>,
    ) -> PResult<TokenSpan, E> {
        use Operator as O;
        alt([
            "defn".value(O::Defn),
            "def".value(O::Def),
            "fn".value(O::Fn),
            "let".value(O::Let),
            "if".value(O::If),
            "quote".value(O::Quote),
            "eval".value(O::Eval),
        ])
        .map(Token::Operator)
        .with_span()
        .parse_next(input)
    }

    fn boolean<'a, E: ParserError<Located<&'a str>>>(
        input: &mut Located<&'a str>,
    ) -> PResult<TokenSpan, E> {
        alt(["true".value(true), "false".value(false)])
            .map(Token::Boolean)
            .with_span()
            .parse_next(input)
    }

    fn integer<'a, E: ParserError<Located<&'a str>>>(
        input: &mut Located<&'a str>,
    ) -> PResult<TokenSpan, E> {
        terminated(
            take_while(1.., ('0'..='9', '-')),
            peek(not(one_of(is_name_char))),
        )
        .parse_to()
        .map(Token::Integer)
        .with_span()
        .parse_next(input)
    }

    fn string<'a, E: ParserError<Located<&'a str>>>(
        input: &mut Located<&'a str>,
    ) -> PResult<TokenSpan, E> {
        delimited(
            '"',
            escaped_transform::<_, _, _, _, String>(
                take_till(1.., ['"', '\\']),
                '\\',
                alt(("\\".value("\\"), "\"".value("\""), "n".value("\n"))),
            ),
            '"',
        )
        .output_into()
        .map(Token::String)
        .with_span()
        .parse_next(input)
    }

    fn name<'a, E: ParserError<Located<&'a str>>>(
        input: &mut Located<&'a str>,
    ) -> PResult<TokenSpan, E> {
        take_while(1.., is_name_char)
            .map(|s: &str| Token::Name(s.to_string()))
            .with_span()
            .parse_next(input)
    }

    fn token<'a, E: ParserError<Located<&'a str>>>(
        input: &mut Located<&'a str>,
    ) -> PResult<TokenSpan, E> {
        alt([punctuation, operator, boolean, integer, string, name]).parse_next(input)
    }

    fn tokens<'a, E: ParserError<Located<&'a str>>>(
        input: &mut Located<&'a str>,
    ) -> PResult<Vec<TokenSpan>, E> {
        repeat(
            0..,
            alt((token.map(Some), take_while(1.., is_whitespace).value(None))),
        )
        .map(|v: Vec<_>| v.into_iter().flatten().collect())
        .parse_next(input)
    }

    pub fn tokenize(
        input: &str,
    ) -> Result<Vec<TokenSpan>, ParseError<Located<&str>, ContextError>> {
        // TODO: Replace this error type with a LispError
        tokens.parse(Located::new(input))
    }
}

pub mod expr {
    use literally::list;
    use winnow::{
        combinator::{alt, delimited, preceded, repeat},
        error::{ContextError, ParseError, StrContext},
        token::{any, one_of},
        PResult, Parser,
    };

    use crate::syntax::{Expression, Operator};

    use super::Token;

    fn single_token_expr<'a>(input: &mut &'a [Token]) -> PResult<Expression, ContextError> {
        any.verify_map(|t| match t {
            Token::Operator(op) => Some(Expression::Operator(op)),
            Token::Boolean(b) => Some(Expression::Boolean(b)),
            Token::Integer(x) => Some(Expression::Integer(x)),
            Token::String(s) => Some(Expression::String(s)),
            Token::Name(s) => Some(Expression::Name(s)),
            _ => None,
        })
        .context(StrContext::Label("single token expression"))
        .parse_next(input)
    }

    fn list<'a>(
        start: Token,
        end: Token,
    ) -> impl Parser<&'a [Token], Vec<Expression>, ContextError> {
        delimited(one_of(start), repeat(0.., expression), one_of(end))
    }

    fn expression<'a>(input: &mut &'a [Token]) -> PResult<Expression, ContextError> {
        alt((
            single_token_expr,
            preceded(one_of(Token::Quote), expression)
                .map(|expr| Expression::List(list![Expression::Operator(Operator::Quote), expr]))
                .context(StrContext::Label("quote")),
            list(Token::LeftParen, Token::RightParen)
                .map(|v| Expression::List(v.into_iter().collect()))
                .context(StrContext::Label("list")),
            list(Token::LeftBracket, Token::RightBracket)
                .map(Expression::Vector)
                .context(StrContext::Label("vector")),
        ))
        .parse_next(input)
    }

    pub fn parse_expression(
        input: &[Token],
    ) -> Result<Expression, ParseError<&[Token], ContextError>> {
        expression.parse(input)
    }
}

pub fn parse(input: &str) -> LispResult<Expression> {
    parse_expression(
        &tokenize(input)
            .map_err(|err| LispError::TokenError(err.to_string()))?
            .into_iter()
            .map(|(t, _)| t)
            .collect::<Vec<_>>()[..],
    )
    .map_err(|err| LispError::SyntaxError(format!("{err:?}")))
}

#[cfg(test)]
mod tests {
    use literally::list;

    use crate::{
        parser::parse,
        syntax::{Expression, Operator},
    };

    #[test]
    fn test_simple_expressions() {
        assert_eq!(parse("def"), Ok(Expression::Operator(Operator::Def)));
        assert_eq!(parse("42"), Ok(Expression::Integer(42)));
    }

    #[test]
    fn test_names() {
        assert_eq!(parse("asdf"), Ok(Expression::Name("asdf".to_string())));
        assert_eq!(parse("3ab"), Ok(Expression::Name("3ab".to_string())));
    }

    #[test]
    fn test_lists() {
        use Expression as E;
        assert_eq!(
            parse("(1 2,3 ,4, 5)"),
            Ok(E::List(list![
                E::Integer(1),
                E::Integer(2),
                E::Integer(3),
                E::Integer(4),
                E::Integer(5)
            ]))
        );
        assert_eq!(
            parse("(((((42), ) ,),) )"),
            Ok(E::List(list![E::List(list![E::List(list![E::List(
                list![E::List(list![E::Integer(42)])]
            )])])])),
        );
    }
}
