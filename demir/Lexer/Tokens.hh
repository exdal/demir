#ifndef TOKEN
    #define TOKEN(name)
#endif

#ifndef KEYWORD
    #define KEYWORD(name, str) TOKEN(name)
#endif

#ifndef PUNCTUATOR
    #define PUNCTUATOR(name, str) TOKEN(name)
#endif

#ifndef LITERAL
    #define LITERAL(name) TOKEN(name)
#endif

#ifndef MISC
    #define MISC(name) TOKEN(name)
#endif

// TOP
MISC(Eof)

KEYWORD(True, "true")
KEYWORD(False, "false")
KEYWORD(Let, "let")
KEYWORD(Var, "var")
KEYWORD(Fn, "fn")
KEYWORD(Return, "return")
KEYWORD(If, "if")
KEYWORD(Else, "else")
KEYWORD(While, "while")
KEYWORD(Break, "break")
KEYWORD(Continue, "continue")
KEYWORD(Match, "match")
KEYWORD(Struct, "struct")

PUNCTUATOR(Equal, "=")
PUNCTUATOR(Add, "+")
PUNCTUATOR(Sub, "-")
PUNCTUATOR(Mul, "*")
PUNCTUATOR(Div, "/")
PUNCTUATOR(Comma, ",")
PUNCTUATOR(Dot, ".")
PUNCTUATOR(Colon, ":")
PUNCTUATOR(SemiColon, ";")
PUNCTUATOR(Backslash, "\\")
PUNCTUATOR(Exclaim, "!")
PUNCTUATOR(Question, "?")
PUNCTUATOR(Hash, "#")
PUNCTUATOR(Modulo, "%")
PUNCTUATOR(ParenLeft, "(")
PUNCTUATOR(ParenRight, ")")
PUNCTUATOR(BraceLeft, "{")
PUNCTUATOR(BraceRight, "}")
PUNCTUATOR(SquareLeft, "[")
PUNCTUATOR(SquareRight, "]")
PUNCTUATOR(AngleLeft, "<")
PUNCTUATOR(AngleRight, ">")
PUNCTUATOR(BitAnd, "&")
PUNCTUATOR(BitXor, "^")
PUNCTUATOR(BitOr, "|")
PUNCTUATOR(BitNot, "~")
PUNCTUATOR(SingleQuote, "'")
PUNCTUATOR(Quote, "\"")
PUNCTUATOR(At, "@")
PUNCTUATOR(Dollar, "$")
PUNCTUATOR(Range, "..")
PUNCTUATOR(RangeEqual, "..=")
PUNCTUATOR(Ellipsis, "...")

PUNCTUATOR(AddEqual, "+=")
PUNCTUATOR(SubEqual, "-=")
PUNCTUATOR(MulEqual, "*=")
PUNCTUATOR(DivEqual, "/=")
PUNCTUATOR(GreaterEqual, "<=")
PUNCTUATOR(LessEqual, ">=")
PUNCTUATOR(Arrow, "->")
PUNCTUATOR(ShipRight, "=>")
PUNCTUATOR(LogicalAnd, "&&")
PUNCTUATOR(LogicalOr, "||")
PUNCTUATOR(CompareEqual, "==")
PUNCTUATOR(CompareNotEqual, "!=")
PUNCTUATOR(ShiftLeft, "<<")
PUNCTUATOR(ShiftRight, ">>")

LITERAL(Identifier)
LITERAL(StringLiteral)
LITERAL(IntegerLiteral)
LITERAL(FloatingPointLiteral)
LITERAL(LineComment)
LITERAL(BlockComment)

#undef TOKEN
#undef KEYWORD
#undef PUNCTUATOR
#undef LITERAL
#undef MISC
