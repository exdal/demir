#include "demir/Lexer/Lexer.hh"

#include "demir/Core/Compiler.hh"
#include "demir/Lexer/Diagnostics.hh"
#include <charconv>

namespace demir {
auto is_alpha(c8 c) -> bool {
    return u32((c | ' ') - 'a') < 26;
}

auto is_digit(c8 c) -> bool {
    return u32(c - '0') < 10;
}

auto is_hex_digit(c8 c) -> bool {
    return u32(c - '0') < 10 || u32((c | ' ') - 'a') < 6;
}

auto is_space(c8 c) -> bool {
    return c == '\r' || c == '\n' || c == ' ' || c == '\t';
}

auto Lexer::tokenize(std::string_view source, bool skip_comments) -> std::vector<Token> {
    auto tokens = std::vector<Token>();
    auto lexer = Lexer{ .buffer_view = source };
    do {
        auto token = lexer.next(skip_comments);
        tokens.emplace_back(token);
    } while (!lexer.current.is(demir::TokenKind::eEof));

    return tokens;
}

auto Lexer::peek(this Lexer &self, u32 look_ahead) -> c8 {
    return (self.offset + look_ahead < self.buffer_view.size()) ? self.buffer_view[self.offset + look_ahead] : '\0';
}

auto Lexer::consume(this Lexer &self) -> void {
    DEMIR_EXPECT(self.peek() != '\n');
    self.offset++;
}

auto Lexer::consume_any(this Lexer &self) -> void {
    if (self.peek() == '\n') {
        self.line++;
        self.line_offset = self.offset + 1;
    }

    self.offset++;
}

auto Lexer::read_whitespaces(this Lexer &self) -> void {
    while (self.peek() != '\0' && is_space(self.peek())) {
        self.consume_any();
    }
}

auto Lexer::read_name(this Lexer &self) -> Token {
    auto start_pos = self.position();
    auto start_off = self.offset;

    do {
        self.consume();
    } while (is_alpha(self.peek()) || is_digit(self.peek()) || self.peek() == '_');

    auto name_str = std::string_view(self.buffer_view.data() + start_off, self.offset - start_off);
    auto kind = TokenKind::eIdentifier;
    auto token_kinds = token_kind_strings();

    for (usize i = 0; i < token_kinds.size(); i++) {
        auto &token_tag_sv = token_kinds[i];
        if (name_str == token_tag_sv) {
            kind = static_cast<TokenKind>(i);
            break;
        }
    }

    return Token(kind, Location(start_pos, self.position()), start_off, self.offset - start_off);
}

auto Lexer::read_number(this Lexer &self) -> Token {
    auto start_pos = self.position();
    auto start_off = self.offset;

    do {
        self.consume();
    } while (is_digit(self.peek()) || (self.peek() == '.' && is_digit(self.peek(1))));

    // TODO: Handle 1eX values
    auto number_str = std::string_view(self.buffer_view.data() + start_off, self.offset - start_off);
    i64 number_value = 0;
    auto r = std::from_chars(number_str.begin(), number_str.end(), number_value);
    if (r.ptr != number_str.end() || r.ec != std::errc{}) {
        throw LexerBadNumberError(Location(start_pos, self.position()));
    }

    return Token(TokenKind::eIntegerLiteral, Location(start_pos, self.position()), number_value);
}

auto Lexer::read_line_comment(this Lexer &self) -> Token {
    auto start_pos = self.position();
    auto start_off = self.offset;

    while (self.peek() != 0 && self.peek() != '\n') {
        self.consume();
    }

    return Token(TokenKind::eLineComment, Location(start_pos, self.position()), start_off, self.offset - start_off);
}

auto Lexer::read_block_comment(this Lexer &self) -> Token {
    auto start_pos = self.position();
    auto start_off = self.offset;

    while (self.peek() != 0 && !(self.peek() == '*' && self.peek(1) == '/')) {
        self.consume();
    }

    self.consume();
    self.consume();

    return Token(TokenKind::eBlockComment, Location(start_pos, self.position()), start_off, self.offset - start_off);
}

auto Lexer::read_backslahsed_string(this Lexer &self) -> void {
    self.consume_any();
    switch (self.peek()) {
        case '\0': {
            break;
        }
        case '\r': {
            self.consume();
            if (self.peek() == '\n') {
                self.consume_any();
            }
            break;
        }
        default: {
            self.consume_any();
        }
    }
}

auto Lexer::read_quoted_string(this Lexer &self) -> Token {
    auto start_pos = self.position();
    self.consume(); // eat quote
    auto start_off = self.offset;

    auto kind = TokenKind::eStringLiteral;
    while (self.peek() != '\"') {
        switch (self.peek()) {
            case 0:
            case '\r':
            case '\n': {
                // TODO: we don't know how to parse multiline strings yet
                self.consume_any();
            }
            case '\\': {
                self.read_backslahsed_string();
                break;
            }
            default: {
                self.consume();
            }
        }
    }

    self.consume();
    return Token(kind, Location(start_pos, self.position()), start_off, self.offset - start_off - 1);
}

auto Lexer::position(this Lexer &self) -> Position {
    return Position(self.line, self.offset - self.line_offset);
}

auto Lexer::next_line(this Lexer &self) -> void {
    while (self.peek() != '\0' && self.peek() != '\r' && self.peek() != '\n') {
        self.consume();
    }

    self.next_token();
}

auto Lexer::next_token(this Lexer &self) -> Token {
    auto start = self.position();
    auto cur_loc = Location(start, 1);

    switch (self.peek()) {
        case '\0': {
            return Token(TokenKind::eEof, cur_loc);
        }
        case '!': {
            self.consume();
            switch (self.peek()) {
                case '=': {
                    self.consume();
                    return Token(TokenKind::eCompareNotEqual, Location(start, 2));
                } break;
                default:;
            }
            return Token(TokenKind::eExclaim, cur_loc);
        }
        case '\'': {
            self.consume();
            return Token(TokenKind::eSingleQuote, cur_loc);
        }
        case '\"': {
            return self.read_quoted_string();
        }
        case '#': {
            self.consume();
            return Token(TokenKind::eHash, cur_loc);
        }
        case '%': {
            self.consume();
            return Token(TokenKind::eModulo, cur_loc);
        }
        case '(': {
            self.consume();
            return Token(TokenKind::eParenLeft, cur_loc);
        }
        case ')': {
            self.consume();
            return Token(TokenKind::eParenRight, cur_loc);
        }
        case '*': {
            self.consume();
            switch (self.peek()) {
                case '=': {
                    self.consume();
                    return Token(TokenKind::eMulEqual, Location(start, 2));
                }
                default:;
            }
            return Token(TokenKind::eMul, cur_loc);
        }
        case '+': {
            self.consume();
            switch (self.peek()) {
                case '=': {
                    self.consume();
                    return Token(TokenKind::eAddEqual, Location(start, 2));
                }
                default:;
            }
            return Token(TokenKind::eAdd, cur_loc);
        }
        case ',': {
            self.consume();
            return Token(TokenKind::eComma, cur_loc);
        }
        case '-': {
            if (is_digit(self.peek(1))) {
                return self.read_number();
            }

            self.consume();
            switch (self.peek()) {
                case '>': {
                    self.consume();
                    return Token(TokenKind::eArrow, Location(start, 2));
                }
                case '=': {
                    self.consume();
                    return Token(TokenKind::eSubEqual, Location(start, 2));
                }
                default:;
            }

            return Token(TokenKind::eSub, cur_loc);
        }
        case '.': {
            self.consume();
            if (self.peek() == '.') {
                self.consume();
                switch (self.peek()) {
                    case '.': {
                        self.consume();
                        return Token(TokenKind::eEllipsis, Location(start, 3));
                    } break;
                    case '=': {
                        self.consume();
                        return Token(TokenKind::eRangeEqual, Location(start, 3));
                    } break;
                    default:;
                }

                return Token(TokenKind::eRange, Location(start, 2));
            }

            return Token(TokenKind::eDot, cur_loc);
        }
        case '/': {
            self.consume();
            switch (self.peek()) {
                case '/': {
                    self.consume();
                    return self.read_line_comment();
                }
                case '*': {
                    self.consume();
                    return self.read_block_comment();
                }
                case '=': {
                    self.consume();
                    return Token(TokenKind::eDivEqual, Location(start, 2));
                }
                default:;
            }

            return Token(TokenKind::eDiv, cur_loc);
        }
        case '\\': {
            self.consume();
            return Token(TokenKind::eBackslash, cur_loc);
        }
        case ':': {
            self.consume();
            return Token(TokenKind::eColon, cur_loc);
        }
        case ';': {
            self.consume();
            return Token(TokenKind::eSemiColon, cur_loc);
        }
        case '<': {
            self.consume();
            switch (self.peek()) {
                case '<': {
                    self.consume();
                    return Token(TokenKind::eShiftLeft, Location(start, 2));
                } break;
                case '=': {
                    self.consume();
                    return Token(TokenKind::eLessEqual, Location(start, 2));
                }
                default:;
            }
            return Token(TokenKind::eAngleLeft, cur_loc);
        }
        case '>': {
            self.consume();
            switch (self.peek()) {
                case '>': {
                    self.consume();
                    return Token(TokenKind::eShiftRight, Location(start, 2));
                } break;
                case '=': {
                    self.consume();
                    return Token(TokenKind::eGreaterEqual, Location(start, 2));
                }
                default:;
            }
            return Token(TokenKind::eAngleRight, cur_loc);
        }
        case '=': {
            self.consume();
            switch (self.peek()) {
                case '>': {
                    self.consume();
                    return Token(TokenKind::eShipRight, Location(start, 2));
                }
                case '=': {
                    self.consume();
                    return Token(TokenKind::eCompareEqual, Location(start, 2));
                }
                default:;
            }
            return Token(TokenKind::eEqual, cur_loc);
        }
        case '?': {
            self.consume();
            return Token(TokenKind::eQuestion, cur_loc);
        }
        case '[': {
            self.consume();
            return Token(TokenKind::eSquareLeft, cur_loc);
        }
        case ']': {
            self.consume();
            return Token(TokenKind::eSquareRight, cur_loc);
        }
        case '{': {
            self.consume();
            return Token(TokenKind::eBraceLeft, cur_loc);
        }
        case '}': {
            self.consume();
            return Token(TokenKind::eBraceRight, cur_loc);
        }
        case '&': {
            self.consume();
            switch (self.peek()) {
                case '&': {
                    self.consume();
                    return Token(TokenKind::eLogicalAnd, Location(start, 2));
                } break;
                default:;
            }
            return Token(TokenKind::eBitAnd, cur_loc);
        }
        case '|': {
            self.consume();
            switch (self.peek()) {
                case '|': {
                    self.consume();
                    return Token(TokenKind::eLogicalOr, Location(start, 2));
                } break;
                default:;
            }
            return Token(TokenKind::eBitOr, cur_loc);
        }
        case '^': {
            self.consume();
            return Token(TokenKind::eBitXor, cur_loc);
        }
        case '~': {
            self.consume();
            return Token(TokenKind::eBitNot, cur_loc);
        }
        case '@': {
            self.consume();
            return Token(TokenKind::eAt, cur_loc);
        }
        default: {
            auto c = self.peek();

            if (is_digit(c)) {
                return self.read_number();
            } else if (is_alpha(c) || c == '_') {
                return self.read_name();
            } else {
                self.consume();
                // Error, unknown token
                throw LexerUnexpectedTokenError(cur_loc, c);
            }
        }
    }
}

auto Lexer::next(this Lexer &self, bool skip_comments) -> const Token & {
    do {
        self.read_whitespaces();

        self.current = self.next_token();
    } while (skip_comments && self.current.is_comment());

    return self.current;
}

} // namespace demir
