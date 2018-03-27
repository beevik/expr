package expr

import (
	"errors"
	"fmt"
	"strconv"
)

// Errors returned by the expression evaluator.
var (
	ErrParse = errors.New("expression syntax error")
)

// A Resolver evaluates identifiers that appear within an expression.
type Resolver interface {
	// ResolveIdentifer evaluates the identifier and returns its value.
	ResolveIdentifier(identifier string) (int64, error)
}

// Eval evaluates an expression string, returning the fully evaluated
// value if possible.
func Eval(expr string) (int64, error) {
	p := NewParser()
	return p.Eval(expr, &nullResolver{})
}

type nullResolver struct{}

func (r *nullResolver) ResolveIdentifier(identifier string) (int64, error) {
	return 0, fmt.Errorf("unable to resolve identifier '%s'", identifier)
}

type tokenType byte

const (
	tokenNil tokenType = iota
	tokenIdentifier
	tokenInteger
	tokenOp
	tokenLParen
	tokenRParen
)

// A token is a atomic piece of an expression.
type token struct {
	Type  tokenType
	Value interface{} // nil, string, int64 or *op (depends on Type)
}

type opType byte

const (
	opNil opType = 0 + iota
	opMultiply
	opDivide
	opModulo
	opAdd
	opSubtract
	opShiftLeft
	opShiftRight
	opBitwiseAnd
	opBitwiseXor
	opBitwiseOr
	opBitwiseNot
	opUnaryMinus
	opUnaryPlus
)

type associativity byte

const (
	left associativity = iota
	right
)

type op struct {
	Symbol     string
	Type       opType
	Precedence byte
	Assoc      associativity
	Args       byte
	UnaryOp    opType
	Eval       func(a, b int64) int64
}

var ops = []op{
	{"", opNil, 0, right, 0, opNil, nil},
	{"*", opMultiply, 6, right, 2, opNil, func(a, b int64) int64 { return a * b }},
	{"/", opDivide, 6, right, 2, opNil, func(a, b int64) int64 { return a / b }},
	{"%", opModulo, 6, right, 2, opNil, func(a, b int64) int64 { return a % b }},
	{"+", opAdd, 5, right, 2, opUnaryPlus, func(a, b int64) int64 { return a + b }},
	{"-", opSubtract, 5, right, 2, opUnaryMinus, func(a, b int64) int64 { return a - b }},
	{"<<", opShiftLeft, 4, right, 2, opNil, func(a, b int64) int64 { return a << uint32(b) }},
	{">>", opShiftRight, 4, right, 2, opNil, func(a, b int64) int64 { return a >> uint32(b) }},
	{"&", opBitwiseAnd, 3, right, 2, opNil, func(a, b int64) int64 { return a & b }},
	{"^", opBitwiseXor, 2, right, 2, opNil, func(a, b int64) int64 { return a ^ b }},
	{"|", opBitwiseOr, 1, right, 2, opNil, func(a, b int64) int64 { return a | b }},
	{"~", opBitwiseNot, 7, left, 1, opNil, func(a, b int64) int64 { return ^a }},
	{"-", opUnaryMinus, 7, left, 1, opNil, func(a, b int64) int64 { return -a }},
	{"+", opUnaryPlus, 7, left, 1, opNil, func(a, b int64) int64 { return a }},
}

// lexeme identifiers
const (
	lNil byte = iota
	lNum
	lIde
	lLPa
	lRPa
	lMul
	lDiv
	lMod
	lAdd
	lSub
	lShl
	lShr
	lAnd
	lXor
	lOra
	lNot
)

// A table mapping lexeme identifiers to the data or parse function used to
// create expression tokens from them.
var lexeme = []struct {
	TokenType tokenType
	OpType    opType
	Parse     func(p *Parser, t tstring) (tok token, remain tstring, err error)
}{
	/*lNil*/ {},
	/*lNum*/ {Parse: (*Parser).parseNumber},
	/*lIde*/ {Parse: (*Parser).parseIdentifier},
	/*lLPa*/ {TokenType: tokenLParen},
	/*lRPa*/ {TokenType: tokenRParen},
	/*lMul*/ {TokenType: tokenOp, OpType: opMultiply},
	/*lDiv*/ {TokenType: tokenOp, OpType: opDivide},
	/*lMod*/ {TokenType: tokenOp, OpType: opModulo},
	/*lAdd*/ {TokenType: tokenOp, OpType: opAdd},
	/*lSub*/ {TokenType: tokenOp, OpType: opSubtract},
	/*lShl*/ {Parse: (*Parser).parseShiftOp},
	/*lShr*/ {Parse: (*Parser).parseShiftOp},
	/*lAnd*/ {TokenType: tokenOp, OpType: opBitwiseAnd},
	/*lXor*/ {TokenType: tokenOp, OpType: opBitwiseXor},
	/*lOra*/ {TokenType: tokenOp, OpType: opBitwiseOr},
	/*lNot*/ {TokenType: tokenOp, OpType: opBitwiseNot},
}

// A table mapping the first character of a lexeme to a lexeme identifier.
var lex0 = [96]byte{
	lNil, lNil, lNil, lNil, lNum, lMod, lAnd, lNil, // 32..39
	lLPa, lRPa, lMul, lAdd, lNil, lSub, lIde, lDiv, // 40..47
	lNum, lNum, lNum, lNum, lNum, lNum, lNum, lNum, // 48..55
	lNum, lNum, lNil, lNil, lShl, lNil, lShr, lNil, // 56..63
	lNil, lIde, lIde, lIde, lIde, lIde, lIde, lIde, // 64..71
	lIde, lIde, lIde, lIde, lIde, lIde, lIde, lIde, // 72..79
	lIde, lIde, lIde, lIde, lIde, lIde, lIde, lIde, // 80..87
	lIde, lIde, lIde, lNil, lNil, lNil, lXor, lIde, // 88..95
	lNil, lIde, lIde, lIde, lIde, lIde, lIde, lIde, // 96..103
	lIde, lIde, lIde, lIde, lIde, lIde, lIde, lIde, // 104..111
	lIde, lIde, lIde, lIde, lIde, lIde, lIde, lIde, // 112..119
	lIde, lIde, lIde, lNil, lOra, lNil, lNot, lNil, // 120..127
}

// A Parser can be used to evaluate expressions.
type Parser struct {
	output        tokenStack
	operatorStack tokenStack
	prevTokenType tokenType
	HexMode       bool
}

// NewParser creates a new expression parser.
func NewParser() *Parser {
	return &Parser{}
}

// Eval evaluates an expression string and returns the numeric result.
func (p *Parser) Eval(expr string, r Resolver) (int64, error) {
	defer p.reset()

	t := tstring(expr)

	for {
		tok, remain, err := p.parseToken(t)
		if err != nil {
			return 0, err
		}
		if tok.Type == tokenNil {
			break
		}
		t = remain

		switch tok.Type {
		case tokenInteger:
			p.output.push(tok)

		case tokenIdentifier:
			v, err := r.ResolveIdentifier(tok.Value.(string))
			if err != nil {
				return 0, err
			}
			tok.Type, tok.Value = tokenInteger, v
			p.output.push(tok)

		case tokenLParen:
			p.operatorStack.push(tok)

		case tokenRParen:
			foundLParen := false
			for !p.operatorStack.isEmpty() {
				tmp := p.operatorStack.pop()
				if tmp.Type == tokenLParen {
					foundLParen = true
					break
				}
				p.output.push(tmp)
			}
			if !foundLParen {
				return 0, ErrParse
			}

		case tokenOp:
			if err := p.checkForUnaryOp(&tok, t); err != nil {
				return 0, err
			}
			for p.isCollapsible(&tok) {
				p.output.push(p.operatorStack.pop())
			}
			p.operatorStack.push(tok)
		}

		p.prevTokenType = tok.Type
	}

	for !p.operatorStack.isEmpty() {
		tok := p.operatorStack.pop()
		if tok.Type == tokenLParen {
			return 0, ErrParse
		}
		p.output.push(tok)
	}

	result, err := p.evalOutput()
	if err != nil {
		return 0, err
	}
	if !p.output.isEmpty() {
		return 0, ErrParse
	}

	return result.Value.(int64), nil
}

func (p *Parser) reset() {
	p.output.reset()
	p.operatorStack.reset()
	p.prevTokenType = tokenNil
}

func (p *Parser) parseToken(t tstring) (tok token, remain tstring, err error) {
	t = t.consumeWhitespace()

	// Return the nil token when there are no more tokens to parse.
	if len(t) == 0 {
		return token{}, t, nil
	}

	// Use the first character of the token string to look up lexeme
	// parser data.
	if t[0] < 32 || t[0] > 127 {
		return token{}, t, ErrParse
	}
	lex := lexeme[lex0[t[0]-32]]

	// One-character lexemes require no additional parsing to generate the
	// token. The token can be created solely from the lexeme data.
	if lex.Parse == nil {
		tok = token{lex.TokenType, nil}
		if tok.Type == tokenOp {
			tok.Value = &ops[lex.OpType]
		}
		return tok, t.consume(1), nil
	}

	// Lexemes that are more than one character in length require custom
	// parsing to generate the token.
	return lex.Parse(p, t)
}

func (p *Parser) parseNumber(t tstring) (tok token, remain tstring, err error) {
	base, fn, num := 10, decimal, t

	if p.HexMode {
		base, fn = 16, hexadecimal
	}

	switch num[0] {
	case '$':
		if len(num) < 2 {
			return token{}, t, ErrParse
		}
		base, fn, num = 16, hexadecimal, num.consume(1)

	case '0':
		if len(num) > 1 && (num[1] == 'x' || num[1] == 'b' || num[1] == 'd') {
			if len(num) < 3 {
				return token{}, t, ErrParse
			}
			switch num[1] {
			case 'x':
				base, fn = 16, hexadecimal
			case 'b':
				base, fn = 2, binary
			case 'd':
				base, fn = 10, decimal
			}
			num = num.consume(2)
		}
	}

	num, remain = num.consumeWhile(fn)
	if num == "" {
		return token{}, t, ErrParse
	}

	v, err := strconv.ParseInt(string(num), base, 64)
	if err != nil {
		return token{}, t, ErrParse
	}

	tok = token{tokenInteger, v}
	return tok, remain, nil
}

func (p *Parser) parseIdentifier(t tstring) (tok token, remain tstring, err error) {
	if p.HexMode {
		return p.parseNumber(t)
	}

	var id tstring
	id, remain = t.consumeWhile(identifier)
	tok = token{tokenIdentifier, string(id)}
	return tok, remain, nil
}

func (p *Parser) parseShiftOp(t tstring) (tok token, remain tstring, err error) {
	if len(t) < 2 || t[1] != t[0] {
		return token{}, t, ErrParse
	}

	var op *op
	switch t[0] {
	case '<':
		op = &ops[opShiftLeft]
	default:
		op = &ops[opShiftRight]
	}

	tok = token{tokenOp, op}
	return tok, t.consume(2), nil
}

func (p *Parser) evalOutput() (token, error) {
	if p.output.isEmpty() {
		return token{}, ErrParse
	}

	tok := p.output.pop()
	if tok.Type == tokenInteger {
		return tok, nil
	}
	if tok.Type != tokenOp {
		return token{}, ErrParse
	}

	op := tok.Value.(*op)
	switch op.Args {
	case 1:
		child, err := p.evalOutput()
		if err != nil {
			return token{}, err
		}
		tok.Type = tokenInteger
		tok.Value = op.Eval(child.Value.(int64), 0)
		return tok, nil

	default:
		child2, err := p.evalOutput()
		if err != nil {
			return token{}, err
		}
		child1, err := p.evalOutput()
		if err != nil {
			return token{}, err
		}

		tok.Type = tokenInteger
		tok.Value = op.Eval(child1.Value.(int64), child2.Value.(int64))
		return tok, nil
	}
}

func (p *Parser) checkForUnaryOp(tok *token, t tstring) error {
	o := tok.Value.(*op)
	if o.UnaryOp == opNil {
		return nil
	}

	// If this operation follows an operation, a left parenthesis, or nothing,
	// then convert it to a unary op.
	if p.prevTokenType == tokenOp || p.prevTokenType == tokenLParen || p.prevTokenType == tokenNil {
		tok.Value = &ops[o.UnaryOp]
	}
	return nil
}

func (p *Parser) isCollapsible(opToken *token) bool {
	if p.operatorStack.isEmpty() {
		return false
	}

	top := p.operatorStack.peek()
	if top.Type != tokenOp {
		return false
	}

	currOp := opToken.Value.(*op)
	topOp := top.Value.(*op)
	if topOp.Precedence > currOp.Precedence {
		return true
	}
	if topOp.Precedence == currOp.Precedence && topOp.Assoc == left {
		return true
	}
	return false
}
