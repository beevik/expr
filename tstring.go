package expr

type tstring string

func (t tstring) consume(n int) tstring {
	return t[n:]
}

func (t tstring) consumeWhitespace() tstring {
	return t.consume(t.scanWhile(whitespace))
}

func (t tstring) scanWhile(fn func(c byte) bool) int {
	i := 0
	for ; i < len(t) && fn(t[i]); i++ {
	}
	return i
}

func (t tstring) consumeWhile(fn func(c byte) bool) (consumed, remain tstring) {
	i := t.scanWhile(fn)
	return t[:i], t[i:]
}

func whitespace(c byte) bool {
	return c == ' ' || c == '\t'
}

func decimal(c byte) bool {
	return (c >= '0' && c <= '9')
}

func hexadecimal(c byte) bool {
	return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')
}

func binary(c byte) bool {
	return c == '0' || c == '1'
}

func identifier(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' || c == '.'
}
