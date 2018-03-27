package expr

type tokenStack struct {
	stack []token
}

func (s *tokenStack) reset() {
	s.stack = s.stack[:0]
}

func (s *tokenStack) isEmpty() bool {
	return len(s.stack) == 0
}

func (s *tokenStack) peek() *token {
	return &s.stack[len(s.stack)-1]
}

func (s *tokenStack) push(t token) {
	s.stack = append(s.stack, t)
}

func (s *tokenStack) pop() token {
	top := len(s.stack) - 1
	t := s.stack[top]
	s.stack = s.stack[:top]
	return t
}
