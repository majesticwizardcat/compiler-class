#!/usr/bin/env python3
#Karantias Konstantinos 2454 cse32454 Goulioumis Ioannis 2232 cse32232
import argparse
import os
import re
import subprocess
import sys
from collections import defaultdict, namedtuple
from pprint import pformat

INVALID_TOKENS = [
    (
        'ccomment', re.compile(r'\A\*\/')
    ),  # should normally be consumed via IGNORED_TOKENS's comment, otherwise there was no comment open
    ('unshut_comment', re.compile(r'\A/\*((?!\*/).)*\Z', re.DOTALL)),
]

IGNORED_TOKENS = [
    ('whitespace', re.compile(r'\A\s+')),
    ('comment', re.compile(r'\A/\*((?!\*/).)*\*/', re.DOTALL)),
    ('inline_comment', re.compile(r'\A//.*')),
]

VALID_TOKENS = [
    # keywords
    ('program', re.compile(r'\A\bprogram\b')),
    ('endprogram', re.compile(r'\A\bendprogram\b')),
    ('declare', re.compile(r'\A\bdeclare\b')),
    ('enddeclare', re.compile(r'\A\benddeclare\b')),
    ('if', re.compile(r'\A\bif\b')),
    ('then', re.compile(r'\A\bthen\b')),
    ('else', re.compile(r'\A\belse\b')),
    ('endif', re.compile(r'\A\bendif\b')),
    ('while', re.compile(r'\A\bwhile\b')),
    ('endwhile', re.compile(r'\A\bendwhile\b')),
    ('repeat', re.compile(r'\A\brepeat\b')),
    ('endrepeat', re.compile(r'\A\bendrepeat\b')),
    ('exit', re.compile(r'\A\bexit\b')),
    ('switch', re.compile(r'\A\bswitch\b')),
    ('case', re.compile(r'\A\bcase\b')),
    ('endswitch', re.compile(r'\A\bendswitch\b')),
    ('forcase', re.compile(r'\A\bforcase\b')),
    ('when', re.compile(r'\A\bwhen\b')),
    ('endforcase', re.compile(r'\A\bendforcase\b')),
    ('procedure', re.compile(r'\A\bprocedure\b')),
    ('endprocedure', re.compile(r'\A\bendprocedure\b')),
    ('function', re.compile(r'\A\bfunction\b')),
    ('endfunction', re.compile(r'\A\bendfunction\b')),
    ('call', re.compile(r'\A\bcall\b')),
    ('return', re.compile(r'\A\breturn\b')),
    ('in', re.compile(r'\A\bin\b')),
    ('inout', re.compile(r'\A\binout\b')),
    ('and', re.compile(r'\A\band\b')),
    ('or', re.compile(r'\A\bor\b')),
    ('not', re.compile(r'\A\bnot\b')),
    ('true', re.compile(r'\A\btrue\b')),
    ('false', re.compile(r'\A\bfalse\b')),
    ('input', re.compile(r'\A\binput\b')),
    ('print', re.compile(r'\A\bprint\b')),
    ('assign', re.compile(r'\A:=')),
    ('semicolon', re.compile(r'\A;')),
    ('colon', re.compile(r'\A:')),
    ('comma', re.compile(r'\A,')),
    ('id', re.compile(r'\A([a-zA-Z][a-zA-Z0-9]{0,29})[a-zA-Z0-9]*')),
    ('int', re.compile(r'\A\b\d+\b')),
    ('plus', re.compile(r'\A\+')),
    ('minus', re.compile(r'\A-')),
    ('div', re.compile(r'\A/')),
    ('mul', re.compile(r'\A\*')),
    ('oparen', re.compile(r'\A\(')),
    ('cparen', re.compile(r'\A\)')),
    ('obracket', re.compile(r'\A\[')),
    ('cbracket', re.compile(r'\A\]')),
    ('neq', re.compile(r'\A<>')),
    ('ge', re.compile(r'\A>=')),
    ('le', re.compile(r'\A<=')),
    ('gt', re.compile(r'\A>')),
    ('lt', re.compile(r'\A<')),
    ('eq', re.compile(r'\A=')),
]

EXTRA_VALIDATORS = defaultdict(lambda: lambda x: True)
EXTRA_VALIDATORS['int'] = lambda x: -32767 <= int(x) <= 32767

Token = namedtuple('Token', ['type', 'value', 'pos'])


class CompilationError(Exception):
    def __init__(self, pos, msg, suggestion=None):
        self.pos = pos
        self.msg = msg
        self.suggestion = suggestion

    def __str__(self):
        const_part = '(%d,%d):\n\t%s' % (self.pos.row, self.pos.col, self.msg)
        suggestion_part = '\n\t-> %s' % self.suggestion if self.suggestion else ''
        return const_part + suggestion_part


class LexerError(CompilationError):
    pass


class InvalidTokenError(LexerError):
    def get_msg(self, token_type):
        return {
            'unshut_comment': 'A comment starts here that never ends.',
            'ccomment': 'A comment was ended here but it never started.',
        }.get(token_type, 'Invalid token: %s' % token_type)

    def __init__(self, pos, token_type=None):
        super().__init__(pos, msg=self.get_msg(token_type))


class SyntaxAnalyzerError(CompilationError):
    def get_suggestion(self, got_token_type):
        return {
            'id': 'Maybe a ";" or a "," is missing.'
        }.get(got_token_type, None)

    def __init__(self, pos, expected_type, got_token):
        super().__init__(
            pos,
            msg='Expected token of type `%s`, got `%s` (`%s`).' %
            (expected_type, got_token.type, got_token.value),
            suggestion=self.get_suggestion(got_token.type))


class Lexer:
    def advance_from_match(self, match):
        right_of_match = match.span()[1]
        self.cursor.advance(right_of_match)

    def __init__(self, source):
        self.cursor = StringCursor(source)

    def source(self):
        return self.cursor.rest()

    def tokenize(self):
        tokens = []
        while len(self.source()) > 0:
            #print('source to lex: "%s"' % self.source())
            for name, regex in INVALID_TOKENS:
                match = regex.search(self.source())
                if match:
                    raise InvalidTokenError(
                        pos=self.cursor.position(), token_type=name)

            for name, regex in IGNORED_TOKENS:
                match = regex.search(self.source())
                if match:
                    #print('Ignored token: ', name)
                    #print('Ignored token match: ', match.group())
                    self.advance_from_match(match)
                    break
            else:
                found_token = False

                for name, regex in VALID_TOKENS:
                    match = regex.search(self.source())
                    if match and EXTRA_VALIDATORS[name](match.group()):
                        found_token = True
                        #print('Matched token: ', name)
                        #print('Match: ', match.span())

                        value = match.group() if len(
                            match.groups()) == 0 else match.group(1)
                        token = Token(
                            type=name, value=value, pos=self.cursor.position())
                        tokens.append(token)
                        self.advance_from_match(match)
                        break

                if not found_token:
                    raise InvalidTokenError(
                        pos=self.cursor.position(),
                        token_type=self.source().split()[0])
            #print('source after lex: "%s"' % self.source())
        return tokens


class StringCursor:
    def __init__(self, string):
        self.string = string
        self.char_pos = 0  # in chars
        self.pos = CursorPosition(1, 1)

    def advance(self, nchars):
        newline = re.compile(r'\r\n|\r|\n')
        new_char_pos = self.char_pos + nchars

        if new_char_pos > len(self.string):
            raise ValueError(
                'Tried to move past the end of the string `%s`.' % self.rest())

        linebreaks = list(
            newline.finditer(
                self.string, pos=self.char_pos, endpos=new_char_pos))
        #print('found linebreaks: ', linebreaks)

        inbetween_rows = len(linebreaks)
        row = self.pos.row
        col = self.pos.col
        if inbetween_rows > 0:
            pos_of_current_line_start = linebreaks[-1].span()[1]
            row += inbetween_rows
            col = new_char_pos - pos_of_current_line_start + 1
        else:
            col += nchars

        self.char_pos = new_char_pos
        self.pos = CursorPosition(row, col)

    def rest(self):
        return self.string[self.char_pos:]

    def position(self):
        return self.pos


CursorPosition = namedtuple('CursorPosition', ['row', 'col'])

Quad = namedtuple('quad', ['id', 'op', 'term0', 'term1', 'target'])


class QuadGenerator:
    def __init__(self):
        self.quad_id = 0
        self.temp_id = 0
        self.quad_list = []

    def nextquad(self):
        return self.quad_id

    def genquad(self, op, term0, term1, target):
        self.quad_list.append(
            Quad(
                id=self.quad_id,
                op=op,
                term0=term0,
                term1=term1,
                target=target))
        self.quad_id += 1

    def newtemp(self):
        temp = 'T_%d' % self.temp_id
        self.temp_id += 1
        return temp

    def backpatch(self, lst, target):
        for l in lst:
            quad = self.quad_list[l]
            self.quad_list[l] = Quad(
                id=quad.id,
                op=quad.op,
                term0=quad.term0,
                term1=quad.term1,
                target=target)

    def __str__(self):
        return '\n'.join(
            '%s: (%s, %s, %s, %s)' % (quad.id, quad.op, quad.term0, quad.term1,
                                      quad.target) for quad in self.quad_list)


class TrueFalse:
    def __init__(self, true=[], false=[]):
        self.true = true
        self.false = false


class Serializable:
    def __repr__(self):
        return pformat(self.__dict__)


class Comparable:
    def __eq__(self, other):
        return self.__dict__ == other.__dict__


class Entity(Serializable, Comparable):
    def __init__(self, name):
        self.name = name


class VariableEntity(Entity):
    def __init__(self, name, offset=None):
        super().__init__(name)
        self.offset = offset


class FunctionEntity(Entity):
    def __init__(self,
                 name,
                 start_quad,
                 arguments=None,
                 frame_length=None,
                 type='function'):
        super().__init__(name)
        self.start_quad = start_quad
        self.arguments = arguments if arguments is not None else []
        self.frame_length = frame_length
        self.type = type


class ConstantEntity(Entity):
    def __init__(self, name, value):
        super().__init__(name)
        self.value = value


class ParameterEntity(Entity):
    def __init__(self, name, mode, offset):
        super().__init__(name)
        self.mode = mode
        self.offset = offset


class TempVariableEntity(Entity):
    def __init__(self, name, offset=None):
        super().__init__(name)
        self.offset = offset


class Scope(Serializable, Comparable):
    def __init__(self, nesting_level, entities=None):
        self.nesting_level = nesting_level
        self.entities = [] if entities is None else entities


class Argument(Serializable, Comparable):
    def __init__(self, name, mode):
        self.name = name
        self.mode = mode


class SymbolTable:
    def __init__(self):
        self.scopes = []

    def create_scope(self):
        params = []
        if isinstance(self.last_entity(), FunctionEntity):
            params = [
                ParameterEntity(arg.name, arg.mode, 12 + i * 4)
                for i, arg in enumerate(self.last_entity().arguments)
            ]
        self.scopes.append(Scope(len(self.scopes)))
        self.scopes[-1].entities += params

        #print('scopes now', self.scopes)
        #print('entities now', self.scopes[-1].entities)

    def destroy_scope(self):
        last_scope = self.scopes.pop()
        if len(self.scopes) > 0:
            if not isinstance(self.last_entity(), FunctionEntity):
                raise Exception
            self.last_entity().frame_length = 12 + len(last_scope.entities) * 4

    def add_entity(self, entity):
        if hasattr(entity, 'offset'):
            entity.offset = self.last_entity().offset + 4 \
            if self.last_entity() is not None else 12

        self.scopes[-1].entities.append(entity)
        #print('add_entity(): entities now', self.scopes[-1].entities)

    def add_argument(self, arg):
        head = self.last_entity()
        if not isinstance(head, FunctionEntity):
            raise ValueError
        head.arguments.append(arg)

    def last_entity(self):
        try:
            return self.scopes[-1].entities[-1]
        except:
            return None

    def am_i_inside_function(self):
        if len(self.scopes) > 1:
            parent_last = self.scopes[-2].entities[-1]
            return isinstance(
                parent_last, FunctionEntity) and parent_last.type == 'function'
        return False

    def lookup(self, name, scopes=None):
        scopes = self.scopes if scopes is None else scopes
        for scope in self.scopes[::-1]:
            for entity in scope.entities[::-1]:
                try:
                    if entity.name == name:
                        return namedtuple('LookupResult',
                                          ['entity', 'nesting_level'])(
                                              entity, scope.nesting_level)
                except AttributeError:
                    pass
        return None

    def lookup_on_current_scope(self, name):
        return self.lookup(name, scopes=[self.scopes[-1]])


class SyntaxAnal:
    def __init__(self, tokens):
        self.tokens = tokens
        self.quad_gen = QuadGenerator()
        self.exits = []
        self.table = SymbolTable()
        self.last_pos = None
        self.seen_return = False
        self.inside_repeat = 0

    def ensure_we_do_not_redeclare(self, name):
        if self.table.lookup_on_current_scope(name) is not None:
            raise CompilationError(
                pos=self.last_pos, msg='Redeclaring %s is not allowed.' % name)

    def check_syntax(self):
        self.parse_program()
        if len(self.tokens) > 0:
            raise Exception('Unexpected token after endprogram.')

    def peek_type(self):
        return self.tokens[0].type

    def consume(self, type):
        #print('Trying to cunsume %s' % type)
        if self.peek(type):
            #print('consumed %s ' % type)
            tk = self.tokens.pop(0)
            self.last_pos = tk.pos
            return tk
        else:
            raise SyntaxAnalyzerError(
                pos=self.tokens[0].pos,
                expected_type=type,
                got_token=self.tokens[0])

    def peek(self, type):
        if len(self.tokens) == 0:
            raise Exception('Unexpected EOF. Maybe endprogram is missing.')
        return self.peek_type() == type

    def parse_program(self):
        self.consume('program')
        name = self.consume('id').value
        self.quad_gen.genquad('begin_block', name, '_', '_')
        self.parse_block()
        # TODO: halt only on the main program (how to decide which one is main?)
        self.quad_gen.genquad('halt', '_', '_', '_')
        self.quad_gen.genquad('end_block', name, '_', '_')
        self.consume('endprogram')

    def parse_block(self):
        self.table.create_scope()
        self.parse_declarations()
        self.parse_subprograms()
        self.parse_statements()
        self.table.destroy_scope()

        if isinstance(self.table.last_entity(), FunctionEntity):
            if self.table.last_entity(
            ).type == 'function' and not self.seen_return:
                raise CompilationError(
                    pos=self.last_pos,
                    msg='End of function block and no return found.',
                    suggestion='Did you forget to return?')

    def parse_declarations(self):
        if self.peek('declare'):
            self.consume('declare')
            self.parse_varlist()
            self.consume('enddeclare')

    def parse_varlist(self):
        if self.peek('id'):
            vid = self.consume('id').value
            self.ensure_we_do_not_redeclare(vid)
            self.quad_gen.genquad('int', vid, '_', '_')
            self.table.add_entity(VariableEntity(vid))

            while self.peek('comma'):
                self.consume('comma')
                vid = self.consume('id').value
                self.ensure_we_do_not_redeclare(vid)
                self.quad_gen.genquad('int', vid, '_', '_')
                self.table.add_entity(VariableEntity(vid))

    def parse_subprograms(self):
        while self.peek('procedure') or self.peek('function'):
            self.parse_procorfunc()

    def parse_procorfunc(self):
        if self.peek('procedure'):
            self.consume('procedure')
            name = self.consume('id').value
            self.ensure_we_do_not_redeclare(name)
            self.table.add_entity(
                FunctionEntity(
                    name, self.quad_gen.nextquad(), type='procedure'))
            self.quad_gen.genquad('begin_block', name, '_', '_')
            self.parse_procorfuncbody()
            self.quad_gen.genquad('end_block', name, '_', '_')
            self.consume('endprocedure')
        else:
            self.consume('function')
            name = self.consume('id').value
            self.ensure_we_do_not_redeclare(name)
            self.seen_return = False
            self.table.add_entity(
                FunctionEntity(name, self.quad_gen.nextquad()))
            self.quad_gen.genquad('begin_block', name, '_', '_')
            self.parse_procorfuncbody()
            self.quad_gen.genquad('end_block', name, '_', '_')
            self.consume('endfunction')

    def parse_procorfuncbody(self):
        self.parse_formalpars()
        self.parse_block()

    def parse_formalpars(self):
        self.consume('oparen')
        self.parse_formalparlist()
        self.consume('cparen')

    def parse_formalparlist(self):
        if self.peek('in') or self.peek('inout'):
            self.parse_formalparitem()

            while self.peek('comma'):
                self.consume('comma')
                self.parse_formalparitem()

    def parse_formalparitem(self):
        mode = None
        if self.peek('in'):
            self.consume('in')
            mode = 'cv'
        else:
            self.consume('inout')
            mode = 'ref'

        name = self.consume('id').value
        self.table.add_argument(Argument(name, mode))

    def parse_statements(self):
        self.parse_statement()

        while self.peek('semicolon'):
            self.consume('semicolon')
            self.parse_statement()

    def parse_statement(self):
        if self.peek('id'):
            self.parse_assignmentstat()
        elif self.peek('if'):
            self.parse_ifstat()
        elif self.peek('while'):
            self.parse_whilestat()
        elif self.peek('repeat'):
            self.parse_repeatstat()
        elif self.peek('exit'):
            self.parse_exitstat()
        elif self.peek('switch'):
            self.parse_switchstat()
        elif self.peek('forcase'):
            self.parse_forcasestat()
        elif self.peek('call'):
            self.parse_callstat()
        elif self.peek('return'):
            self.parse_returnstat()
        elif self.peek('input'):
            self.parse_inputstat()
        elif self.peek('print'):
            self.parse_printstat()

    def parse_assignmentstat(self):
        target = self.consume('id').value
        op = self.consume('assign').value
        value = self.parse_expression()
        self.quad_gen.genquad(op, value, '_', target)

    def parse_ifstat(self):
        self.consume('if')
        cond = self.parse_condition()
        self.consume('then')
        self.quad_gen.backpatch(cond.true, self.quad_gen.nextquad())
        self.parse_statements()
        after_if_jump = self.quad_gen.nextquad()
        self.quad_gen.genquad('jump', '_', '_', '_')

        self.quad_gen.backpatch(cond.false, self.quad_gen.nextquad())
        self.parse_elsepart()
        self.consume('endif')
        self.quad_gen.backpatch([after_if_jump], self.quad_gen.nextquad())

    def parse_elsepart(self):
        if self.peek('else'):
            self.consume('else')
            self.parse_statements()

    def parse_repeatstat(self):
        self.consume('repeat')
        in_repeat = self.quad_gen.nextquad()

        self.inside_repeat += 1

        old_exits = self.exits
        self.exits = []
        self.parse_statements()
        self.quad_gen.genquad('jump', '_', '_', in_repeat)

        self.quad_gen.backpatch(self.exits, self.quad_gen.nextquad())
        self.exits = old_exits

        self.consume('endrepeat')
        self.inside_repeat -= 1

    def parse_exitstat(self):
        self.consume('exit')
        if self.inside_repeat < 1:
            raise CompilationError(
                pos=self.last_pos,
                msg='Found exit outside a repeat block.',
                suggestion='Remove the exit?')
        self.exits.append(self.quad_gen.nextquad())
        self.quad_gen.genquad('jump', '_', '_', '_')

    def parse_whilestat(self):
        self.consume('while')
        pre_cond = self.quad_gen.nextquad()
        cond = self.parse_condition()
        in_while = self.quad_gen.nextquad()
        self.quad_gen.backpatch(cond.true, in_while)
        self.parse_statements()
        self.quad_gen.genquad('jump', '_', '_', pre_cond)

        after_while = self.quad_gen.nextquad()
        self.quad_gen.backpatch(cond.false, after_while)
        self.consume('endwhile')

    def parse_switchstat(self):
        self.consume('switch')
        expr = self.parse_expression()

        jumps_when_done = self.parse_case(expr)
        while self.peek('case'):
            jumps_when_done += self.parse_case(expr)

        self.consume('endswitch')
        self.quad_gen.backpatch(jumps_when_done, self.quad_gen.nextquad())

    def parse_case(self, expr1):
        self.consume('case')
        expr2 = self.parse_expression()

        neq_quad = self.quad_gen.nextquad()
        self.quad_gen.genquad('<>', expr1, expr2, '_')

        self.consume('colon')
        self.parse_statements()
        jump_when_done = self.quad_gen.nextquad()
        self.quad_gen.genquad('jump', '_', '_', '_')
        self.quad_gen.backpatch([neq_quad], self.quad_gen.nextquad())
        return [jump_when_done]

    def parse_forcasestat(self):
        self.consume('forcase')
        in_forcase = self.quad_gen.nextquad()
        was_any_true_flag = self.quad_gen.newtemp()
        self.quad_gen.genquad(':=', '0', '_', was_any_true_flag)

        self.parse_when(was_any_true_flag)

        while self.peek('when'):
            self.parse_when(was_any_true_flag)

        self.quad_gen.genquad('=', '1', was_any_true_flag, in_forcase)
        self.consume('endforcase')

    def parse_when(self, was_any_true_flag):
        self.consume('when')
        cond = self.parse_condition()

        self.consume('colon')

        when_ok = self.quad_gen.nextquad()
        self.quad_gen.backpatch(cond.true, when_ok)
        self.quad_gen.genquad(':=', '1', '_', was_any_true_flag)
        self.parse_statements()

        next_when = self.quad_gen.nextquad()
        self.quad_gen.backpatch(cond.false, next_when)

    def parse_callstat(self):
        self.consume('call')
        name = self.consume('id').value
        self.ensure_a_valid_procedure(name)
        self.parse_actualpars()
        self.quad_gen.genquad('call', name, '_', '_')

    def ensure_a_valid_procedure(self, name):
        self.ensure_a_valid(
            FunctionEntity,
            name,
            extracheck=lambda x: x.type == 'procedure',
            typestr='procedure')

    def parse_returnstat(self):
        self.consume('return')
        self.seen_return = True
        exp = self.parse_expression()
        self.quad_gen.genquad('retv', exp, '_', '_')

        if not self.table.am_i_inside_function():
            raise CompilationError(
                pos=self.last_pos,
                msg='Found stray return outside function block.',
                suggestion='Remove the stray return.')

    def parse_printstat(self):
        self.consume('print')
        expr = self.parse_expression()
        self.quad_gen.genquad('out', expr, '_', '_')

    def parse_inputstat(self):
        self.consume('input')
        name = self.consume('id').value
        self.quad_gen.genquad('inp', name, '_', '_')

    def parse_actualpars(self):
        self.consume('oparen')
        self.parse_actualparlist()
        self.consume('cparen')

    def parse_actualparlist(self):
        if self.peek('in') or self.peek('inout'):
            self.parse_actualparitem()

            while self.peek('comma'):
                self.consume('comma')
                self.parse_actualparitem()

    def parse_actualparitem(self):
        if self.peek('in'):
            self.consume('in')
            par = self.parse_expression()
            self.quad_gen.genquad('par', par, 'cv', '_')
        else:
            self.consume('inout')
            par = self.consume('id').value
            self.quad_gen.genquad('par', par, 'ref', '_')

    def parse_condition(self):
        b = TrueFalse()
        q1 = self.parse_boolterm()
        b.true = q1.true
        b.false = q1.false

        while self.peek('or'):
            self.consume('or')
            self.quad_gen.backpatch(b.false, self.quad_gen.nextquad())

            q2 = self.parse_boolterm()
            b.true += q2.true
            b.false = q2.false

        return b

    def parse_boolterm(self):
        q = TrueFalse()
        r1 = self.parse_boolfactor()

        q.true = r1.true
        q.false = r1.false

        while self.peek('and'):
            self.consume('and')
            self.quad_gen.backpatch(q.true, self.quad_gen.nextquad())

            r2 = self.parse_boolfactor()
            q.false += r2.false
            q.true = r2.true

        return q

    def parse_boolfactor(self):
        if self.peek('not'):
            self.consume('not')
            self.consume('obracket')
            cond = self.parse_condition()
            self.consume('cbracket')
            return TrueFalse(true=cond.false, false=cond.true)
        elif self.peek('obracket'):
            self.consume('obracket')
            cond = self.parse_condition()
            self.consume('cbracket')
            return cond
        elif self.peek('true'):
            self.consume('true')
            qid = self.quad_gen.nextquad()
            self.quad_gen.genquad('jump', '_', '_', '_')
            return TrueFalse(true=[qid])
        elif self.peek('false'):
            self.consume('false')
            qid = self.quad_gen.nextquad()
            self.quad_gen.genquad('jump', '_', '_', '_')
            return TrueFalse(false=[qid])
        else:
            exp1 = self.parse_expression()
            relop = self.parse_relationaloper()
            exp2 = self.parse_expression()

            relop_qid = self.quad_gen.nextquad()
            self.quad_gen.genquad(relop, exp1, exp2, '_')

            nop_qid = self.quad_gen.nextquad()
            self.quad_gen.genquad('jump', '_', '_', '_')
            return TrueFalse(true=[relop_qid], false=[nop_qid])

    def parse_expression(self):
        sign = self.parse_optionalsign()
        term = sign + self.parse_term()

        while self.peek('plus') or self.peek('minus'):
            op = self.parse_addoper()
            secterm = self.parse_term()
            target = self.quad_gen.newtemp()
            self.quad_gen.genquad(op, term, secterm, target)
            term = target

        return term

    def parse_term(self):
        factor = self.parse_factor()

        while self.peek('mul') or self.peek('div'):
            op = self.parse_muloper()
            secfactor = self.parse_factor()
            target = self.quad_gen.newtemp()
            self.quad_gen.genquad(op, factor, secfactor, target)
            factor = target

        return factor

    def ensure_a_valid(self,
                       typ,
                       name,
                       extracheck=lambda x: True,
                       typestr=None):
        lookup_result = self.table.lookup(name)
        if lookup_result is None or not (isinstance(lookup_result.entity, typ)
                                         and extracheck(lookup_result.entity)):
            typestr = typestr if typestr is not None else typ.__name__
            raise CompilationError(
                pos=self.last_pos,
                msg='Using "%s" as a %s but it\'s not one.' % (name, typestr))

    def parse_factor(self):
        if self.peek('oparen'):
            self.consume('oparen')
            exp = self.parse_expression()
            self.consume('cparen')
            return exp

        elif self.peek('id'):
            vid = self.consume('id').value
            place_of_fn_call = self.parse_idtail()
            if place_of_fn_call is not None:
                self.ensure_a_valid_function(vid)
                self.quad_gen.genquad('call', vid, '_', '_')
                return place_of_fn_call
            else:
                self.ensure_a_valid_variable(vid)
                return vid

        else:
            return self.consume('int').value

    def ensure_a_valid_function(self, name):
        self.ensure_a_valid(
            FunctionEntity,
            name,
            extracheck=lambda x: x.type == 'function',
            typestr='function')

    def ensure_a_valid_variable(self, name):
        self.ensure_a_valid(VariableEntity, name, typestr='variable')

    def parse_idtail(self):
        if self.peek('oparen'):
            self.parse_actualpars()
            retval = self.quad_gen.newtemp()
            self.quad_gen.genquad('par', retval, 'ret', '_')
            return retval

        return None

    def parse_relationaloper(self):
        if self.peek('eq'):
            return self.consume('eq').value
        elif self.peek('le'):
            return self.consume('le').value
        elif self.peek('ge'):
            return self.consume('ge').value
        elif self.peek('lt'):
            return self.consume('lt').value
        elif self.peek('gt'):
            return self.consume('gt').value
        else:
            return self.consume('neq').value

    def parse_addoper(self):
        if self.peek('plus'):
            return self.consume('plus').value
        else:
            return self.consume('minus').value

    def parse_muloper(self):
        if self.peek('mul'):
            return self.consume('mul').value
        else:
            return self.consume('div').value

    def parse_optionalsign(self):
        if self.peek('plus') or self.peek('minus'):
            return self.parse_addoper()

        return ''


class CBackend:
    def __init__(self, quadgen):
        self.quadlist = quadgen.quad_list

    def convert(self):
        return '\n'.join(self.prelude() + self.identifiers() + self.code() +
                         self.postlude())

    def prelude(self):
        return ['#include <stdio.h>', 'int main() {']

    def postlude(self):
        return ['}']

    def code(self):
        return [
            '\tL_%d: %s %s' % (quad.id, self.quad_to_c(quad),
                               self.quad_to_comment(quad))
            for quad in self.quadlist
        ]

    @staticmethod
    def quad_to_c(q):
        ret = None
        op = q.op
        if op in ['+', '-', '*', '/']:
            ret = '%s = %s %s %s;' % (q.target, q.term0, op, q.term1)
        elif op in ['begin_block', 'end_block', 'halt', 'int']:
            ret = '{}'
        elif op == ':=':
            ret = '%s = %s;' % (q.target, q.term0)
        elif op in ['>=', '<=', '<', '>', '=', '<>']:
            c_op = op
            if op == '=':
                c_op = '=='
            elif op == '<>':
                c_op = '!='
            ret = 'if (%s %s %s) goto L_%s;' % (q.term0, c_op, q.term1,
                                                q.target)
        elif op == 'jump':
            ret = 'goto L_%s;' % q.target
        elif op == 'out':
            ret = 'printf("%%d\\n", %s);' % q.term0
        elif op == 'inp':
            ret = 'scanf("%%d", &%s);' % q.term0
        else:
            print('Unknown quad type "%s", can\'t translate to C.' % op)
            exit(0)  # C translation errors are OK

        return ret

    @staticmethod
    def quad_to_comment(quad):
        return '// (%s, %s, %s, %s)' % (quad.op, quad.term0, quad.term1,
                                        quad.target)

    def identifiers(self):
        temps = set(
            quad.target for quad in self.quadlist
            if str(quad.target).startswith('T_'))
        declared_variables = set(
            quad.term0 for quad in self.quadlist if quad.op == 'int')
        return ['\tint %s;' % ', '.join(temps | declared_variables)]


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('source_file')
    args = parser.parse_args()

    basename = os.path.basename(args.source_file)
    sourcename = basename.split('.')[0]
    intermediate_filename = '%s.eeli' % sourcename
    c_filename = '%s.c' % sourcename

    with open(args.source_file, 'r') as source_file:
        source = source_file.read()
    try:
        tokens = Lexer(source).tokenize()
        syntax_anal = SyntaxAnal(tokens)
        syntax_anal.check_syntax()
        cbackend = CBackend(syntax_anal.quad_gen)
        print('Putting intermediate code in [%s]...' % intermediate_filename)
        with open(intermediate_filename, 'w') as intermediate_file:
            intermediate_file.write(str(syntax_anal.quad_gen))
        print('Putting C code in [%s]...' % intermediate_filename)
        with open(c_filename, 'w') as c_file:
            c_file.write(cbackend.convert())
        #print('Compiling C code [%s] to [%s]...' % (c_filename, sourcename))
        #subprocess.call(['cc', '-o', sourcename, c_filename])
    except CompilationError as e:
        print('%s:%s\n' % (args.source_file, str(e)))
        sys.exit(1)
