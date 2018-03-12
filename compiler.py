#Karantias Konstantinos 2454 cse32454 Goulioumis Ioannis 2232 cse32232
from collections import namedtuple, defaultdict
from itertools import tee
import re
import pprint

pp = pprint.PrettyPrinter().pprint

INVALID_TOKENS = [
    ('ccomment', re.compile(r'\A\*\/')), # should normally be consumed via IGNORED_TOKENS's comment, otherwise there was no comment open
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
                if regex.search(self.source()):
                    pos = self.cursor.position()
                    raise ValueError('%s on (%d, %d)' % (name, pos.row, pos.col))

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

                        value = match.group() if len(match.groups()) == 0 else match.group(1)
                        token = Token(type=name, value=value, pos=self.cursor.position())
                        tokens.append(token)
                        self.advance_from_match(match)
                        break

                if not found_token:
                    raise ValueError('Couldn\'t tokenize: `%s`' % self.source())
            #print('source after lex: "%s"' % self.source())
        return tokens

class StringCursor:
    def __init__(self, string):
        self.string = string
        self.char_pos = 0 # in chars
        self.pos = CursorPosition(1, 1)

    def advance(self, nchars):
        newline = re.compile(r'\r\n|\r|\n')
        new_char_pos = self.char_pos + nchars

        if new_char_pos > len(self.string):
            raise ValueError('Tried to move past the end of the string `%s`.' % self.rest())

        linebreaks = list(newline.finditer(self.string, pos=self.char_pos, endpos=new_char_pos))
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

class SyntaxAnal:
    def __init__(self, tokens):
        self.tokens = tokens   

    def check_syntax(self):
        self.parse_program()

    def consume(self, type):
        if self.tokens[0].type == type:
            #print('consumed %s ' % type)
            return self.tokens.pop(0)
        else:
            raise ValueError('Syntax error: Expected %s but found %s' % (type, self.tokens[0].type))
            
    def peek(self, type):
        return self.tokens[0].type == type

    def parse_program(self):
        self.consume('program')
        self.consume('id')
        self.parse_block()
        self.consume('endprogram')
    
    def parse_block(self):
        self.parse_declarations()
        self.parse_subprograms()
        self.parse_statements()

    def parse_declarations(self):
        if self.peek('declare'):
            self.consume('declare')
            self.parse_varlist()
            self.consume('enddeclare')

    def parse_varlist(self):
        if self.peek('id'):
            self.consume('id')

            while self.peek('comma'):
                self.consume('comma')
                self.consume('id')
    
    def parse_subprograms(self):
        while self.peek('procedure') or self.peek('function'):
            self.parse_procorfunc()

    def parse_procorfunc(self):
        if self.peek('procedure'):
            self.consume('procedure')
            self.consume('id')
            self.parse_procorfuncbody()
            self.consume('endprocedure')
        else:
            self.consume('function')
            self.consume('id')
            self.parse_procorfuncbody()
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
        if self.peek('in'):
            self.consume('in')
        else:
            self.consume('inout')

        self.consume('id')

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
        self.consume('id')
        self.consume('assign')
        self.parse_expression();

    def parse_ifstat(self):
        self.consume('if')
        self.parse_condition()
        self.consume('then')
        self.parse_statements()
        self.parse_elsepart()
        self.consume('endif')

    def parse_elsepart(self):
        if self.peek('else'):
            self.consume('else')
            self.parse_statements()

    def parse_repeatstat(self):
        self.consume('repeat')
        self.parse_statements()
        self.consume('endrepeat')

    def parse_exitstat(self):
        self.consume('exit')

    def parse_whilestat(self):
        self.consume('while')
        self.parse_condition()
        self.parse_statements()
        self.consume('endwhile')

    def parse_switchstat(self):
        self.consume('switch')
        self.parse_expression()
        self.consume('case')
        self.parse_expression()
        self.consume('colon')
        self.parse_statements()

        while self.peek('case'):
            self.consume('case')
            self.parse_expression()
            self.consume('colon')
            self.parse_statements()
        
        self.consume('endswitch')

    def parse_forcasestat(self):
        self.consume('when')
        self.parse_condition()
        self.consume('colon')
        self.parse_statements()

        while self.peek('when'):
            self.consume('when')
            self.parse_condition()
            self.consume('colon')
            self.parse_statements()

        self.consume('endforcase')

    def parse_callstat(self):
        self.consume('call')
        self.consume('id')
        self.parse_actualpars()

    def parse_returnstat(self):
        self.consume('return')
        self.parse_expression()

    def parse_printstat(self):
        self.consume('print')
        self.parse_expression()

    def parse_inputstat(self):
        self.consume('input')
        self.consume('id')

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
            self.parse_expression()
        else:
            self.consume('inout')
            self.consume('id')

    def parse_condition(self):
        self.parse_boolterm()
        
        while self.peek('or'):
            self.consume('or')
            self.parse_boolterm()

    def parse_boolterm(self):
        self.parse_boolfactor()

        while self.peek('and'):
            self.consume('and')
            self.parse_boolfactor()

    def parse_boolfactor(self):
        if self.peek('not'):
            self.consume('not')
            self.consume('obracket')
            self.parse_condition()
            self.consume('cbracket')
        elif self.peek('obracket'):
            self.consume('obracket')
            self.parse_condition()
            self.consume('cbracket')
        elif self.peek('true'):
            self.consume('true')
        elif self.peek('false'):
            self.consume('false')
        else:
            self.parse_expression()
            self.parse_relationaloper()
            self.parse_expression()

    def parse_expression(self):
        self.parse_optionalsign()
        self.parse_term()

        while self.peek('plus') or self.peek('minus'):
            self.parse_addoper()
            self.parse_term()

    def parse_term(self):
        self.parse_factor()

        while self.peek('mul') or self.peek('div'):
            self.parse_muloper()
            self.parse_factor()

    def parse_factor(self):
        if self.peek('oparen'):
            self.consume('oparen')
            self.parse_expression()
            sefl.consume('cparen')
        elif self.peek('id'):
            self.consume('id')
            self.parse_idtail()
        else:
           self.consume('int')

    def parse_idtail(self):
        if self.peek('oparen'):
            self.parse_actualpars()

    def parse_relationaloper(self):
        if self.peek('eq'):
            self.consume('eq')
        elif self.peek('le'):
            self.consume('le')
        elif self.peek('ge'):
            self.consume('ge')
        elif self.peek('lt'):
            self.consume('lt')
        elif self.peek('gt'):
            self.consume('gt')
        else: 
            self.consume('neq')

    def parse_addoper(self):
        if self.peek('plus'):
            self.consume('plus')
        else:
            self.consume('minus')

    def parse_muloper(self):
        if self.peek('mul'):
            self.consume('mul')
        else:
            self.consume('div')

    def parse_optionalsign(self):
        if self.peek('plus') or self.peek('minus'):
            self.parse_addoper()

import argparse
parser = argparse.ArgumentParser()
parser.add_argument('source_file')
args = parser.parse_args()
with open(args.source_file, 'r') as source_file:
    source = source_file.read()
    tokens = Lexer(source).tokenize()
    SyntaxAnal(tokens).check_syntax()
