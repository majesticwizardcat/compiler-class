from typing import Iterator

from collections import namedtuple, defaultdict
from itertools import tee
import re

INVALID_TOKENS = [
    ('ccomment', re.compile(r'\A\*\/')), # should normally be consumed via IGNORED_TOKENS's comment, otherwise there was no comment open
    ('unshut_comment', re.compile(r'\A\/\*[^*/]*\Z')),
]

IGNORED_TOKENS = [
    ('comment', re.compile(r'\A\/\*[^*/]*\*\/')),
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

    ('id', re.compile(r'\A[a-zA-Z][a-zA-Z0-9]*')),
    ('int', re.compile(r'\A\b\d+\b')),
    ('plus', re.compile(r'\A\+')),
    ('minus', re.compile(r'\A-')),
    ('div', re.compile(r'\A/')),
    ('mul', re.compile(r'\A\*')),

    ('oparen', re.compile(r'\A\(')),
    ('cparen', re.compile(r'\A\)')),
    ('obracket', re.compile(r'\A\[')),
    ('cbracket', re.compile(r'\A\]')),

    ('ge', re.compile(r'\A>=')),
    ('le', re.compile(r'\A<=')),
    ('gt', re.compile(r'\A>')),
    ('lt', re.compile(r'\A<')),
    ('eq', re.compile(r'\A=')),
    ('neq', re.compile(r'\A<>')),
]

EXTRA_VALIDATORS = defaultdict(lambda: lambda x: True)
EXTRA_VALIDATORS['int'] = lambda x: -32767 <= int(x) <= 32767

Token = namedtuple('Token', ['type', 'value'])

class Lexer:
    def advance_from_match(self, match):
        endpos = match.span()[1]
        self.source = self.source[endpos:].lstrip()

    def __init__(self, source):
        self.tokens = []
        self.source = source

        while len(self.source) > 0:
            #print('source to lex: "%s"' % self.source)
            for name, regex in INVALID_TOKENS:
                if regex.search(self.source):
                    raise ValueError(name)

            for name, regex in IGNORED_TOKENS:
                match = regex.search(self.source)
                if match:
                    print('Ignored token: ', name)
                    self.advance_from_match(match)
                    break
            else:
                found_token = False

                for name, regex in VALID_TOKENS:
                    match = regex.search(self.source)
                    if match and EXTRA_VALIDATORS[name](match.group()):
                        found_token = True
                        print('Matched token: ', name)
                        #print('Match: ', match.span())
                        self.advance_from_match(match)
                        break

                if not found_token:
                    raise ValueError('Couldn\'t tokenize: `%s`' % self.source)
            #print('source after lex: "%s"' % self.source)

source = """program example3
   declare a,b,c,d,e,x,y,px,py,temp enddeclare
   repeat
      if not [a<c and b<d] then
        exit
      endif;
      if a=e then
   c:=c+e;
      else
   repeat // est
            if not [a<=d] then                    
                    exit /* lol */
    /* agapo
            hh    ti mana
    su */
      endif;
            a:=a+b;
         endrepeat
      endif
   endrepeat;
   temp:=px;
   x:=1;
   y:=2847389247982378972;
endprogram"""
lex = Lexer(source)
