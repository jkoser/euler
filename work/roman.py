tokens = (
    'M', 'D', 'C', 'L', 'X', 'V', 'I'
    )

t_M = r'M'
t_D = r'D'
t_C = r'C'
t_L = r'L'
t_X = r'X'
t_V = r'V'
t_I = r'I'

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

import ply.lex as lex
lex.lex()

def p_roman_ms(t):
    'roman : ms'
    t[0] = t[1]

def p_ms_m(t):
    'ms : M ms'
    t[0] = 1000 + t[2]

def p_ms_cm(t):
    'ms : C M cs'
    t[0] = 900 + t[3]

def p_ms_ds(t):
    'ms : ds'
    t[0] = t[1]

def p_ds_d(t):
    'ds : D ds'
    t[0] = 500 + t[2]

def p_ds_cd(t):
    'ds : C D cs'
    t[0] = 400 + t[3]

def p_ds_cs(t):
    'ds : cs'
    t[0] = t[1]

def p_cs_c(t):
    'cs : C cs'
    t[0] = 100 + t[2]

def p_cs_xc(t):
    'cs : X C ls'
    t[0] = 90 + t[3]

def p_cs_ls(t):
    'cs : ls'
    t[0] = t[1]

def p_ls_l(t):
    'ls : L ls'
    t[0] = 50 + t[2]

def p_ls_xl(t):
    'ls : X L xs'
    t[0] = 40 + t[3]

def p_ls_xs(t):
    'ls : xs'
    t[0] = t[1]

def p_xs_x(t):
    'xs : X xs'
    t[0] = 10 + t[2]

def p_xs_ix(t):
    'xs : I X'
    t[0] = 9

def p_xs_vs(t):
    'xs : vs'
    t[0] = t[1]

def p_vs_v(t):
    'vs : V vs'
    t[0] = 5 + t[2]

def p_vs_iv(t):
    'vs : I V'
    t[0] = 4

def p_vs_is(t):
    'vs : is'
    t[0] = t[1]

def p_is_i(t):
    'is : I is'
    t[0] = 1 + t[2]

def p_is_empty(t):
    'is :'
    t[0] = 0

def p_error(t):
    print("Syntax error at '%s'" % t.value)

import ply.yacc as yacc
yacc.yacc()

def parse(s):
    return yacc.parse(s)

denom = [
    ('M', 1000),
    ('CM', 900),
    ('D', 500),
    ('CD', 400),
    ('C', 100),
    ('XC', 90),
    ('L', 50),
    ('XL', 40),
    ('X', 10),
    ('IX', 9),
    ('V', 5),
    ('IV', 4),
    ('I', 1)
]

def minimal(n):
    r = ''
    while n > 0:
        for d in denom:
            if n >= d[1]:
                r += d[0]
                n -= d[1]
                break
    return r

if __name__ == "__main__":
    print(yacc.parse('XLII'))
    print(minimal(42))
    print(yacc.parse('CCCCLXIV'))
    print(minimal(464))
    print(yacc.parse('XCIX'))
    print(minimal(99))
    print(yacc.parse('XCXL'))
    print(minimal(130))
