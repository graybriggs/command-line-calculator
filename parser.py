# !/usr/bin/python

import sys


##

class SyntaxException(Exception):
    def __init__(self, err_str="Syntax Error"):
        self.value = err_str

    def __str__(self):
        return repr(self.value)


class SemanticException(Exception):
    def __init__(self, err_str="Semantic Error"):
        self.value = err_str

    def __str__(self):
        return repr(self.value)


######



#### Lexer ####

def atom(data, index):
    i = index
    count = 0
    while i < len(data):
        if data[i].isalnum() or data[i] == '.':
            count += 1
        else:
            break

        i += 1

    return data[index: index + count]


def is_integer(data):
    index = 0

    while index < len(data):
        if not data[index].isdigit():
            return False
        index += 1

    return True


def is_real(data):
    point = data.find('.')
    if point != -1:
        if data[:point].isdigit() and data[point + 1:].isdigit():
            return True

    return False


def is_identifier(data):
    # leading character of variable/identifier must be alpha
    if not data[0].isalpha():
        return False

    index = 1

    while index < len(data):
        if not data[index].isalpha():
            return False
        index += 1

    return True


def tokenize(exp):
    tokens = []
    index = 0
    exp = exp.replace(" ", "")  # strip all whitespace

    while index < len(exp):

        if exp[index] == '*':
            tokens.append((exp[index], "OP_MULTIPLICATION"))
            index += 1
        elif exp[index] == '/':
            tokens.append((exp[index], "OP_DIVISION"))
            index += 1
        elif exp[index] == '+':
            tokens.append((exp[index], "OP_ADDITION"))
            index += 1
        elif exp[index] == '-':
            tokens.append((exp[index], "OP_MINUS"))
            index += 1
        elif exp[index] == '%':
            tokens.append((exp[index], "OP_MODULUS"))
            index += 1
        elif exp[index] == '^':
            tokens.append((exp[index], "OP_EXPONENT"))
            index += 1
        elif exp[index] == '(':
            tokens.append((exp[index], "L_PARENT"))
            index += 1
        elif exp[index] == ')':
            tokens.append((exp[index], "R_PARENT"))
            index += 1
        elif exp[index] == '=':
            tokens.append((exp[index], "OP_ASSIGNMENT"))
            index += 1
        else:
            res = atom(exp, index)
            # determine atom type - integer, real, etc.
            if len(res) == 0:
                raise SyntaxException(exp[index] + ", illegal syntax")
            elif is_real(res):
                tokens.append((res, "REAL"))
            elif is_integer(res):
                tokens.append((res, "INTEGER"))
            elif is_identifier(res):
                tokens.append((res, "IDENTIFIER"))
            else:
                raise SyntaxException("Unidentified syntax")

            atom_length = len(res)
            if atom_length == 0:
                index += 1
            else:
                index += atom_length

    tokens.append(("EOF", "EOF"))  # end of string marker
    return tokens

##############################
##############################
##############################


state_table = {}

#### Parser ####

tok_num = 0


def next_symbol(tokens):
    global tok_num
    tok_num += 1
    return tokens[tok_num]


def previous_symbol(tokens):
    global tok_num
    cur_tok = tokens[tok_num - 1]
    return cur_tok[1]

def unget_symbol(tokens):
    global tok_num
    tok_num -= 1

def expect_symbol_type(token, exp_sym):
    global tok_num
    cur_tok = token[tok_num]
    return cur_tok[1] == exp_sym


def get_symbol_type(tokens):
    global tok_num
    cur_tok = tokens[tok_num]
    return cur_tok[1]


def get_symbol(tokens):
    global tok_num
    cur_tok = tokens[tok_num]
    return cur_tok[0]


def factor(tokens):

    if expect_symbol_type(tokens, "OP_MINUS"):
        next_symbol(tokens)
        exp = factor(tokens)
        return Negative(exp)
    elif expect_symbol_type(tokens, "INTEGER"):
        val = get_symbol(tokens)
        next_symbol(tokens)
        return Integer(val)
    elif expect_symbol_type(tokens, "REAL"):
        val = get_symbol(tokens)
        next_symbol(tokens)
        return Real(val)
    elif expect_symbol_type(tokens, "IDENTIFIER"):
        ident = None
        try:
            ident = state_table[get_symbol(tokens)]
        except KeyError:
            raise SyntaxException(get_symbol(tokens) + ", unknown Identifier")
        next_symbol(tokens)
        return ident
    elif expect_symbol_type(tokens, "L_PARENT"):
        next_symbol(tokens)
        paren_exp = expression(tokens)
        if not expect_symbol_type(tokens, "R_PARENT"):
            raise SyntaxException("Missing right parenthesis")
        else:
            next_symbol(tokens)
            return paren_exp
    else:
        raise SyntaxException("Expected constant or identifier")


def term(tokens):
    lhs_operand = factor(tokens)
    operator = None

    if not expect_symbol_type(tokens, "EOF"):
        while expect_symbol_type(tokens, "OP_MULTIPLICATION") or expect_symbol_type(tokens,
                                                                                    "OP_DIVISION") or expect_symbol_type(
                tokens, "OP_MODULUS") or expect_symbol_type(tokens, "OP_EXPONENT"):
            if expect_symbol_type(tokens, "OP_MULTIPLICATION"):
                next_symbol(tokens)
                rhs_operand = expression(tokens)
                operator = Multiplication(lhs_operand.result(), rhs_operand.result())
            elif expect_symbol_type(tokens, "OP_DIVISION"):
                next_symbol(tokens)
                rhs_operand = expression(tokens)
                operator = Division(lhs_operand.result(), rhs_operand.result())
            elif expect_symbol_type(tokens, "OP_MODULUS"):
                next_symbol(tokens)
                rhs_operand = expression(tokens)
                operator = Modulus(lhs_operand.result(), rhs_operand.result())
            elif expect_symbol_type(tokens, "OP_EXPONENT"):
                next_symbol(tokens)
                rhs_operand = expression(tokens)
                operator = Exponent(lhs_operand.result(), rhs_operand.result())

        if operator is None:
            return lhs_operand
        else:
            return operator
    else:
        return lhs_operand


def expression(tokens):
    lhs_operand = term(tokens)
    operator = None

    if not expect_symbol_type(tokens, "EOF"):
        while expect_symbol_type(tokens, "OP_ADDITION") or expect_symbol_type(tokens, "OP_MINUS"):
            if expect_symbol_type(tokens, "OP_ADDITION"):
                next_symbol(tokens)
                rhs_operand = expression(tokens)
                operator = Addition(lhs_operand.result(), rhs_operand.result())
            elif expect_symbol_type(tokens, "OP_MINUS"):
                next_symbol(tokens)
                rhs_operand = expression(tokens)
                operator = Subtraction(lhs_operand.result(), rhs_operand.result())

        if operator is None:
            return lhs_operand
        else:
            return operator
    else:
        return lhs_operand


def all_arithmetic_operations(tokens):
    if expect_symbol_type(tokens, "OP_ADDITION") or expect_symbol_type(tokens, "OP_MINUS") or expect_symbol_type(tokens, "OP_MULTIPLICATION") or expect_symbol_type(tokens, "OP_DIVISION") or expect_symbol_type(tokens, "OP_MODULUS") or expect_symbol_type(tokens, "R_PARENT") or expect_symbol_type(tokens, "L_PARENT"):
        return True
    else:
        return False


def all_types(tokens):
    if expect_symbol_type(tokens, "INTEGER") or expect_symbol_type(tokens, "REAL"):
        return True
    else:
        return False


def parse(tokens):
    # accept an assignment...
    if expect_symbol_type(tokens, "IDENTIFIER"):
        ident = get_symbol(tokens)
        next_symbol(tokens)
        if expect_symbol_type(tokens, "OP_ASSIGNMENT"):
            next_symbol(tokens)
            res = expression(tokens)
            state_table[ident] = res
            return res
        elif all_arithmetic_operations(tokens):
            unget_symbol(tokens)
            return expression(tokens)
        elif expect_symbol_type(tokens, "EOF"):
            try:
                var_id = state_table[ident]
                return var_id
            except KeyError:
                raise SyntaxException(ident + ", identifier not found")
    # accept an expression
    elif all_types(tokens):
        next_symbol(tokens)
        if all_arithmetic_operations(tokens) or expect_symbol_type(tokens, "EOF"):
            unget_symbol(tokens)
            return expression(tokens)
        else:
            raise SyntaxException("Unrecognized expression")
    elif expect_symbol_type(tokens, "L_PARENT") or expect_symbol_type(tokens, "R_PARENT") or expect_symbol_type(tokens, "OP_MINUS"):
        return expression(tokens)
    else:
        raise SyntaxException("Unrecognized expression 319")


def interpreter(tokens):
    global tok_num
    tok_num = 0

    return parse(tokens)


#################
## AST Classes ##
#################


class Integer:
    def __init__(self, value):
        self.value = int(value)

    def result(self):
        return self.value


class Real:
    def __init__(self, value):
        self.value = float(value)

    def result(self):
        return self.value


class Negative:
    def __init__(self, value):
        self.value = value

    def __neg__(self):
        self.value = -self.value

    def result(self):
        return -self.value.result()


class Addition:
    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs

    def result(self):
        return self.lhs + self.rhs


class Subtraction:
    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs

    def result(self):
        return self.lhs - self.rhs


class Multiplication:
    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs

    def result(self):
        return self.lhs * self.rhs


class Division:
    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs

    def result(self):
        return self.lhs / self.rhs


class Modulus:
    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs

    def result(self):
        return self.lhs % self.rhs


class Exponent:
    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs

    def result(self):
        return self.lhs ** self.rhs


########

def clear_state():
    global state_table
    state_table.clear()
    print("Cleared memory")

def print_state_table():
    global state_table
    if len(state_table) == 0:
        print("No variables stored")
        return

    for k in state_table:
        print(k + ": " + str(state_table[k].result()))



########

user_input = input("Enter expression: ")

while user_input != "exit":

    if user_input == ".state":
        print_state_table()
    elif user_input == ".clear":
        clear_state()
    else:
        try:
            tok = tokenize(user_input)
            print(tok)
            res = interpreter(tok)
            print(res.result())
        except SyntaxException as err:
            print("Syntax Error: " + str(err))
        except SemanticException as err:
            print("Semantic Error: " + str(err))

    user_input = input("Enter expression: ")



