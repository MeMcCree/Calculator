#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <setjmp.h>
#include "vector/vector.h"

typedef enum {
	E_NONE = 0,
	E_MISPAREN,
	E_UNKNWNFUNC,
	E_UNKNWNOPER,
	E_NEARGS,
	E_NODATONSTCK,
	E_DIVBYZERO,
	E_NEGSQRT,
	E_SIZE
} err_t;

jmp_buf err_buff;
err_t err_code;
char err_addinfo[512];

typedef enum {
	F_SIN = 0,
	F_COS,
	F_TAN,
	F_ASIN,
	F_ACOS,
	F_ATAN,
	F_FACT,
	F_ABS,
	F_LOG,
	F_LOG2,
	F_CEIL,
	F_TRUNC,
	F_ROUND,
	F_SQRT,
	F_RAD,
	F_DEG,
	F_SIZE
} func_t;

func_t lookup_func(char* str) {
	if (strcmp(str, "sin") == 0) {
		return F_SIN;
	} else if (strcmp(str, "cos") == 0) {
		return F_COS;
	} else if (strcmp(str, "tan") == 0) {
		return F_TAN;
	} else if (strcmp(str, "asin") == 0) {
		return F_ASIN;
	} else if (strcmp(str, "acos") == 0) {
		return F_ACOS;
	} else if (strcmp(str, "atan") == 0) {
		return F_ATAN;
	} else if (strcmp(str, "fact") == 0) {
		return F_FACT;
	} else if (strcmp(str, "abs") == 0) {
		return F_ABS;
	} else if (strcmp(str, "log") == 0) {
		return F_LOG;
	} else if (strcmp(str, "log2") == 0) {
		return F_LOG2;
	} else if (strcmp(str, "ceil") == 0) {
		return F_CEIL;
	} else if (strcmp(str, "trunc") == 0) {
		return F_TRUNC;
	} else if (strcmp(str, "round") == 0) {
		return F_ROUND;
	} else if (strcmp(str, "sqrt") == 0) {
		return F_SQRT;
	} else if (strcmp(str, "rad") == 0) {
		return F_RAD;
	} else if (strcmp(str, "deg") == 0) {
		return F_DEG;
	}

	return F_SIZE;
}

typedef enum {
	O_PLUS = 0,
	O_MIN,
	O_MUL,
	O_DIV,
	O_POW,
	O_LPAR,
	O_RPAR,
	O_BAND,
	O_BOR,
	O_MOD,
	O_SIZE
} oper_t;

typedef enum {
	L_ASSOCIATIVE = 0,
	R_ASSOCIATIVE
} as_t;

typedef struct {
	enum {K_NUM = 0, K_FUNC, K_OPER} kind;
	union {
		func_t fn_type;
		oper_t op_type;
		double data;
	};
} op_t;

int get_precedence(oper_t op) {
	switch (op) {
		case O_PLUS: 	return 1;	break;
		case O_MIN: 	return 1;	break;
		case O_MUL: 	return 4;	break;
		case O_DIV: 	return 4;	break;
		case O_POW: 	return 5;	break;
		case O_BAND:  return 2; break;
		case O_BOR:   return 3; break;
		case O_MOD: 	return 4;	break;
	}

	return 0;
}

as_t get_associativity(oper_t op) {
	switch (op) {
		case O_PLUS: 	return L_ASSOCIATIVE;	break;
		case O_MIN: 	return L_ASSOCIATIVE;	break;
		case O_MUL: 	return L_ASSOCIATIVE;	break;
		case O_DIV: 	return L_ASSOCIATIVE;	break;
		case O_POW: 	return R_ASSOCIATIVE;	break;
		case O_BAND: 	return L_ASSOCIATIVE;	break;
		case O_BOR: 	return L_ASSOCIATIVE;	break;
		case O_MOD: 	return L_ASSOCIATIVE;	break;
	}
}

DECLARE_VECTOR(op_t)

Vec_op_t parse_string(char* expr) {
	Vec_op_t vec;
	VecInit_op_t(&vec);

	int ln = strlen(expr);

	for (int i = 0; i < ln; i++) {
		char c = expr[i];
		if (c == ' ' || c == '\t' || c == '\n') continue;

		if (c >= '0' && c <= '9' || c == '.') {
			double num = 0, pow = 1.0;
			int floating_point = 0;
			while ((c = expr[i]) && c >= '0' && c <= '9' || c == '.') {
				if (c == '.') {
					floating_point = 1;
					i++;
					continue;
				}

				if (!floating_point) {
					num = num * 10 + (c & 15);
				} else {
					pow /= 10.0;
					num = num + ((c & 15) * pow); 
				}

				i++;
			} i--;

			vec.Push(&vec, (op_t){.kind = K_NUM, .data = num});
		} else if (c >= 'a' && c <= 'z') {
			char buff[256];
			int sz = 0;
			
			while ((c = expr[i]) && ((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9'))) {
				buff[sz++] = c;
				buff[sz] = '\0'; 

				i++;
			} i--;

			func_t f = lookup_func(buff);

			if (f == F_SIZE) {
				strcpy(err_addinfo, buff);
				longjmp(err_buff, E_UNKNWNFUNC);
			} else {
				vec.Push(&vec, (op_t){.kind = K_FUNC, .fn_type = f});
			}
		} else {
			switch (c) {
				case '+': vec.Push(&vec, (op_t){.kind = K_OPER, .op_type = O_PLUS});  break;
				case '-': vec.Push(&vec, (op_t){.kind = K_OPER, .op_type = O_MIN}); 	break;
				case '*': vec.Push(&vec, (op_t){.kind = K_OPER, .op_type = O_MUL}); 	break;
				case '/': vec.Push(&vec, (op_t){.kind = K_OPER, .op_type = O_DIV}); 	break;
				case '^': vec.Push(&vec, (op_t){.kind = K_OPER, .op_type = O_POW}); 	break;
				case '(': vec.Push(&vec, (op_t){.kind = K_OPER, .op_type = O_LPAR}); 	break;
				case ')': vec.Push(&vec, (op_t){.kind = K_OPER, .op_type = O_RPAR}); 	break;
				case '&': vec.Push(&vec, (op_t){.kind = K_OPER, .op_type = O_BAND}); 	break;
				case '|': vec.Push(&vec, (op_t){.kind = K_OPER, .op_type = O_BOR}); 	break;
				case '%': vec.Push(&vec, (op_t){.kind = K_OPER, .op_type = O_MOD}); 	break;
				default: err_addinfo[0] = c; err_addinfo[1] = '\0'; longjmp(err_buff, E_UNKNWNOPER); break;
			}
		}
	}

	return vec;
}

Vec_op_t shunting_yard(char* expr) {
	Vec_op_t pexpr = parse_string(expr);

	Vec_op_t op_stck;
	Vec_op_t out;
	VecInit_op_t(&op_stck);
	VecInit_op_t(&out);

	op_t op;
	for (int i = 0; i < pexpr.size; i++) {
		op = pexpr.Get(&pexpr, i);

		if (op.kind == K_NUM) {
			out.Push(&out, op);
		} else if (op.kind == K_FUNC) {
			op_stck.Push(&op_stck, op);
		} else if (op.kind == K_OPER && op.op_type != O_LPAR && op.op_type != O_RPAR) {
			int precedence = get_precedence(op.op_type);
			op_t tmp;
			while (op_stck.size > 0) {
				tmp = op_stck.Get(&op_stck, op_stck.size - 1);
				if ((tmp.kind == K_OPER && tmp.op_type != O_LPAR)
				 && (get_precedence(tmp.op_type) > precedence
				 ||  get_precedence(tmp.op_type) == precedence && get_associativity(op.op_type) == L_ASSOCIATIVE)) {
					op_stck.size--;
					out.Push(&out, tmp);
				} else {
					break;
				}
			}
			op_stck.Push(&op_stck, op);
		} else if (op.op_type == O_LPAR) {
			op_stck.Push(&op_stck, op);
		} else if (op.op_type == O_RPAR) {
			op_t tmp;
			while (op_stck.size > 0) {
				tmp = op_stck.Get(&op_stck, op_stck.size - 1);

				if (tmp.op_type != O_LPAR) {
					op_stck.size--;
					out.Push(&out, tmp);
				} else {
					break;
				}
			}
			if (op_stck.size < 1) longjmp(err_buff, E_MISPAREN);
			if (op_stck.Get(&op_stck, op_stck.size - 1).op_type != O_LPAR) longjmp(err_buff, E_MISPAREN);
			op_stck.size--;

			if (op_stck.size > 0 && (tmp = op_stck.Get(&op_stck, op_stck.size - 1)).kind == K_FUNC) {
				out.Push(&out, tmp);
				op_stck.size--;
			}
		}
	}

	op_t tmp;
	while (op_stck.size > 0) {
		tmp = op_stck.Get(&op_stck, op_stck.size - 1);
		if (tmp.op_type == O_LPAR || tmp.op_type == O_RPAR) longjmp(err_buff, E_MISPAREN);
		op_stck.size--;
		out.Push(&out, tmp);
	}

	VecFree_op_t(&op_stck);
	VecFree_op_t(&pexpr);

	return out;
}

DECLARE_VECTOR(double)

double fact(int a) {
	if (a < 2) return 1;
	return a * fact(a - 1);
}

double parse_rpnotation(char inp[]) {
	Vec_op_t vec = shunting_yard(inp);

	Vec_double num_stck;
	VecInit_double(&num_stck);

	for (int i = 0; i < vec.size; i++) {
		op_t op = vec.Get(&vec, i);
		switch (op.kind) {
			case K_NUM: num_stck.Push(&num_stck, op.data); break;
			case K_OPER:
			{
				if (num_stck.size < 2) longjmp(err_buff, E_NEARGS);
				double a = num_stck.Get(&num_stck, num_stck.size - 2), b = num_stck.Get(&num_stck, num_stck.size - 1);
				num_stck.size -= 2;
				switch (op.op_type) {
					case O_PLUS: 
						num_stck.Push(&num_stck, a + b);
					break;
					case O_MIN:
						num_stck.Push(&num_stck, a - b);
					break;
					case O_MUL:
						num_stck.Push(&num_stck, a * b);
					break;
					case O_DIV:
						if (b == 0.0) longjmp(err_buff, E_DIVBYZERO);
						num_stck.Push(&num_stck, a / b);
					break;
					case O_POW:
						num_stck.Push(&num_stck, pow(a, b));
					break;
					case O_BAND:
						num_stck.Push(&num_stck, (uint64_t)a & (uint64_t)b);
					break;
					case O_BOR:
						num_stck.Push(&num_stck, (uint64_t)a | (uint64_t)b);
					break;
					case O_MOD:
						if (b == 0.0) longjmp(err_buff, E_DIVBYZERO);
						num_stck.Push(&num_stck, (uint64_t)a % (uint64_t)b);
					break;
				}
			break;
			}
			case K_FUNC:
			{
				if (num_stck.size < 1) longjmp(err_buff, E_NEARGS);
				double b = num_stck.Get(&num_stck, num_stck.size - 1);
				num_stck.size -= 1;
				switch (op.fn_type) {
					case F_SIN:
						num_stck.Push(&num_stck, sin(b));
					break;
					case F_COS:
						num_stck.Push(&num_stck, cos(b));
					break;
					case F_TAN:
						num_stck.Push(&num_stck, tan(b));
					break;
					case F_ASIN:
						num_stck.Push(&num_stck, asin(b));
					break;
					case F_ACOS:
						num_stck.Push(&num_stck, acos(b));
					break;
					case F_ATAN:
						num_stck.Push(&num_stck, atan(b));
					break;
					case F_FACT:
						num_stck.Push(&num_stck, fact(b));
					break;
					case F_ABS:
						num_stck.Push(&num_stck, fabs(b));
					break;
					case F_LOG:
						num_stck.Push(&num_stck, log(b));
					break;
					case F_LOG2:
						num_stck.Push(&num_stck, log2(b));
					break;
					case F_CEIL:
						num_stck.Push(&num_stck, ceil(b));
					break;
					case F_TRUNC:
						num_stck.Push(&num_stck, trunc(b));
					break;
					case F_ROUND:
						num_stck.Push(&num_stck, round(b));
					break;
					case F_SQRT:
						if (b < 0.0) longjmp(err_buff, E_NEGSQRT);
						num_stck.Push(&num_stck, sqrt(b));
					break;
					case F_RAD:
						num_stck.Push(&num_stck, b * M_PI / 180.0);
					break;
					case F_DEG:
						num_stck.Push(&num_stck, b / M_PI * 180.0);
					break;
				}
			break;
			}
		}
	}

	if (num_stck.size < 1) longjmp(err_buff, E_NODATONSTCK);
	double res = num_stck.Get(&num_stck, num_stck.size - 1);

	VecFree_op_t(&vec);
	VecFree_double(&num_stck);

	return res;
}

int main() {
	char inp[512];

	while ((fgets(inp, 512, stdin)), strcmp(inp, "quit") != 0) {
		if (strcmp(inp, "help\n") == 0) {
			printf("[HELP]:\n");
			printf("\tPush number:\n");
			printf("\t\tExample:\n");
			printf("\t\t\t10 or 10.0:\n");
			printf("\tOperators:\n");
			printf("\t\t+: plus operator\n");
			printf("\t\t-: minus operator\n");
			printf("\t\t*: multiplication operator\n");
			printf("\t\t/: division operator\n");
			printf("\t\t^: raise to power operator\n");
			printf("\tFunctions:\n");
			printf("\t\tFunction usage:\n");
			printf("\t\t\tsin(3.14 / 6.0)\n");
			printf("\t\tsin: sine\n");
			printf("\t\tcos: cosine\n");
			printf("\t\ttan: tangent\n");
			printf("\t\tasin: arcsine\n");
			printf("\t\tacos: arccosine\n");
			printf("\t\tatan: arctangent\n");
			printf("\t\tfact: factorial\n");
			printf("\t\tabs: absolute value\n");
			printf("\t\tlog: natural logarithm\n");
			printf("\t\tlog2: binary logarithm\n");
			printf("\t\tceil: round up number\n");
			printf("\t\ttrunc: round down number\n");
			printf("\t\tround: round number\n");
			printf("\t\tsqrt: squre root\n");
			printf("\t\trad: degrees to radians\n");
			printf("\t\tdeg: radians to degrees\n");
			continue;
		}

		switch (setjmp(err_buff)) {
			case E_NONE:
				double res = parse_rpnotation(inp);
				printf("%f\n", res);
			break;
			case E_MISPAREN:
				printf("[ERROR]: Mismatched parenthesis\n");
			break;
			case E_NEARGS:
				printf("[ERROR]: Not enough arguments\n");
			break;
			case E_UNKNWNFUNC:
				printf("[ERROR]: Unknown function \"%s\"\n", err_addinfo);
			break;
			case E_UNKNWNOPER:
				printf("[ERROR]: Unknown operator \'%s\'\n", err_addinfo);
			break;
			case E_NODATONSTCK:
				printf("[ERROR]: No data on stack\n");
			break;
			case E_DIVBYZERO:
				printf("[ERROR]: Division by zero\n");
			break;
			case E_NEGSQRT:
				printf("[ERROR]: Negative square root\n");
			break;
		}
	}


	return 0;
}

