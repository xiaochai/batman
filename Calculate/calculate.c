#include<stdio.h>
#include<stdlib.h>
#include<ctype.h>

#define SUCC 0
#define FAIL 1
#define CAP 100

#define LBRA 10
#define RBRA 20
#define PLUS 11
#define MINUS 21
#define MUL 33
#define DIV 43
#define POWER 55

int getops(char c){
	switch(c){
		case '+':return PLUS;
		case '-':return MINUS;
		case '*':return MUL;
		case '/':return DIV;
		case '^':return POWER;
		case '(':return LBRA;
		case ')':return RBRA;
		default:return -1;
	}
}
int opscmp(char c1, char c2){
	return (c1%10) - (c2%10);
}

typedef struct Stack {
	int top;
	int content[CAP];
} Stack;

int emptyStack(Stack* s){
	return s->top  == -1;
}

int fullStack(Stack* s){
	return s->top == CAP - 1;
}

Stack* initStack(){
	Stack* s = malloc(sizeof(Stack));
	s->top = -1;
	return s;
}

int pushStack(Stack* s, int i){
	if(fullStack(s)){
		return FAIL;
	}else{
		s->top ++;
		s->content[s->top] = i;
		return SUCC;
	}
}

int popStack(Stack* s, int *i){
	if(emptyStack(s)){
		return FAIL;
	}else if(i == NULL){
		s->top -- ;
		return SUCC;
	}else{
		*i = s->content[s->top--];
		return SUCC;
	}
}


int topStack(Stack* s, int * i){
	if(emptyStack(s)){
		return FAIL;
	}else{
		*i = s->content[s->top];
		return SUCC;
	}
}

void printStack(Stack* s){
	printf("Stack top:%d, contents:", s->top);
	for(int i = 0; i <= s->top; i++){
		printf("%d\t", s->content[i]);
	}
	printf("\n");
}

void cal(Stack *ops, Stack *num){
	int n1, n2;
	popStack(num, &n2);
	popStack(num, &n1);
	int o;
	popStack(ops, &o);
	int sum = 1;
	switch(o){
		case PLUS: pushStack(num, n1+n2);break;
		case MINUS: pushStack(num, n1-n2);break;
		case MUL: pushStack(num, n1*n2);break;
		case DIV: pushStack(num, n1/n2);break;
		case POWER:
				  for(int i = 0; i <  n2; i++){
					  sum *= n1;
				  }
				  pushStack(num, sum);break;
		defalut: printf("OPSERROR");
	}
}

#define BUFF_SIZE 200
#define INT_BUFF_SIZE 6
int main(){
	char buff[BUFF_SIZE];
	fgets(buff, sizeof(buff), stdin);

	Stack *num = initStack();
	Stack *ops = initStack();

	char* c = buff;
	char intbuff[INT_BUFF_SIZE];
	int intbuffoffset = 0;
	do{
		if(isdigit(*c)){
			intbuff[intbuffoffset++] = *c;
		}else{
			if(intbuffoffset != 0){
				intbuff[intbuffoffset] = '\0';
				int i = atoi(intbuff);
				pushStack(num, i);
				intbuffoffset = 0;
			}
			if(*c == '\n' || *c== '\0'){
				while(!emptyStack(ops)){
					cal(ops, num);
				}
				break;
			}
			if(isspace(*c)){
				continue;
			}
			int o = getops(*c);
			if(o == -1){
				printf("Unknown operation:%c\n", *c);
				return -1;
			}
			while(1){
				int top;
				topStack(ops, &top);
				if(o == RBRA){
					if(emptyStack(ops)){
						printf("error input:%s\n", buff);
						return -1;
					}else{
						while(top != LBRA){
							cal(ops, num);
							topStack(ops, &top);
						}
						popStack(ops, NULL);
						break;
					}
				}
				if(emptyStack(ops) || opscmp(o, top) > 0 || o == LBRA){
					pushStack(ops, o);
					break;
				}else{
					cal(ops, num);
				}
			}
		}
		c++;
	}while(1);
	printStack(num);
	printStack(ops);
	
	return 0;
}




/*

int main(){
	Stack* s = initStack();
	pushStack(s, 100);
	pushStack(s, 90);
	printStack(s);
	int i;
	topStack(s, &i);
	printf("pop:%d\n", i);
	popStack(s, &i);
	popStack(s, &i);
	popStack(s, &i);
	popStack(s, &i);
	printStack(s);
	printf("pop:%d\n", i);
	return 0;
}
*/
