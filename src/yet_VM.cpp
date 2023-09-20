#include<iostream>
#include<iomanip>
#include<fstream>
#include<cstring>
#include<math.h>
#include<unistd.h>
#include"opecodes.h"
using namespace std;

#define STACK_SIZE 2621456
#define GLOBAL_SIZE 1000000
#define HEEP_SIZE 10000000

void yetiexception(int code_pointer, uint8_t opecode){
    cerr << "execution error at " << code_pointer <<  " opecode : " << (int)opecode << dec << endl;
    exit(1);
}
void *memrev(void *buf, size_t count){
	uint8_t *buf_byte = (uint8_t *)buf;
	uint8_t *r;
	for(r = buf_byte + count - 1; buf_byte < r; buf_byte++, r--){
		*buf_byte ^= *r;
		*r ^= *buf_byte;
		*buf_byte ^= *r;
	}
	return buf;
}

class Runner{
private:
    uint8_t *code;
    uint64_t pc;
    uint8_t stack[STACK_SIZE] = {};
	uint8_t *global;
	uint8_t *heap;
	static const uint8_t RSP = 31, RBP = 30, RHP = 29, RET = 28;
    union REG{double d; int64_t l; int32_t i; uint8_t b[8];} registers[32] = {};
public:
    Runner(uint8_t *c, int globalsize, int heapsize, int entry_point){
        code = c;
		global = new uint8_t[globalsize];
		heap = new uint8_t[heapsize];
        pc = entry_point;
        (registers + RSP)->l = sizeof(stack) / sizeof(stack[0]);
    }
    void run(){
		while(true){
			//cout << "pc:" << pc << endl;
			//printf("printf pc:%ld\n", pc);
	        switch(*(code + pc)){
    	        case (uint8_t)Opcode::MOVN:
        	        memcpy(registers + *(code + pc + 1), code + pc + 2, 8);
            	    pc += 10;
                	break;
	            case (uint8_t)Opcode::MOVS:
					memcpy(registers + *(code + pc + 1), stack + (registers + *(code + pc + 2))->l - 7, 8);
					pc += 3;
    	            break;
				case (uint8_t)Opcode::MOVH:
					memcpy(registers + *(code + pc + 1), heap + (registers + *(code + pc + 2))->l, 8);
					pc += 3;
					break;
				case (uint8_t)Opcode::MOVG:
					memcpy(registers + *(code + pc + 1), global + (registers + *(code + pc + 2))->l, 8);
					pc += 3;
					break;
        	    case (uint8_t)Opcode::MOVR:
            	    memcpy(registers + *(code + pc + 1), registers + *(code + pc + 2), 8);
                	pc += 3;
            	    break;
				case (uint8_t)Opcode::MOVSR:
					memcpy(stack + (registers + *(code + pc + 1))->l - 7, registers + *(code + pc + 2), 8);
					pc += 3;
					break;
				case (uint8_t)Opcode::MOVHR:
					memcpy(heap + (registers + *(code + pc + 1))->l, registers + *(code + pc + 2), 8);
					pc += 3;
					break;
				case (uint8_t)Opcode::MOVGR:
					memcpy(global + (registers + *(code + pc + 1))->l, registers + *(code + pc + 2), 8);
					pc += 3;
					break;
        	    case (uint8_t)Opcode::ADD:
            	    (registers + *(code + pc + 1))->d += (registers + *(code + pc + 2))->d;
                	pc += 3;
	                break;
    	        case (uint8_t)Opcode::SUB:
        	        (registers + *(code + pc + 1))->d -= (registers + *(code + pc + 2))->d;
            	    pc += 3;
                	break;
	            case (uint8_t)Opcode::MUL:
    	            (registers + *(code + pc + 1))->d *= (registers + *(code + pc + 2))->d;
        	        pc += 3;
            	    break;
	            case (uint8_t)Opcode::DIV:
    	            (registers + *(code + pc + 1))->d /= (registers + *(code + pc + 2))->d;
        	        pc += 3;
            	    break;
	            case (uint8_t)Opcode::MOD:
					(registers + *(code + pc + 1))->d = fmod((registers + *(code + pc + 1))->d, (registers + *(code + pc + 2))->d);
    	            pc += 3;
        	        break;
				case (uint8_t)Opcode::EXP:
					(registers + *(code + pc + 1))->d = pow((registers + *(code + pc + 1))->d, (registers + *(code + pc + 2))->d);
					pc += 3;
					break;
            	case (uint8_t)Opcode::EQU:
	                (registers + *(code + pc + 1))->d = (registers + *(code + pc + 1))->d == (registers + *(code + pc + 2))->d;
    	            pc += 3;
        	        break;
            	case (uint8_t)Opcode::NEQU:
	                (registers + *(code + pc + 1))->d = (registers + *(code + pc + 1))->d != (registers + *(code + pc + 2))->d;
    	            pc += 3;
        	        break;
            	case (uint8_t)Opcode::MORE:
                	(registers + *(code + pc + 1))->d = (registers + *(code + pc + 1))->d > (registers + *(code + pc + 2))->d;
	                pc += 3;
    	            break;
        	    case (uint8_t)Opcode::LESS:
            	    (registers + *(code + pc + 1))->d = (registers + *(code + pc + 1))->d < (registers + *(code + pc + 2))->d;
                	pc += 3;
	                break;
				case (uint8_t)Opcode::JUMPN:
					memcpy(&pc, code + pc + 1, 8);
					break;
				case (uint8_t)Opcode::JUMPR:
					pc = (registers + *(code + pc + 1))->l;
					break;
				case (uint8_t)Opcode::IFZERO:
					if((registers + *(code + pc + 1))->d == 0)
						memcpy(&pc, code + pc + 2, 8);
					else
						pc += 10;
					break;
				case (uint8_t)Opcode::PUSHN:
					(registers + RSP)->l -= 8;
					memcpy(stack + (registers + RSP)->l - 7, code + pc + 1, 8);
					pc += 9;
					break;
				case (uint8_t)Opcode::PUSHR:
					(registers + RSP)->l -= 8;
					memcpy(stack + (registers + RSP)->l - 7, registers + *(code + pc + 1), 8);
					pc += 2;
					break;
				case (uint8_t)Opcode::POP:
					memcpy(registers + *(code + pc + 1), stack + (registers + RSP)->l - 7, 8);
					(registers + RSP)->l += 8;
					pc += 2;
					break;
				case (uint8_t)Opcode::OUT:
					cout << "> " << (registers + *(code + pc + 1))->d << endl;
					pc += 2;
					break;
				case (uint8_t)Opcode::IN:
					cout << "< "; cin >> (registers + *(code + pc + 1))->d;
					pc += 2;
					break;
				case (uint8_t)Opcode::COMV:
					(registers + *(code + pc + 1))->l = (long)((registers + *(code + pc + 1))->d * 8);
					pc += 2;
					break;
				case (uint8_t)Opcode::HLT:
					return;
        	    default:
             	    yetiexception(pc, code[pc]);
					break;
        	}
    	}
    }
};

int main(int argc, char *argv[]){
	int opt;
	while((opt = getopt(argc, argv, "h")) != -1){
		switch(opt){
			case 'h':
				cout << "Usage: " << "[-h]" << endl;
				return 0;
		}
	}
	string file_name = argc <= 1? "error" : argv[argc - 1];
	ifstream file(file_name, ios::in|ios::binary);
	const int size = 655365;
	uint8_t *code = new uint8_t[size];
	file.read((char *)code, size);
	file.close();
	Runner runner = Runner(code, GLOBAL_SIZE, HEEP_SIZE, 0);
	runner.run();
	return 0;
}
