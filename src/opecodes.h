enum class Opcode{
    MOVN = 1,
    MOVS,	//reg1 <- stack(reg2)
	MOVH,	//reg1 <- heap(reg2)
	MOVG,	//reg1 <- memory(reg2)
    MOVR,
	MOVSR,	//stack(reg1) <- reg2
	MOVHR,	//heap(reg1) <- reg2
	MOVGR,	//memory(reg1) <- reg2
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    EXP,
    EQU,
    NEQU,
    MORE,
    LESS,
	JUMPN,
	JUMPR,
	IFZERO,
	PUSHN,
	PUSHR,
	POP,
	OUT,
	IN,
    COMV,
    HLT
};
