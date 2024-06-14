package cpu

import (
	"github.com/tejasdeepakmasne/famigo/hardware/console"
	"github.com/tejasdeepakmasne/famigo/hardware/memory"
)

const (
	NMI   uint16 = 0xFFFA
	Reset uint16 = 0xFFFC
	IRQ   uint16 = 0xFFFE
)

type opcodeInfo struct {
	mode     AddressingMode
	pcChange uint16
}

type CPU struct {
	PC      uint16                //program counter
	SP      uint8                 //stack pointer used as a offset from 0x1000
	A       uint8                 //accumulator
	X       uint8                 //index x register
	Y       uint8                 //index y register
	P       uint8                 //processor status
	Memory  memory.Memory         //Memory interface
	WRAM    []uint8               //the WRAM
	opcodes [256]func(opcodeInfo) //function table
}

// the flags are given values corresponding to their bit position
// to set just do an or with the flag - 10111111 | V = 11111111
// to unset just do a and with the not of the flag 01000000 & (~V) = 00000000
// to get bit value for example at bit 6 -> 01000000 & V
// if the result is greater than 0 -> value is 1 else 0
type Flag uint8

const (
	C Flag = 1   //carry flag
	Z Flag = 2   //zero flag
	I Flag = 4   //Interrupt disable
	D Flag = 8   //Decimal mode - removed in 2A03 to prevent chip copyrights
	B Flag = 16  //Break command
	U Flag = 32  //Unused register
	V Flag = 64  //Overflow flag
	N Flag = 128 //Negative flag
)

func (c *CPU) GetFlagValue(f Flag) uint8 {
	if c.P&uint8(f) > 0 {
		return 1
	}
	return 0
}

func (c *CPU) SetFlags(flags ...Flag) {
	for f := range flags {
		c.P |= uint8(f)
	}
}

func (c *CPU) ResetFlags(flags ...Flag) {
	for f := range flags {
		c.P &= uint8(^f)
	}
}

func (c *CPU) UpdateZandN(val uint) {
	if val == 0 {
		c.SetFlags(Z)
	}
	if val > 127 {
		c.SetFlags(N)
	}
}

type AddressingMode int

const (
	modeZeroPage AddressingMode = iota
	modeZeroPageX
	modeZeroPageY
	modeAbsolute
	modeAbsoluteX
	modeAbsoluteY
	modeIndirect
	modeImplied
	modeAccumulator
	modeImmediate
	modeRelative
	modeIndirectX
	modeIndirectY
)

func NewCPU(console *console.Console) *CPU {
	return &CPU{
		Memory: memory.N
	}
}

func (c *CPU) GetModeOperand(mode AddressingMode) uint8 {
	switch mode {
	case modeZeroPage: return c.WRAM[]
	case modeZeroPageX: return c.WRAM[(c.PC+uint16(c.X))%256]
	case modeZeroPageY: return c.WRAM[(c.PC+uint16(c.Y))%256]
	case modeAbsolute: return c.WRAM[()]
	}
}
