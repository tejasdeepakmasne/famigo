package memory

import (
	"log"

	"github.com/tejasdeepakmasne/famigo/hardware/console"
)

type Memory interface {
	Read(address uint16) uint8
	Write(address uint16, value uint8)
}

type cpuMem struct {
	console *console.Console
}

func NewCPUMemory(console *console.Console) Memory {
	return &cpuMem{console: console}
}

func (c *cpuMem) Read(address uint16) uint8 {
	switch {
	case address < 0x2000:
		return c.console.CPU.WRAM[address%0x0800]
	case address < 0x4000:
		return c.console.PPU.ReadRegister(0x2000 + (address % 8))
	case address == 4014:
		return c.console.PPU.ReadRegister(address)
	default:
		log.Fatalf("unhandled memory read at location %x", address)
	}
	return 0
}

func (c *cpuMem) Write(address uint16, value uint8) {
	switch {
	case address < 0x2000:
		c.console.CPU.WRAM[address%0x0800] = value
	}
}
