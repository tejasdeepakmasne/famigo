package hardware

type Bus interface {
	Read(address uint16) uint8
	Write(address uint16, value uint8)
}

type cpuBus struct {
	nes *NES
}

func (c *cpuBus) Read(address uint16) uint8 {
	return c.nes.cpu.WRAM[address]
}

func (c *cpuBus) Write(address uint16, value uint8) {
	c.nes.cpu.WRAM[address] = value
}

