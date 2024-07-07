package hardware

type Bus interface {
	Read(address uint16) uint8
	Write(address uint16, value uint8)
}

type cpuBus struct {
	nes *NES
}

func (c *cpuBus) Read(address uint16) uint8 {
	if address < 0x2000 {
		return c.nes.cpu.WRAM[address&0x07ff]
	}
	return c.nes.cpu.WRAM[address&0x07ff]
}

func (c *cpuBus) Write(address uint16, value uint8) {
	if address < 0x2000 {
		c.nes.cpu.WRAM[address] = value
	}
}
