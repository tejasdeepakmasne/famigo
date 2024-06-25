// collect all components
package hardware

import "fmt"

type NES struct {
	cpu *CPU
	ppu *PPU
	cart *Cartridge
}

func (nes *NES) Run() {
	for {
		// cpuCycles := nes.cpu.Step()
		// ppuCycles := cpuCycles * 3
		// nes.ppu.Step(ppuCycles)

		if nes.ppu.frameComplete {
			nes.ppu.frameComplete = false
			// nes.ppu.Render()
		}
	}
}

func main() {
	ppu := NewPPU()
	cpu := NewCPU()
	nes := &NES {
		cpu: cpu,
		ppu: ppu,
	}

	fmt.Println("NES is running...")
	nes.Run()
}